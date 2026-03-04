const DATA_DIR = "./data";
const EXCLUDE_STATEFP = new Set(["02", "15", "60", "66", "69", "72", "78"]);

const metricLabels = {
  county_tariff_exposure_mean: "Exposure (level)",
  county_tariff_exposure_per_worker: "Exposure (per worker)",
  tariffed_emp_share: "Tariffed employment share",
  political_salience_weighted_exposure: "Political salience index",
  gop_share_20: "GOP share (2020)",
  gop_share_24: "GOP share (2024)",
  swingness_20: "Competitiveness (2020)"
};

const scatterLabels = {
  gop_share_20: "GOP share (2020)",
  gop_share_24: "GOP share (2024)",
  swingness_20: "Competitiveness (2020)",
  tariffed_emp_share: "Tariffed employment share"
};

const yearSelect = document.getElementById("yearSelect");
const metricSelect = document.getElementById("metricSelect");
const scatterSelect = document.getElementById("scatterSelect");
const statusBlock = document.getElementById("statusBlock");
const coefTable = document.getElementById("coefTable");
const topTableBody = document.querySelector("#topTable tbody");
const gpuStatus = document.getElementById("gpuStatus");
const webgpuCanvas = document.getElementById("webgpuCanvas");

let panel = [];
let yearSummary = [];
let regressionTerms = [];
let validationChecks = [];
let topLatest = [];
let countiesGeo = [];
let deckgl = null;

const fmtShort = new Intl.NumberFormat("en-US", {
  notation: "compact",
  compactDisplay: "short",
  maximumFractionDigits: 1
});

const fmtPct = new Intl.NumberFormat("en-US", {
  style: "percent",
  maximumFractionDigits: 1
});

const fmtNum2 = new Intl.NumberFormat("en-US", {
  maximumFractionDigits: 2
});

function byYear(y) {
  return panel.filter((d) => d.year === y);
}

function logValue(v) {
  if (v === null || v === undefined || !Number.isFinite(v)) return null;
  return Math.log10(v + 1);
}

function colorFromScale(value, minVal, maxVal) {
  if (!Number.isFinite(value)) return [220, 224, 228, 180];
  const t = Math.max(0, Math.min(1, (value - minVal) / (maxVal - minVal || 1)));
  const r = Math.round(20 + 200 * t);
  const g = Math.round(55 + 80 * (1 - t));
  const b = Math.round(120 + 90 * (1 - t));
  return [r, g, b, 210];
}

function createStatusBlock(meta) {
  const failed = validationChecks.filter((d) => d.status !== "pass").length;
  const checks = validationChecks.length;
  statusBlock.innerHTML = [
    `<p><strong>Generated:</strong> ${new Date(meta.generated_at).toLocaleString()}</p>`,
    `<p><strong>Panel rows:</strong> ${meta.n_panel_rows.toLocaleString()}</p>`,
    `<p><strong>Latest counties:</strong> ${meta.n_latest_counties.toLocaleString()}</p>`,
    `<p><strong>Validation:</strong> ${checks - failed}/${checks} passing</p>`
  ].join("");
}

function createCoefTable() {
  const rows = regressionTerms
    .filter((d) =>
      ["gop_share_20", "swingness_20", "gop_share_20:swingness_20"].includes(d.term)
    )
    .map(
      (d) => `<tr>
        <td>${d.specification}</td>
        <td>${d.term}</td>
        <td>${fmtNum2.format(d.estimate)}</td>
        <td>${fmtNum2.format(d.std_error)}</td>
        <td>${d.p_value < 0.001 ? "<0.001" : d.p_value.toFixed(3)}</td>
      </tr>`
    )
    .join("");

  coefTable.innerHTML = `<table>
    <thead>
      <tr><th>Spec</th><th>Term</th><th>Estimate</th><th>SE</th><th>p</th></tr>
    </thead>
    <tbody>${rows}</tbody>
  </table>`;
}

function populateControls() {
  const years = [...new Set(panel.map((d) => d.year))].sort((a, b) => a - b);
  years.forEach((y) => {
    const opt = document.createElement("option");
    opt.value = String(y);
    opt.textContent = String(y);
    yearSelect.appendChild(opt);
  });
  yearSelect.value = String(Math.max(...years));
}

function buildTopTable(year) {
  const rows = byYear(year)
    .filter((d) => Number.isFinite(d.county_tariff_exposure_mean))
    .sort((a, b) => b.county_tariff_exposure_mean - a.county_tariff_exposure_mean)
    .slice(0, 50);

  topTableBody.innerHTML = rows
    .map(
      (d) => `<tr>
      <td>${d.county_name || ""}</td>
      <td>${d.state_name || ""}</td>
      <td>${fmtShort.format(d.county_tariff_exposure_mean)}</td>
      <td>${fmtShort.format(d.county_tariff_exposure_per_worker || 0)}</td>
      <td>${Number.isFinite(d.gop_share_20) ? fmtPct.format(d.gop_share_20) : ""}</td>
    </tr>`
    )
    .join("");
}

function buildTrend() {
  const traceMedian = {
    x: yearSummary.map((d) => d.year),
    y: yearSummary.map((d) => d.exposure_median),
    mode: "lines+markers",
    type: "scatter",
    name: "Median",
    line: { color: "#0e7490", width: 2 }
  };

  const traceP90 = {
    x: yearSummary.map((d) => d.year),
    y: yearSummary.map((d) => d.exposure_p90),
    mode: "lines+markers",
    type: "scatter",
    name: "P90",
    line: { color: "#a16207", width: 2 }
  };

  Plotly.newPlot(
    "trend",
    [traceMedian, traceP90],
    {
      margin: { t: 32, r: 10, b: 45, l: 65 },
      xaxis: { title: "Year" },
      yaxis: { title: "Exposure", type: "log" },
      legend: { orientation: "h" }
    },
    { responsive: true, displayModeBar: false }
  );
}

function buildScatter(year, xKey) {
  const rows = byYear(year).filter(
    (d) =>
      Number.isFinite(d[xKey]) && Number.isFinite(d.county_tariff_exposure_mean) && d.county_tariff_exposure_mean > 0
  );

  const trace = {
    x: rows.map((d) => d[xKey]),
    y: rows.map((d) => logValue(d.county_tariff_exposure_mean)),
    text: rows.map((d) => `${d.county_name || ""}, ${d.state_name || ""}`),
    type: "scattergl",
    mode: "markers",
    marker: {
      color: rows.map((d) => d.swingness_20 ?? 0),
      colorscale: "Viridis",
      size: 5,
      opacity: 0.6,
      colorbar: { title: "Swingness20" }
    },
    hovertemplate: "%{text}<br>x=%{x:.3f}<br>log10(exposure+1)=%{y:.3f}<extra></extra>"
  };

  Plotly.newPlot(
    "scatter",
    [trace],
    {
      margin: { t: 30, r: 20, b: 45, l: 65 },
      xaxis: { title: scatterLabels[xKey] || xKey },
      yaxis: { title: "log10(exposure + 1)" }
    },
    { responsive: true, displayModeBar: false }
  );
}

function buildMap(year, metricKey) {
  const rows = byYear(year);
  const rowMap = new Map(rows.map((d) => [d.county_fips, d]));

  const values = rows
    .map((d) => d[metricKey])
    .filter((v) => Number.isFinite(v))
    .map((v) => (metricKey.includes("exposure") && v > 1 ? Math.log10(v + 1) : v));

  const minVal = Math.min(...values);
  const maxVal = Math.max(...values);

  const layer = new deck.GeoJsonLayer({
    id: "county-layer",
    data: countiesGeo,
    stroked: true,
    filled: true,
    getLineColor: [245, 245, 245, 120],
    lineWidthMinPixels: 0.2,
    pickable: true,
    getFillColor: (f) => {
      const row = rowMap.get(f.id);
      if (!row) return [225, 225, 230, 130];
      const raw = row[metricKey];
      const val =
        metricKey.includes("exposure") && Number.isFinite(raw) && raw > 1
          ? Math.log10(raw + 1)
          : raw;
      return colorFromScale(val, minVal, maxVal);
    },
    getTooltip: ({ object }) => {
      if (!object) return null;
      const row = rowMap.get(object.id);
      if (!row) {
        return { text: `FIPS ${object.id}: no data` };
      }
      const v = row[metricKey];
      return {
        text:
          `${row.county_name || "County"}, ${row.state_name || ""}\n` +
          `${metricLabels[metricKey] || metricKey}: ${Number.isFinite(v) ? fmtNum2.format(v) : "NA"}`
      };
    }
  });

  if (!deckgl) {
    deckgl = new deck.DeckGL({
      container: "map",
      initialViewState: {
        longitude: -98.35,
        latitude: 38.5,
        zoom: 3.3,
        minZoom: 2.5,
        maxZoom: 8
      },
      controller: true,
      views: new deck.MapView({ repeat: false }),
      layers: [layer]
    });
  } else {
    deckgl.setProps({ layers: [layer] });
  }
}

async function renderWebGPUScatter(year, xKey) {
  if (!("gpu" in navigator)) {
    gpuStatus.textContent = "WebGPU unavailable in this browser. WebGL map/scatter remains active.";
    return;
  }

  const adapter = await navigator.gpu.requestAdapter();
  if (!adapter) {
    gpuStatus.textContent = "WebGPU adapter not available.";
    return;
  }

  const device = await adapter.requestDevice();
  const context = webgpuCanvas.getContext("webgpu");
  const format = navigator.gpu.getPreferredCanvasFormat();
  context.configure({ device, format, alphaMode: "opaque" });

  const rows = byYear(year).filter(
    (d) =>
      Number.isFinite(d[xKey]) && Number.isFinite(d.county_tariff_exposure_mean) && d.county_tariff_exposure_mean > 0
  );

  if (rows.length === 0) {
    gpuStatus.textContent = "No data available for WebGPU preview in selected year.";
    return;
  }

  const xVals = rows.map((d) => d[xKey]);
  const yVals = rows.map((d) => logValue(d.county_tariff_exposure_mean));
  const xMin = Math.min(...xVals);
  const xMax = Math.max(...xVals);
  const yMin = Math.min(...yVals);
  const yMax = Math.max(...yVals);

  const points = [];
  const size = 0.006;
  for (let i = 0; i < rows.length; i += 1) {
    const xNdc = ((xVals[i] - xMin) / (xMax - xMin || 1)) * 2 - 1;
    const yNdc = ((yVals[i] - yMin) / (yMax - yMin || 1)) * 2 - 1;
    const c = rows[i].swingness_20 ?? 0.5;
    const r = 0.2 + 0.7 * c;
    const g = 0.3 + 0.5 * (1 - c);
    const b = 0.8 - 0.6 * c;

    const verts = [
      [xNdc - size, yNdc - size],
      [xNdc + size, yNdc - size],
      [xNdc + size, yNdc + size],
      [xNdc - size, yNdc - size],
      [xNdc + size, yNdc + size],
      [xNdc - size, yNdc + size]
    ];

    verts.forEach((v) => {
      points.push(v[0], v[1], r, g, b, 0.95);
    });
  }

  const vertices = new Float32Array(points);
  const vertexBuffer = device.createBuffer({
    size: vertices.byteLength,
    usage: GPUBufferUsage.VERTEX | GPUBufferUsage.COPY_DST
  });
  device.queue.writeBuffer(vertexBuffer, 0, vertices);

  const shaderModule = device.createShaderModule({
    code: `
      struct VertexInput {
        @location(0) pos: vec2f,
        @location(1) color: vec4f
      };
      struct VertexOutput {
        @builtin(position) position: vec4f,
        @location(0) color: vec4f
      };
      @vertex
      fn vs_main(in: VertexInput) -> VertexOutput {
        var out: VertexOutput;
        out.position = vec4f(in.pos, 0.0, 1.0);
        out.color = in.color;
        return out;
      }
      @fragment
      fn fs_main(in: VertexOutput) -> @location(0) vec4f {
        return in.color;
      }
    `
  });

  const pipeline = device.createRenderPipeline({
    layout: "auto",
    vertex: {
      module: shaderModule,
      entryPoint: "vs_main",
      buffers: [
        {
          arrayStride: 24,
          attributes: [
            { shaderLocation: 0, offset: 0, format: "float32x2" },
            { shaderLocation: 1, offset: 8, format: "float32x4" }
          ]
        }
      ]
    },
    fragment: {
      module: shaderModule,
      entryPoint: "fs_main",
      targets: [{ format }]
    },
    primitive: { topology: "triangle-list" }
  });

  const encoder = device.createCommandEncoder();
  const pass = encoder.beginRenderPass({
    colorAttachments: [
      {
        view: context.getCurrentTexture().createView(),
        clearValue: { r: 0.06, g: 0.09, b: 0.14, a: 1.0 },
        loadOp: "clear",
        storeOp: "store"
      }
    ]
  });

  pass.setPipeline(pipeline);
  pass.setVertexBuffer(0, vertexBuffer);
  pass.draw(vertices.length / 6);
  pass.end();
  device.queue.submit([encoder.finish()]);

  gpuStatus.textContent = `WebGPU active. Rendered ${rows.length.toLocaleString()} county points.`;
}

async function loadData() {
  const [panelRes, yearRes, regRes, validRes, topRes, metaRes] = await Promise.all([
    fetch(`${DATA_DIR}/panel.json`),
    fetch(`${DATA_DIR}/year_summary.json`),
    fetch(`${DATA_DIR}/regression_main_terms.json`),
    fetch(`${DATA_DIR}/validation_checks.json`),
    fetch(`${DATA_DIR}/top_latest.json`),
    fetch(`${DATA_DIR}/meta.json`)
  ]);

  panel = await panelRes.json();
  yearSummary = await yearRes.json();
  regressionTerms = await regRes.json();
  validationChecks = await validRes.json();
  topLatest = await topRes.json();
  const meta = await metaRes.json();

  const topo = await fetch("https://cdn.jsdelivr.net/npm/us-atlas@3/counties-10m.json").then((r) => r.json());
  countiesGeo = topojson.feature(topo, topo.objects.counties).features.filter(
    (f) => !EXCLUDE_STATEFP.has(String(f.id).slice(0, 2))
  );

  createStatusBlock(meta);
  createCoefTable();
  populateControls();

  const selectedYear = Number(yearSelect.value);
  buildMap(selectedYear, metricSelect.value);
  buildScatter(selectedYear, scatterSelect.value);
  buildTrend();
  buildTopTable(selectedYear);
  await renderWebGPUScatter(selectedYear, scatterSelect.value);
}

function registerHandlers() {
  yearSelect.addEventListener("change", async () => {
    const selectedYear = Number(yearSelect.value);
    buildMap(selectedYear, metricSelect.value);
    buildScatter(selectedYear, scatterSelect.value);
    buildTopTable(selectedYear);
    await renderWebGPUScatter(selectedYear, scatterSelect.value);
  });

  metricSelect.addEventListener("change", () => {
    buildMap(Number(yearSelect.value), metricSelect.value);
  });

  scatterSelect.addEventListener("change", async () => {
    const selectedYear = Number(yearSelect.value);
    buildScatter(selectedYear, scatterSelect.value);
    await renderWebGPUScatter(selectedYear, scatterSelect.value);
  });
}

loadData()
  .then(registerHandlers)
  .catch((err) => {
    console.error(err);
    statusBlock.innerHTML = `<p>Failed to load site data. Run <code>code/build_site_data.R</code> and retry.</p>`;
  });
