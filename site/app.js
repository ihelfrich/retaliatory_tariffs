const DATA_DIR = "./data";
const EXCLUDE_STATEFP = new Set(["02", "15", "60", "66", "69", "72", "78"]);

const KNOWN_LABELS = {
  county_tariff_exposure_mean: "Tariff exposure (level)",
  county_tariff_exposure_p90: "Tariff exposure (p90)",
  county_tariff_exposure_per_worker: "Tariff exposure (per worker)",
  county_allocated_exports_total: "Allocated exports",
  tariffed_emp_share: "Tariffed employment share",
  county_emp_total: "County employment",
  sectors_count: "Sector count",
  tariffed_sectors_count: "Tariffed sector count",
  gop_share_16: "GOP share 2016",
  gop_share_20: "GOP share 2020",
  gop_share_24: "GOP share 2024",
  dem_share_16: "Dem share 2016",
  dem_share_20: "Dem share 2020",
  dem_share_24: "Dem share 2024",
  margin_16: "Margin 2016",
  margin_20: "Margin 2020",
  margin_24: "Margin 2024",
  swingness_16: "Swingness 2016",
  swingness_20: "Swingness 2020",
  swingness_24: "Swingness 2024",
  gop_shift_16_20: "GOP shift 2016-2020",
  gop_shift_20_24: "GOP shift 2020-2024",
  exposure_rank_within_year: "Exposure rank in year",
  political_salience_weighted_exposure: "Political salience weighted exposure"
};

const PALETTES = {
  teal_amber: [
    [29, 61, 78],
    [23, 112, 139],
    [85, 171, 178],
    [225, 201, 120],
    [169, 103, 31]
  ],
  blue_red: [
    [24, 57, 99],
    [74, 128, 178],
    [222, 233, 242],
    [214, 132, 112],
    [141, 48, 33]
  ],
  forest_gold: [
    [35, 66, 44],
    [68, 109, 58],
    [157, 168, 102],
    [218, 194, 116],
    [147, 102, 44]
  ]
};

const DEFAULT_MAP_VAR = "county_tariff_exposure_mean";
const DEFAULT_SCATTER_X = "gop_share_20";
const DEFAULT_SCATTER_Y = "county_tariff_exposure_mean";
const DEFAULT_SCATTER_COLOR = "swingness_20";

const dom = {
  loadStatus: document.getElementById("loadStatus"),
  statusBlock: document.getElementById("statusBlock"),
  countyDetails: document.getElementById("countyDetails"),
  mapLegend: document.getElementById("mapLegend"),
  mapTitle: document.getElementById("mapTitle"),
  scatterTitle: document.getElementById("scatterTitle"),
  trendTitle: document.getElementById("trendTitle"),
  tableTitle: document.getElementById("tableTitle"),

  yearSelect: document.getElementById("yearSelect"),
  mapMetricSelect: document.getElementById("mapMetricSelect"),
  mapTransformSelect: document.getElementById("mapTransformSelect"),
  mapPaletteSelect: document.getElementById("mapPaletteSelect"),
  resetMapBtn: document.getElementById("resetMapBtn"),

  scatterXSelect: document.getElementById("scatterXSelect"),
  scatterYSelect: document.getElementById("scatterYSelect"),
  scatterColorSelect: document.getElementById("scatterColorSelect"),

  customNameInput: document.getElementById("customNameInput"),
  customFormulaInput: document.getElementById("customFormulaInput"),
  customParamsInput: document.getElementById("customParamsInput"),
  addCustomVarBtn: document.getElementById("addCustomVarBtn"),
  removeCustomVarBtn: document.getElementById("removeCustomVarBtn"),
  customVarList: document.getElementById("customVarList"),
  customVarFeedback: document.getElementById("customVarFeedback"),

  regScopeSelect: document.getElementById("regScopeSelect"),
  regDependentSelect: document.getElementById("regDependentSelect"),
  regPredictorsInput: document.getElementById("regPredictorsInput"),
  regInterceptCheck: document.getElementById("regInterceptCheck"),
  runRegBtn: document.getElementById("runRegBtn"),
  regSummary: document.getElementById("regSummary"),
  regResultBody: document.querySelector("#regResultTable tbody"),

  topTableBody: document.querySelector("#topTable tbody")
};

const state = {
  panel: [],
  yearSummary: [],
  validationChecks: [],
  meta: null,
  countiesGeo: [],
  years: [],
  latestYear: null,
  numericVars: [],
  customVars: new Map(),
  selectedCountyFips: null,
  mapViewState: {
    longitude: -98.35,
    latitude: 38.5,
    zoom: 3.25,
    minZoom: 2.4,
    maxZoom: 9
  },
  deck: null
};

const fmt = {
  short: new Intl.NumberFormat("en-US", {
    notation: "compact",
    compactDisplay: "short",
    maximumFractionDigits: 1
  }),
  num2: new Intl.NumberFormat("en-US", { maximumFractionDigits: 2 }),
  num3: new Intl.NumberFormat("en-US", { maximumFractionDigits: 3 }),
  pct1: new Intl.NumberFormat("en-US", {
    style: "percent",
    maximumFractionDigits: 1
  })
};

const mathApi = globalThis.math;
const jStatApi = globalThis.jStat;

function autoLabel(key) {
  if (KNOWN_LABELS[key]) return KNOWN_LABELS[key];
  return key
    .replace(/_/g, " ")
    .replace(/\b\w/g, (c) => c.toUpperCase());
}

function normalizeFips(value) {
  if (value === null || value === undefined) return null;
  const s = String(value).replace(/[^0-9]/g, "");
  if (!s) return null;
  return s.padStart(5, "0");
}

function finiteOrNull(value) {
  return Number.isFinite(value) ? value : null;
}

function signedLog(value) {
  if (!Number.isFinite(value)) return null;
  if (value === 0) return 0;
  const sign = value >= 0 ? 1 : -1;
  return sign * Math.log10(Math.abs(value) + 1);
}

function quantile(sortedValues, p) {
  if (sortedValues.length === 0) return null;
  const idx = (sortedValues.length - 1) * p;
  const lo = Math.floor(idx);
  const hi = Math.ceil(idx);
  if (lo === hi) return sortedValues[lo];
  const w = idx - lo;
  return sortedValues[lo] * (1 - w) + sortedValues[hi] * w;
}

function median(values) {
  if (values.length === 0) return null;
  const sorted = [...values].sort((a, b) => a - b);
  return quantile(sorted, 0.5);
}

function mean(values) {
  if (values.length === 0) return null;
  return values.reduce((acc, v) => acc + v, 0) / values.length;
}

function sd(values, m) {
  if (values.length < 2 || !Number.isFinite(m)) return null;
  const ss = values.reduce((acc, v) => acc + (v - m) ** 2, 0);
  return Math.sqrt(ss / (values.length - 1));
}

function byYear(year) {
  return state.panel.filter((row) => row.year === year);
}

async function fetchJson(path) {
  const response = await fetch(path);
  if (!response.ok) {
    throw new Error(`Request failed (${response.status}) for ${path}`);
  }
  return response.json();
}

function setStatus(text, type = "info") {
  dom.loadStatus.textContent = text;
  dom.loadStatus.classList.remove("good", "bad");
  if (type === "good") dom.loadStatus.classList.add("good");
  if (type === "bad") dom.loadStatus.classList.add("bad");
}

function parsePanelRows(rows) {
  return rows.map((row) => {
    const out = { ...row };
    out.county_fips = normalizeFips(row.county_fips);
    out.year = Number(row.year);

    Object.entries(out).forEach(([k, v]) => {
      if (k === "county_fips" || k === "county_name" || k === "state_name") return;
      if (v === null || v === undefined || v === "") return;
      const n = Number(v);
      if (Number.isFinite(n)) out[k] = n;
    });

    return out;
  });
}

function detectNumericVars(rows) {
  if (rows.length === 0) return [];
  const keys = new Set();
  rows.forEach((row) => {
    Object.keys(row).forEach((k) => keys.add(k));
  });

  const disallow = new Set(["county_fips", "county_name", "state_name"]);
  return [...keys]
    .filter((k) => !disallow.has(k))
    .filter((k) => rows.some((row) => Number.isFinite(row[k])));
}

function numericVarOptions() {
  return state.numericVars.map((k) => ({ value: k, label: autoLabel(k) }));
}

function replaceOptions(selectEl, options, preferred) {
  const prev = selectEl.value;
  selectEl.innerHTML = "";

  options.forEach((opt) => {
    const node = document.createElement("option");
    node.value = opt.value;
    node.textContent = opt.label;
    selectEl.appendChild(node);
  });

  const candidate = [prev, preferred].find((v) => v && options.some((o) => o.value === v));
  if (candidate) {
    selectEl.value = candidate;
  } else if (options.length > 0) {
    selectEl.value = options[0].value;
  }
}

function rebuildVariableControls() {
  const options = numericVarOptions();

  replaceOptions(dom.mapMetricSelect, options, DEFAULT_MAP_VAR);
  replaceOptions(dom.scatterXSelect, options, DEFAULT_SCATTER_X);
  replaceOptions(dom.scatterYSelect, options, DEFAULT_SCATTER_Y);
  replaceOptions(dom.scatterColorSelect, options, DEFAULT_SCATTER_COLOR);
  replaceOptions(dom.regDependentSelect, options, DEFAULT_SCATTER_Y);

  if (!dom.regPredictorsInput.value.trim()) {
    dom.regPredictorsInput.value = [
      DEFAULT_SCATTER_X,
      "swingness_20",
      "tariffed_emp_share"
    ].filter((k) => options.some((o) => o.value === k)).join(", ");
  }
}

function buildStatusBlock() {
  const checks = state.validationChecks || [];
  const pass = checks.filter((c) => String(c.status).toLowerCase() === "pass").length;
  const fail = checks.length - pass;

  const lines = [
    `<p><strong>Generated:</strong> ${state.meta?.generated_at ? String(state.meta.generated_at) : "n/a"}</p>`,
    `<p><strong>Rows:</strong> ${(state.meta?.n_panel_rows || state.panel.length).toLocaleString()}</p>`,
    `<p><strong>Latest year:</strong> ${state.latestYear ?? "n/a"}</p>`,
    `<p><strong>Validation:</strong> <span class="${fail === 0 ? "good" : "bad"}">${pass}/${checks.length} passing</span></p>`
  ];

  const detail = checks.slice(0, 8).map((c) => {
    const ok = String(c.status).toLowerCase() === "pass";
    return `<p class="${ok ? "good" : "bad"}">${c.check}: ${c.status}</p>`;
  });

  dom.statusBlock.innerHTML = [...lines, ...detail].join("");
}

function transformSeries(values, transform) {
  const raw = values.map((v) => finiteOrNull(v));

  if (transform === "none") {
    return raw;
  }

  if (transform === "log") {
    return raw.map((v) => signedLog(v));
  }

  if (transform === "zscore") {
    const finite = raw.filter((v) => Number.isFinite(v));
    const m = mean(finite);
    const s = sd(finite, m);
    return raw.map((v) => {
      if (!Number.isFinite(v)) return null;
      if (!Number.isFinite(s) || s === 0) return 0;
      return (v - m) / s;
    });
  }

  if (transform === "rank") {
    const finite = raw
      .map((v, i) => ({ v, i }))
      .filter((x) => Number.isFinite(x.v))
      .sort((a, b) => a.v - b.v);

    const ranked = new Array(raw.length).fill(null);
    const denom = Math.max(finite.length - 1, 1);
    finite.forEach((x, idx) => {
      ranked[x.i] = idx / denom;
    });
    return ranked;
  }

  return raw;
}

function paletteColor(t, paletteName) {
  const colors = PALETTES[paletteName] || PALETTES.teal_amber;
  const clamped = Math.max(0, Math.min(1, t));

  const segment = (colors.length - 1) * clamped;
  const lo = Math.floor(segment);
  const hi = Math.min(colors.length - 1, lo + 1);
  const w = segment - lo;

  const c0 = colors[lo];
  const c1 = colors[hi];
  return [
    Math.round(c0[0] * (1 - w) + c1[0] * w),
    Math.round(c0[1] * (1 - w) + c1[1] * w),
    Math.round(c0[2] * (1 - w) + c1[2] * w),
    220
  ];
}

function formatValue(v) {
  if (!Number.isFinite(v)) return "NA";
  const abs = Math.abs(v);
  if (abs >= 1000000) return fmt.short.format(v);
  if (abs >= 1) return fmt.num2.format(v);
  if (abs === 0) return "0";
  return fmt.num3.format(v);
}

function currentYear() {
  return Number(dom.yearSelect.value);
}

function selectedRows() {
  return byYear(currentYear());
}

function createLegend(minVal, maxVal, paletteName, transformLabel) {
  const colors = PALETTES[paletteName] || PALETTES.teal_amber;
  const stops = colors
    .map((c, idx) => {
      const p = (idx / Math.max(colors.length - 1, 1)) * 100;
      return `rgb(${c[0]} ${c[1]} ${c[2]}) ${p.toFixed(1)}%`;
    })
    .join(", ");

  dom.mapLegend.innerHTML = [
    `<span>${formatValue(minVal)}</span>`,
    `<span class="legend-gradient" style="background: linear-gradient(90deg, ${stops});"></span>`,
    `<span>${formatValue(maxVal)}</span>`,
    `<span>(${transformLabel})</span>`
  ].join("");
}

function buildMap() {
  const year = currentYear();
  const metricKey = dom.mapMetricSelect.value;
  const transform = dom.mapTransformSelect.value;
  const palette = dom.mapPaletteSelect.value;

  const rows = byYear(year);
  const rowMap = new Map(rows.map((r) => [r.county_fips, r]));
  const metricRaw = rows.map((r) => r[metricKey]);
  const metricTransformed = transformSeries(metricRaw, transform);

  const valueByFips = new Map();
  rows.forEach((row, idx) => {
    valueByFips.set(row.county_fips, metricTransformed[idx]);
  });

  const finiteVals = metricTransformed.filter((v) => Number.isFinite(v));
  const minVal = finiteVals.length > 0 ? Math.min(...finiteVals) : 0;
  const maxVal = finiteVals.length > 0 ? Math.max(...finiteVals) : 1;

  dom.mapTitle.textContent = `County map: ${autoLabel(metricKey)} (${year})`;
  createLegend(minVal, maxVal, palette, transform);

  const layer = new deck.GeoJsonLayer({
    id: "county-layer",
    data: state.countiesGeo,
    stroked: true,
    filled: true,
    pickable: true,
    autoHighlight: true,
    lineWidthMinPixels: 0.15,
    getLineColor: (feature) => {
      const fips = feature.properties?.__fips;
      const isSelected = fips === state.selectedCountyFips;
      return isSelected ? [10, 10, 10, 255] : [245, 245, 245, 135];
    },
    getLineWidth: (feature) => {
      const fips = feature.properties?.__fips;
      return fips === state.selectedCountyFips ? 1.25 : 0.2;
    },
    getFillColor: (feature) => {
      const fips = feature.properties?.__fips;
      const val = valueByFips.get(fips);
      if (!Number.isFinite(val)) return [226, 229, 232, 150];
      const t = (val - minVal) / (maxVal - minVal || 1);
      return paletteColor(t, palette);
    },
    onClick: (info) => {
      if (!info?.object) return;
      const fips = info.object.properties?.__fips;
      state.selectedCountyFips = fips;
      const row = rowMap.get(fips);
      if (!row) {
        dom.countyDetails.textContent = `FIPS ${fips}: no row found for current year.`;
      } else {
        dom.countyDetails.textContent = `${row.county_name || "County"}, ${row.state_name || ""} | ${autoLabel(metricKey)}: ${formatValue(row[metricKey])}`;
      }
      buildMap();
    }
  });

  const tooltip = ({ object }) => {
    if (!object) return null;
    const fips = object.properties?.__fips;
    const row = rowMap.get(fips);
    if (!row) return { text: `FIPS ${fips}: no data` };
    const val = row[metricKey];
    return {
      text:
        `${row.county_name || "County"}, ${row.state_name || ""}\n` +
        `${autoLabel(metricKey)}: ${formatValue(val)}\n` +
        `FIPS: ${row.county_fips}`
    };
  };

  if (!state.deck) {
    state.deck = new deck.DeckGL({
      container: "map",
      initialViewState: state.mapViewState,
      controller: true,
      views: new deck.MapView({ repeat: false }),
      layers: [layer],
      getTooltip: tooltip
    });
  } else {
    state.deck.setProps({ layers: [layer], getTooltip: tooltip });
  }
}

function buildScatter() {
  const year = currentYear();
  const xKey = dom.scatterXSelect.value;
  const yKey = dom.scatterYSelect.value;
  const colorKey = dom.scatterColorSelect.value;

  const rows = byYear(year).filter((r) =>
    Number.isFinite(r[xKey]) && Number.isFinite(r[yKey])
  );

  const colorVals = rows.map((r) => r[colorKey]).filter((v) => Number.isFinite(v));
  const cMin = colorVals.length > 0 ? Math.min(...colorVals) : 0;
  const cMax = colorVals.length > 0 ? Math.max(...colorVals) : 1;

  dom.scatterTitle.textContent = `Scatter: ${autoLabel(yKey)} vs ${autoLabel(xKey)} (${year})`;

  Plotly.react(
    "scatter",
    [
      {
        x: rows.map((r) => r[xKey]),
        y: rows.map((r) => r[yKey]),
        text: rows.map((r) => `${r.county_name || "County"}, ${r.state_name || ""}`),
        type: "scattergl",
        mode: "markers",
        marker: {
          size: 6,
          opacity: 0.65,
          color: rows.map((r) => r[colorKey]),
          cmin: cMin,
          cmax: cMax,
          colorscale: "Viridis",
          colorbar: { title: autoLabel(colorKey) }
        },
        hovertemplate: "%{text}<br>x=%{x:.4g}<br>y=%{y:.4g}<extra></extra>"
      }
    ],
    {
      margin: { t: 30, r: 12, b: 52, l: 72 },
      xaxis: { title: autoLabel(xKey), zeroline: false },
      yaxis: { title: autoLabel(yKey), zeroline: false }
    },
    { responsive: true, displayModeBar: false }
  );
}

function buildTrend() {
  const metricKey = dom.mapMetricSelect.value;
  const years = [...state.years].sort((a, b) => a - b);

  const trend = years.map((year) => {
    const values = byYear(year).map((r) => r[metricKey]).filter((v) => Number.isFinite(v));
    const sorted = [...values].sort((a, b) => a - b);
    return {
      year,
      median: quantile(sorted, 0.5),
      p90: quantile(sorted, 0.9)
    };
  });

  dom.trendTitle.textContent = `Year trend: ${autoLabel(metricKey)}`;

  Plotly.react(
    "trend",
    [
      {
        x: trend.map((d) => d.year),
        y: trend.map((d) => d.median),
        type: "scatter",
        mode: "lines+markers",
        name: "Median",
        line: { color: "#12677f", width: 2 }
      },
      {
        x: trend.map((d) => d.year),
        y: trend.map((d) => d.p90),
        type: "scatter",
        mode: "lines+markers",
        name: "P90",
        line: { color: "#9d5c17", width: 2 }
      }
    ],
    {
      margin: { t: 30, r: 10, b: 50, l: 72 },
      xaxis: { title: "Year" },
      yaxis: { title: autoLabel(metricKey) },
      legend: { orientation: "h" }
    },
    { responsive: true, displayModeBar: false }
  );
}

function buildTopTable() {
  const year = currentYear();
  const metricKey = dom.mapMetricSelect.value;
  const rows = byYear(year)
    .filter((r) => Number.isFinite(r[metricKey]))
    .sort((a, b) => b[metricKey] - a[metricKey])
    .slice(0, 75);

  dom.tableTitle.textContent = `Top counties by ${autoLabel(metricKey)} (${year})`;

  dom.topTableBody.innerHTML = rows
    .map((r) => `
      <tr>
        <td>${r.county_name || ""}</td>
        <td>${r.state_name || ""}</td>
        <td>${formatValue(r[metricKey])}</td>
        <td>${formatValue(r.county_tariff_exposure_mean)}</td>
        <td>${Number.isFinite(r.gop_share_20) ? fmt.pct1.format(r.gop_share_20) : ""}</td>
      </tr>
    `)
    .join("");
}

function refreshAllViews() {
  buildMap();
  buildScatter();
  buildTrend();
  buildTopTable();
}

function refreshCustomVarList() {
  const selected = dom.customVarList.value;
  dom.customVarList.innerHTML = "";

  [...state.customVars.keys()].sort().forEach((name) => {
    const opt = document.createElement("option");
    opt.value = name;
    opt.textContent = name;
    dom.customVarList.appendChild(opt);
  });

  if (selected && state.customVars.has(selected)) {
    dom.customVarList.value = selected;
  }
}

function parseParams(text) {
  const cleaned = text.trim();
  if (!cleaned) return {};

  let parsed;
  try {
    parsed = JSON.parse(cleaned);
  } catch (_err) {
    throw new Error("Parameter block must be valid JSON, like {\"alpha\": 0.7}.");
  }

  if (parsed === null || typeof parsed !== "object" || Array.isArray(parsed)) {
    throw new Error("Parameters must be a JSON object.");
  }

  const out = {};
  Object.entries(parsed).forEach(([k, v]) => {
    const n = Number(v);
    if (!Number.isFinite(n)) {
      throw new Error(`Parameter '${k}' must be numeric.`);
    }
    out[k] = n;
  });

  return out;
}

function addOrUpdateCustomVariable() {
  const name = dom.customNameInput.value.trim();
  const expr = dom.customFormulaInput.value.trim();

  if (!/^[A-Za-z][A-Za-z0-9_]*$/.test(name)) {
    throw new Error("Variable name must start with a letter and use letters, numbers, or underscore.");
  }

  if (!expr) {
    throw new Error("Formula cannot be empty.");
  }

  if (state.numericVars.includes(name) && !state.customVars.has(name)) {
    throw new Error(`'${name}' already exists as a base variable.`);
  }

  const params = parseParams(dom.customParamsInput.value);
  if (!mathApi || typeof mathApi.compile !== "function") {
    throw new Error("Formula engine failed to load. Reload the page and retry.");
  }

  const compiled = mathApi.compile(expr);

  for (let i = 0; i < state.panel.length; i += 1) {
    const row = state.panel[i];
    const scope = { ...params };

    state.numericVars.forEach((k) => {
      const v = row[k];
      if (Number.isFinite(v)) scope[k] = v;
    });

    let val;
    try {
      val = compiled.evaluate(scope);
    } catch (err) {
      throw new Error(`Formula error around row ${i + 1}: ${err.message}`);
    }

    const num = Number(val);
    row[name] = Number.isFinite(num) ? num : null;
  }

  state.customVars.set(name, { expr, params });
  if (!state.numericVars.includes(name)) {
    state.numericVars.push(name);
    state.numericVars.sort();
  }

  KNOWN_LABELS[name] = `${name} (custom)`;

  refreshCustomVarList();
  rebuildVariableControls();
  refreshAllViews();

  dom.customVarFeedback.textContent = `Saved '${name}' using formula: ${expr}`;
  dom.customVarFeedback.classList.remove("bad");
  dom.customVarFeedback.classList.add("good");
}

function removeSelectedCustomVariable() {
  const name = dom.customVarList.value;
  if (!name) {
    throw new Error("Choose a custom variable to remove.");
  }

  state.panel.forEach((row) => {
    delete row[name];
  });

  state.customVars.delete(name);
  state.numericVars = state.numericVars.filter((v) => v !== name);
  delete KNOWN_LABELS[name];

  refreshCustomVarList();
  rebuildVariableControls();
  refreshAllViews();

  dom.customVarFeedback.textContent = `Removed '${name}'.`;
  dom.customVarFeedback.classList.remove("bad");
  dom.customVarFeedback.classList.add("good");
}

function selectedRegressionRows() {
  const scope = dom.regScopeSelect.value;
  if (scope === "all_years") return state.panel;
  if (scope === "latest_year") return byYear(state.latestYear);
  return selectedRows();
}

function parsePredictorList(text) {
  return text
    .split(",")
    .map((s) => s.trim())
    .filter(Boolean);
}

function runOls(yKey, xKeys, includeIntercept) {
  if (!jStatApi || typeof jStatApi.multiply !== "function") {
    throw new Error("Regression engine failed to load. Reload the page and retry.");
  }

  const rows = selectedRegressionRows();
  const cleaned = rows.filter((row) => {
    if (!Number.isFinite(row[yKey])) return false;
    return xKeys.every((x) => Number.isFinite(row[x]));
  });

  if (cleaned.length < 30) {
    throw new Error(`Not enough complete rows for regression (${cleaned.length}).`);
  }

  const y = cleaned.map((row) => [row[yKey]]);
  const x = cleaned.map((row) => {
    const values = xKeys.map((k) => row[k]);
    return includeIntercept ? [1, ...values] : values;
  });

  const names = includeIntercept ? ["(Intercept)", ...xKeys] : [...xKeys];

  let xTxInv;
  let beta;
  try {
    const xT = jStatApi.transpose(x);
    const xTx = jStatApi.multiply(xT, x);
    xTxInv = jStatApi.inv(xTx);
    const xTy = jStatApi.multiply(xT, y);
    beta = jStatApi.multiply(xTxInv, xTy);
  } catch (_err) {
    throw new Error("X'X is singular. Drop redundant predictors or reduce formula complexity.");
  }

  const yHat = jStatApi.multiply(x, beta);
  const residuals = y.map((row, i) => row[0] - yHat[i][0]);
  const sse = residuals.reduce((acc, v) => acc + v * v, 0);
  const yMean = mean(y.map((row) => row[0]));
  const sst = y.reduce((acc, row) => acc + (row[0] - yMean) ** 2, 0);

  const n = cleaned.length;
  const p = names.length;
  const dof = n - p;
  if (dof <= 0) {
    throw new Error(`Degrees of freedom <= 0 (n=${n}, p=${p}).`);
  }

  const sigma2 = sse / dof;
  const vcov = jStatApi.multiply(sigma2, xTxInv);

  const coefRows = names.map((term, i) => {
    const est = beta[i][0];
    const se = Math.sqrt(Math.max(vcov[i][i], 0));
    const t = se > 0 ? est / se : null;
    const pVal = Number.isFinite(t)
      ? 2 * (1 - jStatApi.studentt.cdf(Math.abs(t), dof))
      : null;

    return { term, estimate: est, se, t, p: pVal };
  });

  const r2 = sst > 0 ? 1 - sse / sst : null;

  return {
    n,
    p,
    dof,
    r2,
    dep: yKey,
    predictors: xKeys,
    includeIntercept,
    coefficients: coefRows
  };
}

function renderRegressionResult(result) {
  dom.regSummary.innerHTML = [
    `<strong>Dependent:</strong> ${autoLabel(result.dep)}`,
    `<strong>N:</strong> ${result.n.toLocaleString()}`,
    `<strong>k:</strong> ${result.p}`,
    `<strong>R²:</strong> ${result.r2 === null ? "NA" : fmt.num3.format(result.r2)}`,
    `<strong>Sample:</strong> ${dom.regScopeSelect.options[dom.regScopeSelect.selectedIndex].text}`
  ].join(" | ");

  dom.regResultBody.innerHTML = result.coefficients
    .map((c) => `
      <tr>
        <td>${c.term}</td>
        <td>${formatValue(c.estimate)}</td>
        <td>${formatValue(c.se)}</td>
        <td>${c.t === null ? "NA" : fmt.num2.format(c.t)}</td>
        <td>${c.p === null ? "NA" : c.p < 0.001 ? "<0.001" : fmt.num3.format(c.p)}</td>
      </tr>
    `)
    .join("");
}

function runRegressionFromUi() {
  const dep = dom.regDependentSelect.value;
  const predictors = parsePredictorList(dom.regPredictorsInput.value);

  if (!dep) {
    throw new Error("Pick a dependent variable.");
  }

  if (predictors.length === 0) {
    throw new Error("Add at least one predictor.");
  }

  const known = new Set(state.numericVars);
  const unknown = predictors.filter((x) => !known.has(x));
  if (unknown.length > 0) {
    throw new Error(`Unknown predictors: ${unknown.join(", ")}`);
  }

  if (!known.has(dep)) {
    throw new Error(`Unknown dependent variable: ${dep}`);
  }

  const includeIntercept = dom.regInterceptCheck.checked;
  const result = runOls(dep, predictors, includeIntercept);
  renderRegressionResult(result);
}

function attachHandlers() {
  [
    dom.yearSelect,
    dom.mapMetricSelect,
    dom.mapTransformSelect,
    dom.mapPaletteSelect,
    dom.scatterXSelect,
    dom.scatterYSelect,
    dom.scatterColorSelect
  ].forEach((el) => {
    el.addEventListener("change", () => {
      refreshAllViews();
    });
  });

  dom.resetMapBtn.addEventListener("click", () => {
    state.selectedCountyFips = null;
    dom.countyDetails.textContent = "Click a county to pin details here.";
    if (state.deck) {
      state.deck.setProps({
        initialViewState: state.mapViewState,
        viewState: state.mapViewState
      });
    }
    buildMap();
  });

  dom.addCustomVarBtn.addEventListener("click", () => {
    try {
      addOrUpdateCustomVariable();
    } catch (err) {
      dom.customVarFeedback.textContent = err.message;
      dom.customVarFeedback.classList.remove("good");
      dom.customVarFeedback.classList.add("bad");
    }
  });

  dom.removeCustomVarBtn.addEventListener("click", () => {
    try {
      removeSelectedCustomVariable();
    } catch (err) {
      dom.customVarFeedback.textContent = err.message;
      dom.customVarFeedback.classList.remove("good");
      dom.customVarFeedback.classList.add("bad");
    }
  });

  dom.runRegBtn.addEventListener("click", () => {
    try {
      runRegressionFromUi();
      dom.regSummary.classList.remove("bad");
      dom.regSummary.classList.add("good");
    } catch (err) {
      dom.regSummary.textContent = err.message;
      dom.regSummary.classList.remove("good");
      dom.regSummary.classList.add("bad");
      dom.regResultBody.innerHTML = "";
    }
  });
}

function buildYearSelector() {
  dom.yearSelect.innerHTML = "";
  state.years.forEach((year) => {
    const opt = document.createElement("option");
    opt.value = String(year);
    opt.textContent = String(year);
    dom.yearSelect.appendChild(opt);
  });
  dom.yearSelect.value = String(state.latestYear);
}

async function loadCoreData() {
  setStatus("Loading data files...");

  const [panelJson, yearSummary, validationChecks, meta] = await Promise.all([
    fetchJson(`${DATA_DIR}/panel.json`),
    fetchJson(`${DATA_DIR}/year_summary.json`).catch(() => []),
    fetchJson(`${DATA_DIR}/validation_checks.json`).catch(() => []),
    fetchJson(`${DATA_DIR}/meta.json`)
  ]);

  state.panel = parsePanelRows(panelJson);
  state.yearSummary = yearSummary;
  state.validationChecks = validationChecks;
  state.meta = meta;

  state.years = [...new Set(state.panel.map((r) => r.year).filter((v) => Number.isFinite(v)))].sort((a, b) => a - b);
  state.latestYear = Number.isFinite(meta.latest_year)
    ? Number(meta.latest_year)
    : Math.max(...state.years);

  state.numericVars = detectNumericVars(state.panel).sort();

  const topo = await fetchJson("https://cdn.jsdelivr.net/npm/us-atlas@3/counties-10m.json");
  state.countiesGeo = topojson
    .feature(topo, topo.objects.counties)
    .features
    .map((feature) => {
      const fips = normalizeFips(feature.id ?? feature.properties?.GEOID);
      return {
        ...feature,
        properties: {
          ...(feature.properties || {}),
          __fips: fips
        }
      };
    })
    .filter((feature) => {
      const fips = feature.properties?.__fips;
      if (!fips) return false;
      return !EXCLUDE_STATEFP.has(fips.slice(0, 2));
    });
}

async function init() {
  try {
    await loadCoreData();
    buildYearSelector();
    rebuildVariableControls();
    buildStatusBlock();
    refreshCustomVarList();
    refreshAllViews();
    attachHandlers();

    setStatus("Loaded", "good");

    try {
      runRegressionFromUi();
    } catch (_err) {
      dom.regSummary.textContent = "Ready. Set variables and click Run regression.";
    }
  } catch (err) {
    console.error(err);
    setStatus("Load failed", "bad");
    dom.statusBlock.innerHTML = `<p class="bad">${err.message}</p><p>Run <code>source('code/build_site_data.R')</code> and redeploy <code>site/</code>.</p>`;
  }
}

init();
