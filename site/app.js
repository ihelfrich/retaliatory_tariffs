const DATA_DIR = "./data";
const EXCLUDE_STATEFP = new Set(["02", "15", "60", "66", "69", "72", "78"]);

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

const KNOWN_LABELS = {
  county_tariff_exposure_mean: "Tariff exposure (mean)",
  county_tariff_exposure_p90: "Tariff exposure (p90)",
  county_tariff_exposure_per_worker: "Tariff exposure per worker",
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
  exposure_rank_within_year: "Exposure rank within year",
  political_salience_weighted_exposure: "Political salience weighted exposure"
};

const DEFAULT_VIEW_STATE = {
  longitude: -98.35,
  latitude: 38.5,
  zoom: 3.25,
  minZoom: 2.4,
  maxZoom: 9,
  pitch: 0,
  bearing: 0
};

const DEFAULTS = {
  mapVar: "county_tariff_exposure_mean",
  mapTransform: "none",
  mapScaleMode: "variable_year_auto",
  mapColorScale: "quantile",
  mapPalette: "teal_amber",
  mapRenderMode: "extruded_3d",
  mapElevationScale: 70000,
  scatterX: "gop_share_20",
  scatterY: "county_tariff_exposure_mean",
  scatterColor: "swingness_20",
  regDep: "county_tariff_exposure_mean",
  regPreds: ["gop_share_20", "swingness_20", "tariffed_emp_share"],
  regScope: "latest_year",
  regSeMode: "hc1",
  regIntercept: true
};

const mathApi = globalThis.math;
const jStatApi = globalThis.jStat;

const dom = {
  loadStatus: document.getElementById("loadStatus"),
  statusBlock: document.getElementById("statusBlock"),
  mapTitle: document.getElementById("mapTitle"),
  mapLegend: document.getElementById("mapLegend"),
  mapStats: document.getElementById("mapStats"),
  mapNotice: document.getElementById("mapNotice"),
  countyDetails: document.getElementById("countyDetails"),
  scatterTitle: document.getElementById("scatterTitle"),
  trendTitle: document.getElementById("trendTitle"),
  tableTitle: document.getElementById("tableTitle"),

  yearSelect: document.getElementById("yearSelect"),
  mapMetricSelect: document.getElementById("mapMetricSelect"),
  mapTransformSelect: document.getElementById("mapTransformSelect"),
  mapScaleModeSelect: document.getElementById("mapScaleModeSelect"),
  mapColorScaleSelect: document.getElementById("mapColorScaleSelect"),
  mapPaletteSelect: document.getElementById("mapPaletteSelect"),
  mapRenderModeSelect: document.getElementById("mapRenderModeSelect"),
  mapElevationScaleRange: document.getElementById("mapElevationScaleRange"),
  mapElevationScaleLabel: document.getElementById("mapElevationScaleLabel"),
  resetMapBtn: document.getElementById("resetMapBtn"),

  scatterXSelect: document.getElementById("scatterXSelect"),
  scatterYSelect: document.getElementById("scatterYSelect"),
  scatterColorSelect: document.getElementById("scatterColorSelect"),

  customNameInput: document.getElementById("customNameInput"),
  customFormulaInput: document.getElementById("customFormulaInput"),
  customParamsInput: document.getElementById("customParamsInput"),
  previewCustomVarBtn: document.getElementById("previewCustomVarBtn"),
  addCustomVarBtn: document.getElementById("addCustomVarBtn"),
  removeCustomVarBtn: document.getElementById("removeCustomVarBtn"),
  customVarList: document.getElementById("customVarList"),
  customVarPreview: document.getElementById("customVarPreview"),
  customVarFeedback: document.getElementById("customVarFeedback"),

  regPresetSelect: document.getElementById("regPresetSelect"),
  regScopeSelect: document.getElementById("regScopeSelect"),
  regDependentSelect: document.getElementById("regDependentSelect"),
  regPredictorSelect: document.getElementById("regPredictorSelect"),
  regPredictorsAdvanced: document.getElementById("regPredictorsAdvanced"),
  regSeModeSelect: document.getElementById("regSeModeSelect"),
  regInterceptCheck: document.getElementById("regInterceptCheck"),
  runRegBtn: document.getElementById("runRegBtn"),
  regSummary: document.getElementById("regSummary"),
  regWarnings: document.getElementById("regWarnings"),
  regResultBody: document.querySelector("#regResultTable tbody"),

  topTableBody: document.querySelector("#topTable tbody")
};

const state = {
  panel: [],
  yearSummary: [],
  validationChecks: [],
  meta: null,
  variableMeta: [],
  variableMetaMap: new Map(),
  regressionPresets: [],
  years: [],
  latestYear: null,
  numericVars: [],
  customVars: new Map(),
  selectedCountyFips: null,
  countiesGeo: [],
  deck: null,
  viewState: { ...DEFAULT_VIEW_STATE },
  mapScaleCache: new Map(),
  mapPaintRevision: 0
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

function setStatus(text, type = "info") {
  dom.loadStatus.textContent = text;
  dom.loadStatus.classList.remove("good", "bad");
  if (type === "good") dom.loadStatus.classList.add("good");
  if (type === "bad") dom.loadStatus.classList.add("bad");
}

function autoLabel(key) {
  if (state.variableMetaMap.has(key)) {
    return state.variableMetaMap.get(key).label;
  }
  if (KNOWN_LABELS[key]) return KNOWN_LABELS[key];
  return key
    .replace(/_/g, " ")
    .replace(/\b\w/g, (c) => c.toUpperCase());
}

function inferGroup(name) {
  if (/^(gop|dem|margin|swingness|gop_shift)/.test(name)) return "politics";
  if (/exposure|tariff|exports/.test(name)) return "exposure";
  if (/emp|sector|county_allocated/.test(name)) return "structure";
  return "other";
}

function allowedTransforms(name) {
  if (/share|margin|swing|rank|shift/.test(name)) return ["none", "zscore", "rank"];
  if (/exposure|exports|emp|count|tariff/.test(name)) return ["none", "log", "zscore", "rank"];
  return ["none", "zscore", "rank"];
}

function normalizeFips(value) {
  if (value === null || value === undefined) return null;
  const stripped = String(value).replace(/[^0-9]/g, "");
  if (!stripped) return null;
  return stripped.padStart(5, "0");
}

function asNumber(value) {
  if (value === null || value === undefined || value === "") return null;
  const n = Number(value);
  return Number.isFinite(n) ? n : null;
}

function finiteOrNull(value) {
  return Number.isFinite(value) ? value : null;
}

function signedLog10(value) {
  if (!Number.isFinite(value)) return null;
  if (value === 0) return 0;
  const sign = value >= 0 ? 1 : -1;
  return sign * Math.log10(Math.abs(value) + 1);
}

function mean(values) {
  if (!values.length) return null;
  return values.reduce((acc, v) => acc + v, 0) / values.length;
}

function quantile(sortedValues, p) {
  if (!sortedValues.length) return null;
  const idx = (sortedValues.length - 1) * p;
  const lo = Math.floor(idx);
  const hi = Math.ceil(idx);
  if (lo === hi) return sortedValues[lo];
  const weight = idx - lo;
  return sortedValues[lo] * (1 - weight) + sortedValues[hi] * weight;
}

function median(values) {
  if (!values.length) return null;
  const sorted = [...values].sort((a, b) => a - b);
  return quantile(sorted, 0.5);
}

function formatValue(value) {
  if (!Number.isFinite(value)) return "NA";
  const abs = Math.abs(value);
  if (abs >= 1000000) return fmt.short.format(value);
  if (abs >= 1) return fmt.num2.format(value);
  if (abs === 0) return "0";
  return fmt.num3.format(value);
}

function transformValue(value, transform) {
  const raw = finiteOrNull(value);
  if (!Number.isFinite(raw)) return null;

  if (transform === "none") return raw;
  if (transform === "log") return signedLog10(raw);

  return raw;
}

function transformSeries(values, transform) {
  const raw = values.map((v) => finiteOrNull(v));

  if (transform === "none" || transform === "log") {
    return raw.map((v) => transformValue(v, transform));
  }

  if (transform === "zscore") {
    const finite = raw.filter((v) => Number.isFinite(v));
    const m = mean(finite);
    const sd = Math.sqrt(
      finite.reduce((acc, v) => acc + (v - m) ** 2, 0) /
      Math.max(finite.length - 1, 1)
    );

    return raw.map((v) => {
      if (!Number.isFinite(v)) return null;
      if (!Number.isFinite(sd) || sd === 0) return 0;
      return (v - m) / sd;
    });
  }

  if (transform === "rank") {
    const ranked = raw.map((v, idx) => ({ v, idx })).filter((x) => Number.isFinite(x.v));
    ranked.sort((a, b) => a.v - b.v);

    const out = new Array(raw.length).fill(null);
    const denom = Math.max(ranked.length - 1, 1);
    ranked.forEach((x, order) => {
      out[x.idx] = order / denom;
    });
    return out;
  }

  return raw;
}

function paletteColor(t, paletteName) {
  const palette = PALETTES[paletteName] || PALETTES.teal_amber;
  const clamped = Math.max(0, Math.min(1, t));
  const scaled = clamped * (palette.length - 1);
  const lo = Math.floor(scaled);
  const hi = Math.min(lo + 1, palette.length - 1);
  const w = scaled - lo;

  const c0 = palette[lo];
  const c1 = palette[hi];
  return [
    Math.round(c0[0] * (1 - w) + c1[0] * w),
    Math.round(c0[1] * (1 - w) + c1[1] * w),
    Math.round(c0[2] * (1 - w) + c1[2] * w),
    220
  ];
}

function byYear(year) {
  return state.panel.filter((row) => row.year === year);
}

function selectedYear() {
  return Number(dom.yearSelect.value);
}

function selectedRows() {
  return byYear(selectedYear());
}

function optionsFromMetadata() {
  return state.variableMeta
    .filter((meta) => state.numericVars.includes(meta.name))
    .sort((a, b) => {
      if (a.group === b.group) return a.label.localeCompare(b.label);
      return a.group.localeCompare(b.group);
    })
    .map((meta) => ({
      value: meta.name,
      label: `${meta.label}`,
      group: meta.group
    }));
}

function replaceOptions(selectEl, options, preferred = null, selectedValues = []) {
  const previous = selectEl.multiple
    ? Array.from(selectEl.selectedOptions).map((opt) => opt.value)
    : selectEl.value;

  selectEl.innerHTML = "";

  options.forEach((opt) => {
    const optionEl = document.createElement("option");
    optionEl.value = opt.value;
    optionEl.textContent = opt.label;
    selectEl.appendChild(optionEl);
  });

  if (selectEl.multiple) {
    const values = selectedValues.length ? selectedValues : previous;
    const wanted = Array.isArray(values) ? values : [values];
    Array.from(selectEl.options).forEach((opt) => {
      opt.selected = wanted.includes(opt.value);
    });
  } else {
    const candidate = [preferred, previous].find((v) =>
      options.some((opt) => opt.value === v)
    );
    if (candidate) {
      selectEl.value = candidate;
    } else if (options.length > 0) {
      selectEl.value = options[0].value;
    }
  }
}

function readUrlState() {
  const p = new URLSearchParams(window.location.search);
  return {
    year: p.get("year"),
    mapVar: p.get("map"),
    mapTransform: p.get("map_t"),
    mapScaleMode: p.get("map_scale"),
    mapColorScale: p.get("map_cscale"),
    mapPalette: p.get("map_palette"),
    mapRenderMode: p.get("map_mode"),
    mapElevationScale: p.get("map_elev"),
    scatterX: p.get("sx"),
    scatterY: p.get("sy"),
    scatterColor: p.get("sc"),
    regDep: p.get("dep"),
    regPreds: p.get("preds") ? p.get("preds").split(",").filter(Boolean) : [],
    regScope: p.get("scope"),
    regSeMode: p.get("se"),
    regIntercept: p.get("intercept"),
    regAdvanced: p.get("preds_adv")
  };
}

function validOptionValue(selectEl, value) {
  return Array.from(selectEl.options).some((opt) => opt.value === value);
}

function applyUrlState(queryState) {
  if (queryState.year && validOptionValue(dom.yearSelect, queryState.year)) {
    dom.yearSelect.value = queryState.year;
  }
  if (queryState.mapVar && validOptionValue(dom.mapMetricSelect, queryState.mapVar)) {
    dom.mapMetricSelect.value = queryState.mapVar;
  }
  if (queryState.mapTransform && validOptionValue(dom.mapTransformSelect, queryState.mapTransform)) {
    dom.mapTransformSelect.value = queryState.mapTransform;
  }
  if (queryState.mapScaleMode && validOptionValue(dom.mapScaleModeSelect, queryState.mapScaleMode)) {
    dom.mapScaleModeSelect.value = queryState.mapScaleMode;
  }
  if (queryState.mapColorScale && validOptionValue(dom.mapColorScaleSelect, queryState.mapColorScale)) {
    dom.mapColorScaleSelect.value = queryState.mapColorScale;
  }
  if (queryState.mapPalette && validOptionValue(dom.mapPaletteSelect, queryState.mapPalette)) {
    dom.mapPaletteSelect.value = queryState.mapPalette;
  }
  if (queryState.mapRenderMode && validOptionValue(dom.mapRenderModeSelect, queryState.mapRenderMode)) {
    dom.mapRenderModeSelect.value = queryState.mapRenderMode;
  }
  if (queryState.mapElevationScale !== null && queryState.mapElevationScale !== "") {
    const elev = Number(queryState.mapElevationScale);
    if (Number.isFinite(elev)) {
      const min = Number(dom.mapElevationScaleRange.min || 0);
      const max = Number(dom.mapElevationScaleRange.max || 1000000);
      const bounded = Math.max(min, Math.min(max, elev));
      dom.mapElevationScaleRange.value = String(bounded);
    }
  }
  if (queryState.scatterX && validOptionValue(dom.scatterXSelect, queryState.scatterX)) {
    dom.scatterXSelect.value = queryState.scatterX;
  }
  if (queryState.scatterY && validOptionValue(dom.scatterYSelect, queryState.scatterY)) {
    dom.scatterYSelect.value = queryState.scatterY;
  }
  if (queryState.scatterColor && validOptionValue(dom.scatterColorSelect, queryState.scatterColor)) {
    dom.scatterColorSelect.value = queryState.scatterColor;
  }
  if (queryState.regDep && validOptionValue(dom.regDependentSelect, queryState.regDep)) {
    dom.regDependentSelect.value = queryState.regDep;
  }
  if (queryState.regScope && validOptionValue(dom.regScopeSelect, queryState.regScope)) {
    dom.regScopeSelect.value = queryState.regScope;
  }
  if (queryState.regSeMode && validOptionValue(dom.regSeModeSelect, queryState.regSeMode)) {
    dom.regSeModeSelect.value = queryState.regSeMode;
  }
  if (queryState.regIntercept !== null) {
    dom.regInterceptCheck.checked = queryState.regIntercept !== "false";
  }
  if (queryState.regAdvanced) {
    dom.regPredictorsAdvanced.value = queryState.regAdvanced;
  }

  if (queryState.regPreds.length > 0) {
    Array.from(dom.regPredictorSelect.options).forEach((opt) => {
      opt.selected = queryState.regPreds.includes(opt.value);
    });
  }
}

function syncUrlState() {
  const p = new URLSearchParams();
  p.set("year", dom.yearSelect.value);
  p.set("map", dom.mapMetricSelect.value);
  p.set("map_t", dom.mapTransformSelect.value);
  p.set("map_scale", dom.mapScaleModeSelect.value);
  p.set("map_cscale", dom.mapColorScaleSelect.value);
  p.set("map_palette", dom.mapPaletteSelect.value);
  p.set("map_mode", dom.mapRenderModeSelect.value);
  p.set("map_elev", dom.mapElevationScaleRange.value);
  p.set("sx", dom.scatterXSelect.value);
  p.set("sy", dom.scatterYSelect.value);
  p.set("sc", dom.scatterColorSelect.value);
  p.set("dep", dom.regDependentSelect.value);
  p.set("scope", dom.regScopeSelect.value);
  p.set("se", dom.regSeModeSelect.value);
  p.set("intercept", String(dom.regInterceptCheck.checked));

  const selectedPredictors = Array.from(dom.regPredictorSelect.selectedOptions).map((opt) => opt.value);
  if (selectedPredictors.length > 0) {
    p.set("preds", selectedPredictors.join(","));
  }

  const adv = dom.regPredictorsAdvanced.value.trim();
  if (adv) {
    p.set("preds_adv", adv);
  }

  const next = `${window.location.pathname}?${p.toString()}`;
  window.history.replaceState({}, "", next);
}

async function fetchJson(path) {
  const response = await fetch(path);
  if (!response.ok) {
    throw new Error(`Failed request ${response.status}: ${path}`);
  }
  return response.json();
}

function parsePanelRows(rawRows) {
  return rawRows.map((row) => {
    const out = { ...row };
    out.county_fips = normalizeFips(row.county_fips);
    out.county_name = row.county_name || "";
    out.state_name = row.state_name || "";
    out.year = Number(row.year);

    Object.keys(out).forEach((key) => {
      if (["county_fips", "county_name", "state_name"].includes(key)) return;
      const converted = asNumber(out[key]);
      if (converted !== null) out[key] = converted;
    });

    return out;
  });
}

function detectNumericVars(rows) {
  if (!rows.length) return [];
  const disallow = new Set(["county_fips", "county_name", "state_name"]);
  const keys = new Set();
  rows.forEach((row) => {
    Object.keys(row).forEach((key) => keys.add(key));
  });

  return [...keys]
    .filter((key) => !disallow.has(key))
    .filter((key) => rows.some((row) => Number.isFinite(row[key])));
}

function normalizeVariableMetadata(rawMeta) {
  if (!Array.isArray(rawMeta)) return [];

  const seen = new Set();
  return rawMeta
    .map((row) => {
      const name = String(row.name || "").trim();
      if (!name) return null;

      const allowed = Array.isArray(row.allowed_transforms)
        ? row.allowed_transforms.filter((x) => ["none", "log", "zscore", "rank"].includes(x))
        : allowedTransforms(name);

      return {
        name,
        label: String(row.label || autoLabel(name)),
        group: String(row.group || inferGroup(name)),
        allowed_transforms: allowed.length > 0 ? allowed : ["none", "zscore", "rank"],
        is_custom_allowed: row.is_custom_allowed !== false
      };
    })
    .filter((row) => row !== null)
    .filter((row) => {
      if (seen.has(row.name)) return false;
      seen.add(row.name);
      return true;
    });
}

function buildFallbackVariableMetadata() {
  return state.numericVars.map((name) => ({
    name,
    label: KNOWN_LABELS[name] || autoLabel(name),
    group: inferGroup(name),
    allowed_transforms: allowedTransforms(name),
    is_custom_allowed: true
  }));
}

function normalizePresets(rawPresets) {
  if (!Array.isArray(rawPresets)) return [];

  return rawPresets
    .map((preset) => {
      const predictors = Array.isArray(preset.predictors)
        ? preset.predictors.map((x) => String(x))
        : [];

      return {
        id: String(preset.id || ""),
        label: String(preset.label || ""),
        sample_scope: String(preset.sample_scope || DEFAULTS.regScope),
        dependent: String(preset.dependent || ""),
        predictors,
        se_mode: String(preset.se_mode || DEFAULTS.regSeMode),
        include_intercept: preset.include_intercept !== false
      };
    })
    .filter((preset) => preset.id && preset.label);
}

function fallbackPresets() {
  return [
    {
      id: "baseline_latest_hc1",
      label: "Latest year baseline (HC1)",
      sample_scope: "latest_year",
      dependent: DEFAULTS.regDep,
      predictors: DEFAULTS.regPreds,
      se_mode: "hc1",
      include_intercept: true
    },
    {
      id: "baseline_selected_cluster",
      label: "Selected year baseline (cluster state)",
      sample_scope: "selected_year",
      dependent: DEFAULTS.regDep,
      predictors: DEFAULTS.regPreds,
      se_mode: "cluster_state",
      include_intercept: true
    }
  ];
}

function buildSelectors() {
  const yearOptions = state.years.map((year) => ({ value: String(year), label: String(year) }));
  replaceOptions(dom.yearSelect, yearOptions, String(state.latestYear));

  const variableOptions = optionsFromMetadata();

  replaceOptions(dom.mapMetricSelect, variableOptions, DEFAULTS.mapVar);
  replaceOptions(dom.scatterXSelect, variableOptions, DEFAULTS.scatterX);
  replaceOptions(dom.scatterYSelect, variableOptions, DEFAULTS.scatterY);
  replaceOptions(dom.scatterColorSelect, variableOptions, DEFAULTS.scatterColor);
  replaceOptions(dom.regDependentSelect, variableOptions, DEFAULTS.regDep);

  replaceOptions(dom.regPredictorSelect, variableOptions, null, DEFAULTS.regPreds);

  const presetOptions = state.regressionPresets.map((preset) => ({
    value: preset.id,
    label: preset.label
  }));
  replaceOptions(dom.regPresetSelect, presetOptions, presetOptions[0]?.value || null);

  if (!dom.regPredictorsAdvanced.value.trim()) {
    dom.regPredictorsAdvanced.value = "";
  }
}

function getPresetById(id) {
  return state.regressionPresets.find((preset) => preset.id === id) || null;
}

function applyPresetToControls(presetId) {
  const preset = getPresetById(presetId);
  if (!preset) return;

  if (validOptionValue(dom.regScopeSelect, preset.sample_scope)) {
    dom.regScopeSelect.value = preset.sample_scope;
  }
  if (validOptionValue(dom.regDependentSelect, preset.dependent)) {
    dom.regDependentSelect.value = preset.dependent;
  }
  if (validOptionValue(dom.regSeModeSelect, preset.se_mode)) {
    dom.regSeModeSelect.value = preset.se_mode;
  }
  dom.regInterceptCheck.checked = Boolean(preset.include_intercept);

  Array.from(dom.regPredictorSelect.options).forEach((opt) => {
    opt.selected = preset.predictors.includes(opt.value);
  });

  dom.regPredictorsAdvanced.value = "";
}

function buildStatusBlock() {
  const checks = Array.isArray(state.validationChecks) ? state.validationChecks : [];
  const pass = checks.filter((row) => String(row.status).toLowerCase() === "pass").length;
  const fail = checks.length - pass;

  const lines = [
    `<p><strong>Generated:</strong> ${state.meta?.generated_at || "n/a"}</p>`,
    `<p><strong>Schema:</strong> ${state.meta?.schema_version || "n/a"}</p>`,
    `<p><strong>Rows:</strong> ${(state.meta?.n_panel_rows || state.panel.length).toLocaleString()}</p>`,
    `<p><strong>Latest year:</strong> ${state.latestYear || "n/a"}</p>`,
    `<p><strong>Validation:</strong> <span class="${fail === 0 ? "good" : "bad"}">${pass}/${checks.length} passing</span></p>`
  ];

  const detail = checks.slice(0, 10).map((row) => {
    const ok = String(row.status).toLowerCase() === "pass";
    return `<p class="${ok ? "good" : "bad"}">${row.check}: ${row.status}</p>`;
  });

  dom.statusBlock.innerHTML = [...lines, ...detail].join("");
}

function createLegend(minVal, maxVal, paletteName, transformLabel) {
  const palette = PALETTES[paletteName] || PALETTES.teal_amber;
  const stops = palette
    .map((color, idx) => {
      const p = (idx / Math.max(palette.length - 1, 1)) * 100;
      return `rgb(${color[0]} ${color[1]} ${color[2]}) ${p.toFixed(1)}%`;
    })
    .join(", ");

  dom.mapLegend.innerHTML = [
    `<span>${formatValue(minVal)}</span>`,
    `<span class="legend-gradient" style="background: linear-gradient(90deg, ${stops});"></span>`,
    `<span>${formatValue(maxVal)}</span>`,
    `<span>(${transformLabel})</span>`
  ].join("");
}

function createLegendWithScale(minVal, maxVal, paletteName, transformLabel, colorScaleMode, p10, p50, p90) {
  if (colorScaleMode !== "quantile") {
    createLegend(minVal, maxVal, paletteName, transformLabel);
    return;
  }

  const palette = PALETTES[paletteName] || PALETTES.teal_amber;
  const stops = palette
    .map((color, idx) => {
      const p = (idx / Math.max(palette.length - 1, 1)) * 100;
      return `rgb(${color[0]} ${color[1]} ${color[2]}) ${p.toFixed(1)}%`;
    })
    .join(", ");

  dom.mapLegend.innerHTML = [
    `<span>p10 ${formatValue(p10)}</span>`,
    `<span class="legend-gradient" style="background: linear-gradient(90deg, ${stops});"></span>`,
    `<span>p90 ${formatValue(p90)}</span>`,
    `<span>(quantile; p50 ${formatValue(p50)} | ${transformLabel})</span>`
  ].join("");
}

function lowerBound(sortedValues, target) {
  let lo = 0;
  let hi = sortedValues.length;
  while (lo < hi) {
    const mid = (lo + hi) >> 1;
    if (sortedValues[mid] < target) lo = mid + 1;
    else hi = mid;
  }
  return lo;
}

function upperBound(sortedValues, target) {
  let lo = 0;
  let hi = sortedValues.length;
  while (lo < hi) {
    const mid = (lo + hi) >> 1;
    if (sortedValues[mid] <= target) lo = mid + 1;
    else hi = mid;
  }
  return lo;
}

function updateElevationScaleLabel() {
  const raw = Number(dom.mapElevationScaleRange.value);
  const value = Number.isFinite(raw) ? raw : DEFAULTS.mapElevationScale;
  dom.mapElevationScaleLabel.textContent = `3D height scale: ${fmt.short.format(value)}`;
}

function applyMapCameraMode(renderMode) {
  if (renderMode === "extruded_3d") {
    state.viewState = {
      ...state.viewState,
      pitch: Math.max(state.viewState.pitch || 0, 42),
      bearing: Number.isFinite(state.viewState.bearing) ? state.viewState.bearing : -10,
      zoom: Math.max(state.viewState.zoom || DEFAULT_VIEW_STATE.zoom, 3.3)
    };
    return;
  }

  state.viewState = {
    ...state.viewState,
    pitch: 0,
    bearing: 0
  };
}

function mapRangeForVariable(variable, transform, scaleMode, yearRows) {
  if (scaleMode === "variable_year_auto") {
    const transformed = transformSeries(yearRows.map((row) => row[variable]), transform);
    const finite = transformed.filter((v) => Number.isFinite(v));
    if (!finite.length) return { min: 0, max: 1 };
    return { min: Math.min(...finite), max: Math.max(...finite) };
  }

  const cacheKey = `${variable}||${transform}`;
  if (state.mapScaleCache.has(cacheKey)) {
    return state.mapScaleCache.get(cacheKey);
  }

  const transformed = transformSeries(state.panel.map((row) => row[variable]), transform);
  const finite = transformed.filter((v) => Number.isFinite(v));
  const range = finite.length
    ? { min: Math.min(...finite), max: Math.max(...finite) }
    : { min: 0, max: 1 };

  state.mapScaleCache.set(cacheKey, range);
  return range;
}

function buildMap() {
  const year = selectedYear();
  const variable = dom.mapMetricSelect.value;
  const transform = dom.mapTransformSelect.value;
  const palette = dom.mapPaletteSelect.value;
  const scaleMode = dom.mapScaleModeSelect.value;
  const colorScaleMode = dom.mapColorScaleSelect.value;
  const renderMode = dom.mapRenderModeSelect.value;
  const elevationScaleRaw = Number(dom.mapElevationScaleRange.value);
  const elevationScale = Number.isFinite(elevationScaleRaw)
    ? elevationScaleRaw
    : DEFAULTS.mapElevationScale;
  updateElevationScaleLabel();

  const rows = byYear(year);
  const rowMap = new Map(rows.map((row) => [row.county_fips, row]));

  const transformed = transformSeries(rows.map((row) => row[variable]), transform);
  const valueByFips = new Map();
  rows.forEach((row, idx) => {
    valueByFips.set(row.county_fips, transformed[idx]);
  });

  const finite = transformed.filter((v) => Number.isFinite(v));
  const finiteCount = finite.length;
  const missingCount = rows.length - finiteCount;
  const minVal = finiteCount ? Math.min(...finite) : 0;
  const maxVal = finiteCount ? Math.max(...finite) : 1;
  const medVal = finiteCount ? median(finite) : null;

  const scaleRows = scaleMode === "variable_global" ? state.panel : rows;
  const transformedScale = transformSeries(scaleRows.map((row) => row[variable]), transform);
  const finiteScale = transformedScale.filter((v) => Number.isFinite(v));
  const sortedScale = [...finiteScale].sort((a, b) => a - b);
  const scale = mapRangeForVariable(variable, transform, scaleMode, rows);
  const p10 = sortedScale.length ? quantile(sortedScale, 0.1) : null;
  const p50 = sortedScale.length ? quantile(sortedScale, 0.5) : null;
  const p90 = sortedScale.length ? quantile(sortedScale, 0.9) : null;
  const mapPaintRevision = (state.mapPaintRevision += 1);
  const denom = scale.max - scale.min;

  dom.mapTitle.textContent = `County map: ${autoLabel(variable)} (${year})`;
  createLegendWithScale(scale.min, scale.max, palette, transform, colorScaleMode, p10, p50, p90);

  const colorPositionByFips = new Map();
  if (colorScaleMode === "quantile" && sortedScale.length > 0) {
    const nScale = sortedScale.length;
    rows.forEach((row, idx) => {
      const val = transformed[idx];
      if (!Number.isFinite(val)) {
        colorPositionByFips.set(row.county_fips, null);
        return;
      }
      const lo = lowerBound(sortedScale, val);
      const hi = upperBound(sortedScale, val);
      const midRank = lo === hi ? lo : (lo + hi - 1) / 2;
      const t = nScale > 1 ? midRank / (nScale - 1) : 0.5;
      colorPositionByFips.set(row.county_fips, t);
    });
  } else {
    rows.forEach((row, idx) => {
      const val = transformed[idx];
      if (!Number.isFinite(val)) {
        colorPositionByFips.set(row.county_fips, null);
        return;
      }
      colorPositionByFips.set(row.county_fips, (val - scale.min) / (denom || 1));
    });
  }

  dom.mapStats.textContent = [
    `finite ${finiteCount.toLocaleString()} / ${rows.length.toLocaleString()}`,
    `missing ${missingCount.toLocaleString()}`,
    `min ${formatValue(minVal)}`,
    `median ${formatValue(medVal)}`,
    `max ${formatValue(maxVal)}`,
    `transform ${transform}`,
    `scale ${scaleMode}`,
    `color ${colorScaleMode}`,
    `mode ${renderMode}`
  ].join(" | ");

  if (finiteCount === 0) {
    dom.mapNotice.textContent = "No finite values for this variable/year combination.";
    dom.mapNotice.className = "notice bad";
  } else if (finiteCount / Math.max(rows.length, 1) < 0.6) {
    dom.mapNotice.textContent = "Low finite-value coverage for selected map variable; interpret carefully.";
    dom.mapNotice.className = "notice bad";
  } else {
    dom.mapNotice.textContent = "Map updated.";
    dom.mapNotice.className = "notice good";
  }

  const layer = new deck.GeoJsonLayer({
    id: `county-layer-${mapPaintRevision}`,
    data: state.countiesGeo,
    filled: true,
    stroked: true,
    extruded: renderMode === "extruded_3d",
    wireframe: false,
    material: {
      ambient: 0.34,
      diffuse: 0.6,
      shininess: 36,
      specularColor: [160, 160, 160]
    },
    pickable: true,
    autoHighlight: true,
    lineWidthMinPixels: 0.15,
    getLineColor: (feature) => {
      const fips = feature.properties?.__fips;
      return fips === state.selectedCountyFips ? [15, 15, 15, 255] : [245, 245, 245, 140];
    },
    getLineWidth: (feature) => {
      const fips = feature.properties?.__fips;
      return fips === state.selectedCountyFips ? 1.2 : 0.2;
    },
    getFillColor: (feature) => {
      const fips = feature.properties?.__fips;
      const t = colorPositionByFips.get(fips);
      if (!Number.isFinite(t)) return [226, 229, 232, 150];
      return paletteColor(t, palette);
    },
    getElevation: (feature) => {
      if (renderMode !== "extruded_3d") return 0;
      const fips = feature.properties?.__fips;
      const t = colorPositionByFips.get(fips);
      if (!Number.isFinite(t)) return 0;
      return Math.max(0, t) * elevationScale;
    },
    transitions: {
      getFillColor: 260,
      getElevation: 260
    },
    updateTriggers: {
      getFillColor: [mapPaintRevision, year, variable, transform, palette, scaleMode, colorScaleMode, scale.min, scale.max, p10, p50, p90],
      getElevation: [mapPaintRevision, renderMode, elevationScale, year, variable, transform, scaleMode, colorScaleMode, scale.min, scale.max, p10, p50, p90],
      getLineColor: [state.selectedCountyFips, mapPaintRevision],
      getLineWidth: [state.selectedCountyFips, mapPaintRevision]
    },
    onClick: (info) => {
      if (!info?.object) return;
      const fips = info.object.properties?.__fips;
      state.selectedCountyFips = fips;
      const row = rowMap.get(fips);
      if (!row) {
        dom.countyDetails.textContent = `FIPS ${fips}: no row found for selected year.`;
      } else {
        dom.countyDetails.textContent = `${row.county_name || "County"}, ${row.state_name || ""} | ${autoLabel(variable)}: ${formatValue(row[variable])}`;
      }
      buildMap();
    }
  });

  const tooltip = ({ object }) => {
    if (!object) return null;
    const fips = object.properties?.__fips;
    const row = rowMap.get(fips);
    if (!row) return { text: `FIPS ${fips}: no data` };

    return {
      text:
        `${row.county_name || "County"}, ${row.state_name || ""}\n` +
        `${autoLabel(variable)}: ${formatValue(row[variable])}\n` +
        `FIPS: ${row.county_fips}`
    };
  };

  if (!state.deck) {
    state.deck = new deck.DeckGL({
      container: "map",
      controller: true,
      viewState: state.viewState,
      onViewStateChange: ({ viewState }) => {
        state.viewState = { ...viewState };
      },
      views: new deck.MapView({ repeat: false }),
      layers: [layer],
      getTooltip: tooltip
    });
  } else {
    state.deck.setProps({
      layers: [layer],
      getTooltip: tooltip,
      viewState: state.viewState,
      onViewStateChange: ({ viewState }) => {
        state.viewState = { ...viewState };
      }
    });
    state.deck.redraw(true);
  }
}

function buildScatter() {
  const year = selectedYear();
  const xKey = dom.scatterXSelect.value;
  const yKey = dom.scatterYSelect.value;
  const colorKey = dom.scatterColorSelect.value;

  const rows = byYear(year).filter((row) => Number.isFinite(row[xKey]) && Number.isFinite(row[yKey]));

  dom.scatterTitle.textContent = `Scatter: ${autoLabel(yKey)} vs ${autoLabel(xKey)} (${year})`;

  if (!rows.length) {
    Plotly.react(
      "scatter",
      [],
      {
        margin: { t: 25, r: 10, b: 40, l: 60 },
        annotations: [
          {
            text: "No finite rows for selected scatter variables.",
            showarrow: false,
            x: 0.5,
            y: 0.5,
            xref: "paper",
            yref: "paper"
          }
        ]
      },
      { responsive: true, displayModeBar: false }
    );
    return;
  }

  const colorVals = rows.map((row) => row[colorKey]).filter((v) => Number.isFinite(v));
  const cMin = colorVals.length ? Math.min(...colorVals) : 0;
  const cMax = colorVals.length ? Math.max(...colorVals) : 1;

  Plotly.react(
    "scatter",
    [
      {
        x: rows.map((row) => row[xKey]),
        y: rows.map((row) => row[yKey]),
        text: rows.map((row) => `${row.county_name || "County"}, ${row.state_name || ""}`),
        type: "scattergl",
        mode: "markers",
        marker: {
          size: 6,
          opacity: 0.65,
          color: rows.map((row) => row[colorKey]),
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
  const variable = dom.mapMetricSelect.value;
  const yearly = [...state.years]
    .sort((a, b) => a - b)
    .map((year) => {
      const values = byYear(year)
        .map((row) => row[variable])
        .filter((v) => Number.isFinite(v))
        .sort((a, b) => a - b);

      return {
        year,
        median: quantile(values, 0.5),
        p90: quantile(values, 0.9)
      };
    });

  dom.trendTitle.textContent = `Year trend: ${autoLabel(variable)}`;

  Plotly.react(
    "trend",
    [
      {
        x: yearly.map((row) => row.year),
        y: yearly.map((row) => row.median),
        type: "scatter",
        mode: "lines+markers",
        name: "Median",
        line: { color: "#116b84", width: 2 }
      },
      {
        x: yearly.map((row) => row.year),
        y: yearly.map((row) => row.p90),
        type: "scatter",
        mode: "lines+markers",
        name: "P90",
        line: { color: "#9f5d17", width: 2 }
      }
    ],
    {
      margin: { t: 30, r: 10, b: 50, l: 72 },
      xaxis: { title: "Year" },
      yaxis: { title: autoLabel(variable) },
      legend: { orientation: "h" }
    },
    { responsive: true, displayModeBar: false }
  );
}

function buildTopTable() {
  const year = selectedYear();
  const variable = dom.mapMetricSelect.value;

  const rows = byYear(year)
    .filter((row) => Number.isFinite(row[variable]))
    .sort((a, b) => b[variable] - a[variable])
    .slice(0, 75);

  dom.tableTitle.textContent = `Top counties by ${autoLabel(variable)} (${year})`;

  dom.topTableBody.innerHTML = rows
    .map((row) => `
      <tr>
        <td>${row.county_name || ""}</td>
        <td>${row.state_name || ""}</td>
        <td>${formatValue(row[variable])}</td>
        <td>${formatValue(row.county_tariff_exposure_mean)}</td>
        <td>${Number.isFinite(row.gop_share_20) ? fmt.pct1.format(row.gop_share_20) : ""}</td>
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

function parseParams(text) {
  const cleaned = text.trim();
  if (!cleaned) return {};

  let parsed;
  try {
    parsed = JSON.parse(cleaned);
  } catch (_err) {
    throw new Error("Parameter block must be valid JSON, e.g. {\"alpha\": 0.75}.");
  }

  if (parsed === null || typeof parsed !== "object" || Array.isArray(parsed)) {
    throw new Error("Parameter block must be a JSON object.");
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

function prepareCustomFormula() {
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
  if (!mathApi || typeof mathApi.compile !== "function") {
    throw new Error("Formula engine failed to load. Reload the page.");
  }

  const params = parseParams(dom.customParamsInput.value);
  const compiled = mathApi.compile(expr);

  return { name, expr, params, compiled };
}

function evaluateCustomFormula(compiled, params) {
  const values = new Array(state.panel.length).fill(null);
  const sample = [];
  let finiteCount = 0;
  let nonFiniteCount = 0;

  for (let i = 0; i < state.panel.length; i += 1) {
    const row = state.panel[i];
    const scope = { ...params };

    state.numericVars.forEach((key) => {
      const value = row[key];
      if (Number.isFinite(value)) {
        scope[key] = value;
      }
    });

    let evaluated;
    try {
      evaluated = compiled.evaluate(scope);
    } catch (err) {
      throw new Error(`Formula error at row ${i + 1}: ${err.message}`);
    }

    const number = Number(evaluated);
    if (Number.isFinite(number)) {
      values[i] = number;
      finiteCount += 1;
      if (sample.length < 5) {
        sample.push(`${row.county_name || row.county_fips} (${row.year}): ${formatValue(number)}`);
      }
    } else {
      values[i] = null;
      nonFiniteCount += 1;
    }
  }

  return {
    values,
    finiteCount,
    nonFiniteCount,
    sample
  };
}

function upsertVariableMeta(name, label, group = "custom") {
  state.variableMeta = state.variableMeta.filter((row) => row.name !== name);
  state.variableMeta.push({
    name,
    label,
    group,
    allowed_transforms: ["none", "log", "zscore", "rank"],
    is_custom_allowed: true
  });
  state.variableMetaMap = new Map(state.variableMeta.map((row) => [row.name, row]));
}

function previewCustomVariable() {
  const { name, expr, params, compiled } = prepareCustomFormula();
  const result = evaluateCustomFormula(compiled, params);
  const sampleText = result.sample.length ? result.sample.join(" | ") : "no finite sample values";

  dom.customVarPreview.textContent =
    `Preview ${name}: finite ${result.finiteCount.toLocaleString()}, non-finite ${result.nonFiniteCount.toLocaleString()} | sample: ${sampleText}`;
  dom.customVarPreview.classList.remove("bad");
  dom.customVarPreview.classList.add("good");

  return { name, expr, params, result };
}

function rebuildVariableSelectorsAfterCustom(selectedVar = null) {
  const options = optionsFromMetadata();

  replaceOptions(dom.mapMetricSelect, options, selectedVar || dom.mapMetricSelect.value);
  replaceOptions(dom.scatterXSelect, options, dom.scatterXSelect.value);
  replaceOptions(dom.scatterYSelect, options, dom.scatterYSelect.value);
  replaceOptions(dom.scatterColorSelect, options, dom.scatterColorSelect.value);
  replaceOptions(dom.regDependentSelect, options, dom.regDependentSelect.value);

  const selectedPreds = Array.from(dom.regPredictorSelect.selectedOptions).map((opt) => opt.value);
  replaceOptions(dom.regPredictorSelect, options, null, selectedPreds);
}

function refreshCustomVarList() {
  const selected = dom.customVarList.value;
  dom.customVarList.innerHTML = "";

  [...state.customVars.keys()].sort().forEach((name) => {
    const option = document.createElement("option");
    option.value = name;
    option.textContent = name;
    dom.customVarList.appendChild(option);
  });

  if (selected && state.customVars.has(selected)) {
    dom.customVarList.value = selected;
  }
}

function applyCustomVariable() {
  const { name, expr, params, result } = previewCustomVariable();

  state.panel.forEach((row, idx) => {
    row[name] = result.values[idx];
  });

  state.customVars.set(name, { expr, params });
  if (!state.numericVars.includes(name)) {
    state.numericVars.push(name);
    state.numericVars.sort();
  }

  KNOWN_LABELS[name] = `${name} (custom)`;
  upsertVariableMeta(name, `${name} (custom)`, "custom");

  refreshCustomVarList();
  rebuildVariableSelectorsAfterCustom(name);
  dom.mapMetricSelect.value = name;

  dom.customVarFeedback.textContent =
    `Saved '${name}' with ${result.finiteCount.toLocaleString()} finite values and ${result.nonFiniteCount.toLocaleString()} non-finite values.`;
  dom.customVarFeedback.classList.remove("bad");
  dom.customVarFeedback.classList.add("good");

  refreshAllViews();
  syncUrlState();
}

function removeCustomVariable() {
  const name = dom.customVarList.value;
  if (!name) {
    throw new Error("Select a custom variable to remove.");
  }
  if (!state.customVars.has(name)) {
    throw new Error(`'${name}' is not a custom variable.`);
  }

  state.panel.forEach((row) => {
    delete row[name];
  });

  state.customVars.delete(name);
  state.numericVars = state.numericVars.filter((key) => key !== name);
  delete KNOWN_LABELS[name];
  state.variableMeta = state.variableMeta.filter((row) => row.name !== name);
  state.variableMetaMap = new Map(state.variableMeta.map((row) => [row.name, row]));

  refreshCustomVarList();
  rebuildVariableSelectorsAfterCustom(DEFAULTS.mapVar);

  dom.customVarFeedback.textContent = `Removed '${name}'.`;
  dom.customVarFeedback.classList.remove("bad");
  dom.customVarFeedback.classList.add("good");

  refreshAllViews();
  syncUrlState();
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
    .map((x) => x.trim())
    .filter(Boolean);
}

function regressionPredictors() {
  const chipPredictors = Array.from(dom.regPredictorSelect.selectedOptions).map((opt) => opt.value);
  const advanced = parsePredictorList(dom.regPredictorsAdvanced.value || "");

  if (chipPredictors.length > 0) {
    return [...new Set([...chipPredictors, ...advanced])];
  }
  return [...new Set(advanced)];
}

function zeroMatrix(rows, cols) {
  return Array.from({ length: rows }, () => Array.from({ length: cols }, () => 0));
}

function transpose(matrix) {
  if (!Array.isArray(matrix) || matrix.length === 0) return [];
  const rows = matrix.length;
  const cols = matrix[0].length;
  const out = zeroMatrix(cols, rows);
  for (let i = 0; i < rows; i += 1) {
    for (let j = 0; j < cols; j += 1) {
      out[j][i] = matrix[i][j];
    }
  }
  return out;
}

function multiply(a, b) {
  if (!Array.isArray(a) || !Array.isArray(b) || a.length === 0 || b.length === 0) return null;
  const aRows = a.length;
  const aCols = a[0].length;
  const bRows = b.length;
  const bCols = b[0].length;

  if (aCols !== bRows) return null;

  const out = zeroMatrix(aRows, bCols);
  for (let i = 0; i < aRows; i += 1) {
    for (let j = 0; j < bCols; j += 1) {
      let sum = 0;
      for (let k = 0; k < aCols; k += 1) {
        sum += a[i][k] * b[k][j];
      }
      out[i][j] = sum;
    }
  }
  return out;
}

function scalarMultiply(matrix, scalar) {
  return matrix.map((row) => row.map((v) => v * scalar));
}

function addInPlace(target, addition) {
  for (let i = 0; i < target.length; i += 1) {
    for (let j = 0; j < target[0].length; j += 1) {
      target[i][j] += addition[i][j];
    }
  }
}

function outer(vecA, vecB) {
  const out = zeroMatrix(vecA.length, vecB.length);
  for (let i = 0; i < vecA.length; i += 1) {
    for (let j = 0; j < vecB.length; j += 1) {
      out[i][j] = vecA[i] * vecB[j];
    }
  }
  return out;
}

function invertMatrix(matrix) {
  const n = matrix.length;
  if (!n || matrix.some((row) => row.length !== n)) return null;

  const a = matrix.map((row, i) => [
    ...row.map((v) => (Number.isFinite(v) ? v : NaN)),
    ...Array.from({ length: n }, (_x, j) => (i === j ? 1 : 0))
  ]);

  const eps = 1e-12;

  for (let col = 0; col < n; col += 1) {
    let pivotRow = col;
    let maxAbs = Math.abs(a[col][col]);

    for (let row = col + 1; row < n; row += 1) {
      const absVal = Math.abs(a[row][col]);
      if (absVal > maxAbs) {
        maxAbs = absVal;
        pivotRow = row;
      }
    }

    if (!Number.isFinite(maxAbs) || maxAbs < eps) {
      return null;
    }

    if (pivotRow !== col) {
      const temp = a[col];
      a[col] = a[pivotRow];
      a[pivotRow] = temp;
    }

    const pivot = a[col][col];
    if (!Number.isFinite(pivot) || Math.abs(pivot) < eps) {
      return null;
    }

    for (let j = 0; j < 2 * n; j += 1) {
      a[col][j] /= pivot;
    }

    for (let row = 0; row < n; row += 1) {
      if (row === col) continue;
      const factor = a[row][col];
      for (let j = 0; j < 2 * n; j += 1) {
        a[row][j] -= factor * a[col][j];
      }
    }
  }

  const inv = a.map((row) => row.slice(n));
  const allFinite = inv.every((row) => row.every((v) => Number.isFinite(v)));
  return allFinite ? inv : null;
}

function normalCdf(x) {
  const sign = x < 0 ? -1 : 1;
  const z = Math.abs(x) / Math.sqrt(2);
  const t = 1 / (1 + 0.3275911 * z);
  const a1 = 0.254829592;
  const a2 = -0.284496736;
  const a3 = 1.421413741;
  const a4 = -1.453152027;
  const a5 = 1.061405429;
  const erf =
    1 - (((((a5 * t + a4) * t + a3) * t + a2) * t + a1) * t) * Math.exp(-z * z);
  return 0.5 * (1 + sign * erf);
}

function twoSidedPValue(tStat, dof) {
  if (!Number.isFinite(tStat)) return null;

  if (jStatApi && jStatApi.studentt && typeof jStatApi.studentt.cdf === "function" && Number.isFinite(dof) && dof > 0) {
    const cdf = jStatApi.studentt.cdf(Math.abs(tStat), dof);
    return 2 * (1 - cdf);
  }

  return 2 * (1 - normalCdf(Math.abs(tStat)));
}

function criticalValue95(dof) {
  if (jStatApi && jStatApi.studentt && typeof jStatApi.studentt.inv === "function" && Number.isFinite(dof) && dof > 0) {
    return jStatApi.studentt.inv(0.975, dof);
  }
  return 1.96;
}

function matrixAllFinite(matrix) {
  return matrix.every((row) => row.every((v) => Number.isFinite(v)));
}

function computeVcovOls(invXtX, sse, dof) {
  const sigma2 = sse / dof;
  return scalarMultiply(invXtX, sigma2);
}

function computeVcovHC1(X, residuals, invXtX, n, dof) {
  const k = invXtX.length;
  const meat = zeroMatrix(k, k);

  for (let i = 0; i < X.length; i += 1) {
    const xi = X[i];
    const e2 = residuals[i] ** 2;
    for (let a = 0; a < k; a += 1) {
      for (let b = 0; b < k; b += 1) {
        meat[a][b] += e2 * xi[a] * xi[b];
      }
    }
  }

  const left = multiply(invXtX, meat);
  const vcov = multiply(left, invXtX);
  if (!vcov) return null;
  return scalarMultiply(vcov, n / dof);
}

function computeVcovCluster(X, residuals, clusters, invXtX, n, dof) {
  const warnings = [];
  const k = invXtX.length;
  const grouped = new Map();

  for (let i = 0; i < X.length; i += 1) {
    const clusterId = clusters[i] || "__missing_cluster__";
    if (!grouped.has(clusterId)) {
      grouped.set(clusterId, Array.from({ length: k }, () => 0));
    }
    const vec = grouped.get(clusterId);
    const e = residuals[i];
    const xi = X[i];
    for (let j = 0; j < k; j += 1) {
      vec[j] += xi[j] * e;
    }
  }

  const g = grouped.size;
  if (g <= 1) {
    return {
      vcov: null,
      seModeUsed: "hc1",
      warnings: ["State clusters <= 1; fell back to HC1 robust SE."]
    };
  }

  const meat = zeroMatrix(k, k);
  grouped.forEach((vec) => {
    const contrib = outer(vec, vec);
    addInPlace(meat, contrib);
  });

  const left = multiply(invXtX, meat);
  const vcovBase = multiply(left, invXtX);
  if (!vcovBase) {
    return {
      vcov: null,
      seModeUsed: "hc1",
      warnings: ["Clustered variance matrix failed; fell back to HC1 robust SE."]
    };
  }

  const correction = (g / (g - 1)) * ((n - 1) / dof);
  const vcov = scalarMultiply(vcovBase, correction);

  if (!matrixAllFinite(vcov)) {
    warnings.push("Clustered variance matrix had non-finite entries; fell back to HC1 robust SE.");
    return {
      vcov: null,
      seModeUsed: "hc1",
      warnings
    };
  }

  return {
    vcov,
    seModeUsed: "cluster_state",
    warnings
  };
}

function runOls(dep, predictors, includeIntercept, seMode) {
  const warnings = [];

  const rows = selectedRegressionRows();
  const cleaned = rows.filter((row) => {
    if (!Number.isFinite(row[dep])) return false;
    return predictors.every((key) => Number.isFinite(row[key]));
  });

  const k = predictors.length + (includeIntercept ? 1 : 0);
  const minN = Math.max(40, k + 10);
  if (cleaned.length < minN) {
    throw new Error(`Not enough complete rows for regression (${cleaned.length}, need at least ${minN}).`);
  }

  const X = cleaned.map((row) => {
    const vals = predictors.map((key) => row[key]);
    return includeIntercept ? [1, ...vals] : vals;
  });
  const y = cleaned.map((row) => [row[dep]]);
  const names = includeIntercept ? ["(Intercept)", ...predictors] : [...predictors];

  const Xt = transpose(X);
  const XtX = multiply(Xt, X);
  if (!XtX) {
    throw new Error("Could not form X'X matrix. Check predictor dimensions.");
  }

  const invXtX = invertMatrix(XtX);
  if (!invXtX) {
    throw new Error("X'X is singular. Remove collinear predictors.");
  }

  const XtY = multiply(Xt, y);
  const beta = multiply(invXtX, XtY);

  if (!beta || !matrixAllFinite(beta)) {
    throw new Error("Regression coefficients are non-finite. Adjust predictors.");
  }

  const yHat = multiply(X, beta);
  if (!yHat || !matrixAllFinite(yHat)) {
    throw new Error("Predicted values are non-finite. Adjust predictors.");
  }

  const residuals = y.map((row, idx) => row[0] - yHat[idx][0]);
  const sse = residuals.reduce((acc, e) => acc + e * e, 0);
  const yMean = mean(y.map((row) => row[0]));
  const sst = y.reduce((acc, row) => acc + (row[0] - yMean) ** 2, 0);

  const n = cleaned.length;
  const dof = n - k;
  if (dof <= 0) {
    throw new Error(`Degrees of freedom <= 0 (n=${n}, k=${k}).`);
  }

  let seModeUsed = seMode;
  let vcov = null;

  if (seMode === "ols") {
    vcov = computeVcovOls(invXtX, sse, dof);
  } else if (seMode === "cluster_state") {
    const clusters = cleaned.map((row) => row.state_name || "");
    const clusterResult = computeVcovCluster(X, residuals, clusters, invXtX, n, dof);
    warnings.push(...clusterResult.warnings);

    if (clusterResult.vcov) {
      vcov = clusterResult.vcov;
      seModeUsed = clusterResult.seModeUsed;
    } else {
      vcov = computeVcovHC1(X, residuals, invXtX, n, dof);
      seModeUsed = "hc1";
    }
  } else {
    vcov = computeVcovHC1(X, residuals, invXtX, n, dof);
    seModeUsed = "hc1";
  }

  if (!vcov || !matrixAllFinite(vcov)) {
    throw new Error("Variance matrix is non-finite. Try fewer predictors.");
  }

  const tCrit = criticalValue95(dof);
  const coefficients = names.map((term, idx) => {
    const estimate = beta[idx][0];
    const variance = vcov[idx][idx];
    const se = variance > 0 ? Math.sqrt(variance) : 0;
    const tStat = se > 0 ? estimate / se : null;
    const pValue = tStat === null ? null : twoSidedPValue(tStat, dof);

    return {
      term,
      estimate,
      std_error: se,
      t_stat: tStat,
      p_value: pValue,
      ci_low: se > 0 ? estimate - tCrit * se : null,
      ci_high: se > 0 ? estimate + tCrit * se : null
    };
  });

  const r2 = sst > 0 ? 1 - sse / sst : null;
  const adjR2 = r2 === null ? null : 1 - (1 - r2) * ((n - 1) / dof);

  return {
    n,
    k,
    dof,
    r2,
    adj_r2: adjR2,
    se_type: seModeUsed,
    warnings,
    dependent: dep,
    predictors,
    include_intercept: includeIntercept,
    coefficients
  };
}

function renderRegressionResult(result) {
  dom.regSummary.innerHTML = [
    `<strong>Dependent:</strong> ${autoLabel(result.dependent)}`,
    `<strong>N:</strong> ${result.n.toLocaleString()}`,
    `<strong>k:</strong> ${result.k}`,
    `<strong>R2:</strong> ${result.r2 === null ? "NA" : fmt.num3.format(result.r2)}`,
    `<strong>Adj R2:</strong> ${result.adj_r2 === null ? "NA" : fmt.num3.format(result.adj_r2)}`,
    `<strong>SE:</strong> ${result.se_type}`
  ].join(" | ");

  if (result.warnings.length) {
    dom.regWarnings.textContent = result.warnings.join(" ");
    dom.regWarnings.classList.remove("good");
    dom.regWarnings.classList.add("bad");
  } else {
    dom.regWarnings.textContent = "Model estimated successfully.";
    dom.regWarnings.classList.remove("bad");
    dom.regWarnings.classList.add("good");
  }

  dom.regResultBody.innerHTML = result.coefficients
    .map((coef) => `
      <tr>
        <td>${coef.term}</td>
        <td>${formatValue(coef.estimate)}</td>
        <td>${formatValue(coef.std_error)}</td>
        <td>${coef.t_stat === null ? "NA" : fmt.num2.format(coef.t_stat)}</td>
        <td>${coef.p_value === null ? "NA" : coef.p_value < 0.001 ? "<0.001" : fmt.num3.format(coef.p_value)}</td>
        <td>${formatValue(coef.ci_low)}</td>
        <td>${formatValue(coef.ci_high)}</td>
      </tr>
    `)
    .join("");
}

function runRegressionFromUi() {
  const dep = dom.regDependentSelect.value;
  let predictors = regressionPredictors();

  if (!dep) {
    throw new Error("Choose a dependent variable.");
  }

  predictors = [...new Set(predictors)];
  predictors = predictors.filter((key) => key !== dep);

  if (!predictors.length) {
    throw new Error("Choose at least one predictor (chips or advanced list).");
  }

  const unknown = predictors.filter((key) => !state.numericVars.includes(key));
  if (unknown.length) {
    throw new Error(`Unknown predictor(s): ${unknown.join(", ")}`);
  }

  const includeIntercept = dom.regInterceptCheck.checked;
  const seMode = dom.regSeModeSelect.value;
  const result = runOls(dep, predictors, includeIntercept, seMode);
  renderRegressionResult(result);

  syncUrlState();
}

function registerHandlers() {
  [
    dom.yearSelect,
    dom.mapMetricSelect,
    dom.mapTransformSelect,
    dom.mapScaleModeSelect,
    dom.mapColorScaleSelect,
    dom.mapPaletteSelect,
    dom.mapRenderModeSelect,
    dom.scatterXSelect,
    dom.scatterYSelect,
    dom.scatterColorSelect
  ].forEach((el) => {
    el.addEventListener("change", () => {
      if (el === dom.mapRenderModeSelect) {
        applyMapCameraMode(dom.mapRenderModeSelect.value);
      }
      refreshAllViews();
      syncUrlState();
    });
  });

  dom.mapElevationScaleRange.addEventListener("input", () => {
    updateElevationScaleLabel();
    buildMap();
    syncUrlState();
  });

  dom.regPresetSelect.addEventListener("change", () => {
    applyPresetToControls(dom.regPresetSelect.value);
    syncUrlState();
  });

  dom.resetMapBtn.addEventListener("click", () => {
    state.selectedCountyFips = null;
    state.viewState = { ...DEFAULT_VIEW_STATE };
    applyMapCameraMode(dom.mapRenderModeSelect.value);
    dom.countyDetails.textContent = "Click a county to pin details here.";

    if (state.deck) {
      state.deck.setProps({ viewState: state.viewState });
    }

    buildMap();
    syncUrlState();
  });

  dom.previewCustomVarBtn.addEventListener("click", () => {
    try {
      previewCustomVariable();
      dom.customVarFeedback.textContent = "";
    } catch (err) {
      dom.customVarPreview.textContent = err.message;
      dom.customVarPreview.classList.remove("good");
      dom.customVarPreview.classList.add("bad");
    }
  });

  dom.addCustomVarBtn.addEventListener("click", () => {
    try {
      applyCustomVariable();
    } catch (err) {
      dom.customVarFeedback.textContent = err.message;
      dom.customVarFeedback.classList.remove("good");
      dom.customVarFeedback.classList.add("bad");
    }
  });

  dom.removeCustomVarBtn.addEventListener("click", () => {
    try {
      removeCustomVariable();
    } catch (err) {
      dom.customVarFeedback.textContent = err.message;
      dom.customVarFeedback.classList.remove("good");
      dom.customVarFeedback.classList.add("bad");
    }
  });

  [
    dom.regScopeSelect,
    dom.regDependentSelect,
    dom.regPredictorSelect,
    dom.regPredictorsAdvanced,
    dom.regSeModeSelect,
    dom.regInterceptCheck
  ].forEach((el) => {
    el.addEventListener("change", () => {
      syncUrlState();
    });
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
      dom.regWarnings.textContent = "";
      dom.regResultBody.innerHTML = "";
    }
  });
}

async function loadCoreData() {
  setStatus("Loading data files...");

  const [panelJson, yearSummaryJson, validationJson, metaJson, variablesJson, presetsJson] = await Promise.all([
    fetchJson(`${DATA_DIR}/panel.json`),
    fetchJson(`${DATA_DIR}/year_summary.json`).catch(() => []),
    fetchJson(`${DATA_DIR}/validation_checks.json`).catch(() => []),
    fetchJson(`${DATA_DIR}/meta.json`),
    fetchJson(`${DATA_DIR}/variables.json`).catch(() => []),
    fetchJson(`${DATA_DIR}/regression_presets.json`).catch(() => [])
  ]);

  state.panel = parsePanelRows(panelJson);
  state.yearSummary = yearSummaryJson;
  state.validationChecks = validationJson;
  state.meta = metaJson;
  state.numericVars = detectNumericVars(state.panel).sort();

  const normalizedMeta = normalizeVariableMetadata(variablesJson).filter((row) =>
    state.numericVars.includes(row.name)
  );
  state.variableMeta = normalizedMeta.length ? normalizedMeta : buildFallbackVariableMetadata();
  state.variableMetaMap = new Map(state.variableMeta.map((row) => [row.name, row]));

  const normalizedPresets = normalizePresets(presetsJson).filter((preset) =>
    state.numericVars.includes(preset.dependent) &&
    preset.predictors.every((key) => state.numericVars.includes(key))
  );
  state.regressionPresets = normalizedPresets.length ? normalizedPresets : fallbackPresets();

  state.years = [...new Set(state.panel.map((row) => row.year).filter((year) => Number.isFinite(year)))].sort((a, b) => a - b);
  state.latestYear = Number.isFinite(metaJson.latest_year)
    ? Number(metaJson.latest_year)
    : Math.max(...state.years);

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
      return fips && !EXCLUDE_STATEFP.has(fips.slice(0, 2));
    });
}

async function init() {
  try {
    await loadCoreData();

    buildSelectors();

    const queryState = readUrlState();
    applyUrlState(queryState);
    applyMapCameraMode(dom.mapRenderModeSelect.value);
    updateElevationScaleLabel();

    if (dom.regPresetSelect.options.length > 0 && !queryState.dep) {
      applyPresetToControls(dom.regPresetSelect.value);
    }

    buildStatusBlock();
    refreshCustomVarList();
    refreshAllViews();
    registerHandlers();
    syncUrlState();

    setStatus("Loaded", "good");

    try {
      runRegressionFromUi();
    } catch (_err) {
      dom.regSummary.textContent = "Ready. Select controls and click Run regression.";
    }
  } catch (err) {
    console.error(err);
    setStatus("Load failed", "bad");
    dom.statusBlock.innerHTML =
      `<p class="bad">${err.message}</p>` +
      `<p>Run <code>source('code/build_site_data.R')</code> and redeploy <code>site/</code>.</p>`;
  }
}

init();
