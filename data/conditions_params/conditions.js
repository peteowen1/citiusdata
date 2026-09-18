// Live conditions adjuster — a line-for-line port of citius::adjust_conditions()
// and citius::race_shock() (citius/R/conditions.R). Reads the parameters
// exported by export_conditions_params.R (_all.json, keyed by event_id).
// Units: perf-log. perf = orientation * log(mark); positive adjustment = the
// condition helped, so cleaned = perf - wind - venue - indoor - shock.
//
// Parity with R is checked by scripts/diagnostics/check_conditions_js_parity.R.
(function (root) {
  function interp(grid, curve, x) {
    if (!Number.isFinite(x)) return 0
    const lo = grid[0], hi = grid[grid.length - 1]
    if (x <= lo) return curve[0]
    if (x >= hi) return curve[curve.length - 1]
    let i = 1
    while (grid[i] < x) i++
    const t = (x - grid[i - 1]) / (grid[i] - grid[i - 1])
    return curve[i - 1] + t * (curve[i] - curve[i - 1])
  }

  // params: one event's entry from _all.json. wind: m/s or null. alt_m: metres or null.
  function adjustConditions(params, wind, alt_m, indoor) {
    const wind_adj = params.wind ? interp(params.wind.grid, params.wind.curve, wind) : 0
    const venue_adj = interp(params.altitude.grid_m, params.altitude.curve,
                             Number.isFinite(alt_m) ? Math.max(alt_m, 0) : NaN)
    const indoor_adj = params.has_indoor && indoor === true ? params.indoor_coef : 0
    return { wind_adj, venue_adj, indoor_adj }
  }

  // resid: array of (cleaned perf - expected perf), one per athlete; null/NaN
  // for athletes with no expectation. Returns the shrunk, winsorised field mean.
  function raceShock(resid, var_race, var_resid, cap_sd = 3) {
    const r = resid.filter(Number.isFinite)
    const n = r.length
    if (!n || !Number.isFinite(var_race) || !Number.isFinite(var_resid)) return 0
    const cap = cap_sd * Math.sqrt(var_resid)
    const mean = r.reduce((s, v) => s + Math.min(Math.max(v, -cap), cap), 0) / n
    return mean * n * var_race / (n * var_race + var_resid)
  }

  // Leave-one-out: each athlete's shock from the OTHER athletes' residuals, so
  // their own surprise is never evidence the day was fast. Port of
  // citius::race_shock_loo(). Returns one value per input.
  function raceShockLoo(resid, var_race, var_resid, cap_sd = 3) {
    if (!Number.isFinite(var_race) || !Number.isFinite(var_resid)) return resid.map(() => 0)
    const cap = cap_sd * Math.sqrt(var_resid)
    const ok = resid.map(Number.isFinite)
    const r = resid.map((v, i) => ok[i] ? Math.min(Math.max(v, -cap), cap) : 0)
    const n = ok.filter(Boolean).length, s = r.reduce((a, b) => a + b, 0)
    return resid.map((_, i) => {
      const n_i = n - (ok[i] ? 1 : 0), s_i = s - r[i]
      return n_i > 0 ? (s_i / n_i) * n_i * var_race / (n_i * var_race + var_resid) : 0
    })
  }

  // Full race: rows = [{perf, wind, alt_m, indoor, expected}], expected in perf
  // units or null. Returns rows with the components and adj_perf attached.
  // loo = true (the default, matching build_adjusted_marks_v2.R) gives each
  // athlete the field's shock excluding themselves.
  function adjustRace(params, rows, var_resid = params.var_resid, loo = true) {
    const out = rows.map(r => {
      const a = adjustConditions(params, r.wind, r.alt_m, r.indoor)
      const cleaned = r.perf - a.wind_adj - a.venue_adj - a.indoor_adj
      return { ...r, ...a, cleaned, resid: Number.isFinite(r.expected) ? cleaned - r.expected : NaN }
    })
    const resid = out.map(r => r.resid)
    const shock = loo ? raceShockLoo(resid, params.var_race, var_resid)
                      : out.map(() => raceShock(resid, params.var_race, var_resid))
    return out.map((r, i) => ({ ...r, race_shock: shock[i], adj_perf: r.cleaned - shock[i] }))
  }

  const api = { interp, adjustConditions, raceShock, raceShockLoo, adjustRace }
  if (typeof module !== "undefined" && module.exports) module.exports = api
  else root.conditions = api
})(typeof window !== "undefined" ? window : globalThis)
