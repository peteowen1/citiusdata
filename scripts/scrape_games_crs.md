# Scraping a Games Competition Results System

One-off browser recipe for Games that run a Microplus CRS (Glasgow 2026, and
the same system is used across many multi-sport Games).

**When to use this.** Only when the results are not available through a
federation feed. World Athletics ingests Commonwealth athletics after the fact,
so athletics should come from `competition_results()` — see
`watch_glasgow2026.R`. Swimming has no such route (World Aquatics does not
sanction the Commonwealth Games), so the CRS is the only source.

## Why not the API

The CRS exposes a full REST API — endpoints listed at
`https://crs-cg2026.glasgow2026.com/assets/api_config.json` — but every call
returns `401` without a bearer token the Angular app obtains at runtime, and
Cloudflare challenges non-browser clients. The app renders everything publicly,
so the rendered pages are scraped instead. Do not attempt to extract the token.

## Recipe

Open the results app in Chrome, then run each step in the page console.

### 1. Collect result routes

```js
const days = ['2026-07-27','2026-07-28','2026-07-29','2026-07-30','2026-07-31','2026-08-01'];
const sleep = ms => new Promise(r => setTimeout(r, ms));
const found = new Set();
for (const d of days) {
  location.hash = '#/athletic-sports-schedule/ATH/' + d;   // SWM for swimming
  await sleep(2500);
  document.querySelectorAll('a').forEach(a => {
    const h = a.getAttribute('href') || '';
    if (h.indexOf('athletic-result') !== -1) found.add(h.split('#/')[1]);
  });
}
window.__links = Array.from(found); window.__links.length
```

### 2. Visit each route and capture the rendered table

Hash changes re-render without a page load, so the whole sweep runs in-page.
Batch in groups of ~22 to stay within tool timeouts.

```js
window.__out = window.__out || {};
for (const route of window.__links.filter(l => !(l in window.__out)).slice(0, 22)) {
  location.hash = '#/' + route;
  await sleep(1700);
  const m = document.querySelector('main');
  window.__out[route] = m ? m.innerText.replace(/\n{2,}/g, '\n').trim() : '';
}
Object.keys(window.__out).length
```

### 3. Parse to compact JSON

The results table is **tab-separated** and each athlete spans three lines
(`rank⇥lane`, then nation, then `name⇥RT⇥time⇥behind`). Splitting on whitespace
silently loses most rows — split on `\t`.

```js
function parse(route, txt) {
  const L = txt.split('\n');
  const meta = (L.find(l => /\d{1,2} \w{3} \d{4}/.test(l)) || '').trim();
  const title = (L.find(l => /^[A-Z][A-Z'’\- 0-9]{5,}$/.test(l.trim()) &&
    !/^(SWIMMING|ATHLETICS|RESULTS|STARTLIST|REPORTS|SCHEDULE)/.test(l.trim())) || '').trim();
  const rows = []; let heat = '';
  for (let i = 0; i < L.length; i++) {
    const s = L[i].trim();
    const hm = s.match(/^(Heat|Semifinal|Semi-Final|Final|Round)\s*(\d*)\s*-\s*\w+/i);
    if (hm) { heat = s.split('-')[0].trim(); continue; }
    const c = L[i].split('\t').map(x => x.trim());
    if (!/^(\d{1,2}|DSQ|DNS|DNF|DQ)$/.test(c[0]) || !/^\d{1,2}$/.test(c[1] || '')) continue;
    const nat = (L[i+1] || '').split('\t').map(x => x.trim()).filter(Boolean)[0] || '';
    const d = (L[i+2] || '').split('\t').map(x => x.trim()).filter(Boolean);
    if (d.length < 2) continue;
    const isRT = /^\d\.\d{2}$/.test(d[1]);
    rows.push({r: c[0], ln: c[1], nat, nm: d[0],
               rt: isRT ? d[1] : '', tm: isRT ? (d[2] || '') : (d[1] || ''), ht: heat});
  }
  return {rt: route, w: meta, t: title, rows};
}
const parsed = window.__links.map(l => parse(l, window.__out[l]));
const pages = [], rows = [];
parsed.forEach(p => {
  if (!p.rows.length) return;
  const pi = pages.length;
  pages.push([p.rt, p.w, p.t]);
  p.rows.forEach(r => rows.push([pi, r.ht, r.r, r.ln, r.nat, r.nm, r.rt, r.tm]));
});
window.__flat = JSON.stringify({p: pages, r: rows});
```

### 4. Download and parse in R

```js
const b = new Blob([window.__flat], {type: 'application/json'});
const a = document.createElement('a');
a.href = URL.createObjectURL(b);
a.download = 'glasgow2026_athletics.json';
document.body.appendChild(a); a.click();
```

```r
res <- parse_crs_export("citiusdata/data/glasgow2026_athletics.json")
```

## Known gaps

- **Para events do not parse.** Sport-class rows (`S9`, `S14`, `SM10`) carry an
  extra column that shifts the layout. 16 of 65 swimming pages were affected.
  Out of scope for the current registry.
- **DSQ rows are dropped at capture.** The rendered rank cell reads
  `Disqualified` on a separate line from `DSQ`, so step 3's rank pattern misses
  them. `parse_crs_export()` handles a `DSQ` rank correctly if one reaches it —
  the loss is in the browser step. Fix before relying on no-mark rates from CRS.
- **Relays resolve to `NA` `event_id`** by design; they are team events and need
  separate modelling. Rows are retained, not dropped.
