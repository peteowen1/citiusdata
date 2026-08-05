# Renders the Glasgow 2026 card -- our top five per event against who actually
# medalled -- as one self-contained HTML page with sport / event / sex filters.
#
# Reads `glasgow.json` from the directory given as argv[1] (default: cwd) and
# writes glasgow2026.html beside it.
#
# This lived in a session scratchpad until 2026-08-05, which meant every filter
# and layout change would have been lost when that session's temp dir went.
import json, io, html, os, sys
S = sys.argv[1] if len(sys.argv) > 1 else os.getcwd()
src = os.path.join(S, "glasgow.json")
if not os.path.exists(src):
    raise SystemExit("no glasgow.json in %s -- run the card export first" % S)
d = json.load(io.open(src, encoding="utf-8"))
meta, events = d["meta"], d["events"]
def esc(x): return html.escape(str(x)) if x is not None else ""

# Ordered by how confident our top pick was: the read runs from our strongest
# calls down to the events we had no real opinion on.
events.sort(key=lambda e: -(e["predicted"][0]["p_gold"] if e["predicted"] else 0))

def status(e):
    w = [a for a in e["actual"] if a["place"] == 1]
    if not w: return ("unrated", "no final")
    r = w[0].get("pred_rank")
    if r is None: return ("unrated", "winner unrated")
    if r == 1: return ("called", "called it")
    if r <= 5: return ("near", "winner was our #%d" % r)
    return ("missed", "winner was our #%d" % r)
MEDAL = {1: "gold", 2: "silver", 3: "bronze"}

blocks = []
for e in events:
    cls, label = status(e)
    preds = "".join(
        '<li><span class="pos">{r}</span><span class="nm">{n}</span>'
        '<span class="cc">{c}</span><span class="mk">{m}</span>'
        '<span class="pg">{g}%</span></li>'.format(
            r=p["rank"], n=esc(p["athlete"]), c=esc(p["country"]),
            m=esc(p["mark"] or "—"), g=p["p_gold"]) for p in e["predicted"])
    acts = []
    for a in e["actual"]:
        r = a.get("pred_rank")
        if r is None: tag, tc = "unrated", "t-un"
        elif r <= 5:  tag, tc = "our #%d" % r, "t-hit"
        else:         tag, tc = "our #%d" % r, "t-miss"
        acts.append(
            '<li class="m-{md}"><span class="pl">{p}</span><span class="nm">{n}</span>'
            '<span class="cc">{c}</span><span class="mk">{m}</span>'
            '<span class="tag {tc}">{t}</span></li>'.format(
                md=MEDAL.get(a["place"], "x"), p=a["place"], n=esc(a["athlete"]),
                c=esc(a["country"]) or "&nbsp;", m=esc(a["mark"] or "—"), t=tag, tc=tc))
    sex = "W" if e["event"].endswith("-W") else ("M" if e["event"].endswith("-M") else "X")
    blocks.append("""<article class="ev" data-sport="{sport}" data-sex="{sex}" data-ev="{evid}" data-name="{lname}" data-status="{cls}">
  <header class="ev-h">
    <h2>{name}</h2>
    <span class="sp sp--{spc}">{sport}</span>
    <div class="ev-meta"><span class="fld">{field} entered</span><span class="st st--{cls}">{label}</span></div>
  </header>
  <div class="ev-b">
    <section class="col"><h3>Our five, before a shot was fired</h3><ol class="pred">{preds}</ol></section>
    <section class="col"><h3>The podium</h3><ol class="act">{acts}</ol></section>
  </div>
</article>""".format(name=esc(e["name"]), sport=esc(e["sport"]),
                     spc=e["sport"][:3].lower(), field=e["field"], cls=cls,
                     label=label, preds=preds, acts="".join(acts),
                     sex=sex, evid=esc(e["event"]),
                     lname=esc(e["name"]).lower()))


# ---- sticky filter bar -------------------------------------------------------
# Built outside the page template so the JavaScript braces do not have to be
# doubled for str.format, which is where this kind of thing usually breaks.
opts = "".join(
    '<option value="{ev}">{nm}</option>'.format(ev=esc(e["event"]), nm=esc(e["name"]))
    for e in sorted(events, key=lambda x: x["name"]))
FILTERS = """
  <div class="bar" id="bar">
    <div class="bar-head">
      <button type="button" id="ftog" class="ftog" aria-expanded="true" aria-controls="fbody">
        <span class="chev" aria-hidden="true"></span><span>Filters</span>
      </button>
      <span class="fsum" id="fsum"></span>
      <output id="count" aria-live="polite"></output>
    </div>
    <div class="bar-body" id="fbody">
      <div class="bar-in">
        <div class="fg" role="group" aria-label="Sport">
          <span class="fl">Sport</span>
          <div class="seg" data-filter="sport">
            <button type="button" class="on" data-v="all">All</button>
            <button type="button" data-v="Athletics">Athletics</button>
            <button type="button" data-v="Swimming">Swimming</button>
          </div>
        </div>
        <div class="fg" role="group" aria-label="Sex">
          <span class="fl">Sex</span>
          <div class="seg" data-filter="sex">
            <button type="button" class="on" data-v="all">All</button>
            <button type="button" data-v="M">Men</button>
            <button type="button" data-v="W">Women</button>
          </div>
        </div>
        <div class="fg fg-grow">
          <label class="fl" for="evsel">Event</label>
          <select id="evsel"><option value="all">All events</option>__OPTS__</select>
        </div>
        <div class="fg">
          <span class="fl" aria-hidden="true">&nbsp;</span>
          <button type="button" id="reset" class="rst">Reset</button>
        </div>
      </div>
    </div>
  </div>
""".replace("__OPTS__", opts)

FILTER_JS = """
<script>
(function () {
  var state = {sport: 'all', sex: 'all', ev: 'all'};
  var evs = Array.prototype.slice.call(document.querySelectorAll('.ev'));
  var sel = document.getElementById('evsel');
  var count = document.getElementById('count');
  var bar = document.getElementById('bar');
  var sum = document.getElementById('fsum');
  var tog = document.getElementById('ftog');
  var body = document.getElementById('fbody');

  // The event list is rebuilt from whatever sport/sex is active, so the dropdown
  // can never offer a choice that yields nothing.
  function syncOptions() {
    var cur = sel.value;
    var avail = evs.filter(function (el) {
      return (state.sport === 'all' || el.dataset.sport === state.sport) &&
             (state.sex === 'all' || el.dataset.sex === state.sex);
    });
    var html = '<option value="all">All events</option>';
    avail.slice().sort(function (a, b) {
      return a.dataset.name < b.dataset.name ? -1 : 1;
    }).forEach(function (el) {
      html += '<option value="' + el.dataset.ev + '">' +
              el.querySelector('h2').textContent + '</option>';
    });
    sel.innerHTML = html;
    // Keep the chosen event if it survives the narrowing; otherwise fall back.
    if (cur !== 'all' && avail.some(function (el) { return el.dataset.ev === cur; })) {
      sel.value = cur;
    } else {
      sel.value = 'all';
      state.ev = 'all';
    }
  }

  function apply() {
    var shown = 0;
    evs.forEach(function (el) {
      var ok = (state.sport === 'all' || el.dataset.sport === state.sport) &&
               (state.sex === 'all' || el.dataset.sex === state.sex) &&
               (state.ev === 'all' || el.dataset.ev === state.ev);
      el.hidden = !ok;
      if (ok) shown++;
    });
    count.textContent = shown === evs.length
      ? evs.length + ' events'
      : shown + ' of ' + evs.length + ' events';

    // Collapsing must not hide what is active, so the head carries a summary.
    var parts = [];
    if (state.sport !== 'all') parts.push(state.sport);
    if (state.sex !== 'all') parts.push(state.sex === 'M' ? 'Men' : 'Women');
    if (state.ev !== 'all') {
      var sels = sel.options[sel.selectedIndex];
      if (sels) parts.push(sels.textContent);
    }
    sum.textContent = parts.length ? parts.join(' · ') : 'All sports, all events';
    sum.classList.toggle('active', parts.length > 0);
    var empty = document.getElementById('empty');
    if (empty) empty.hidden = shown !== 0;
  }

  document.querySelectorAll('.seg').forEach(function (seg) {
    seg.addEventListener('click', function (e) {
      var b = e.target.closest('button');
      if (!b) return;
      seg.querySelectorAll('button').forEach(function (x) {
        x.classList.toggle('on', x === b);
        x.setAttribute('aria-pressed', x === b ? 'true' : 'false');
      });
      state[seg.dataset.filter] = b.dataset.v;
      syncOptions();
      apply();
    });
  });
  sel.addEventListener('change', function () { state.ev = sel.value; apply(); });
  document.getElementById('reset').addEventListener('click', function () {
    state = {sport: 'all', sex: 'all', ev: 'all'};
    document.querySelectorAll('.seg').forEach(function (seg) {
      seg.querySelectorAll('button').forEach(function (x) {
        x.classList.toggle('on', x.dataset.v === 'all');
        x.setAttribute('aria-pressed', x.dataset.v === 'all' ? 'true' : 'false');
      });
    });
    syncOptions();
    apply();
    bar.scrollIntoView({block: 'start', behavior: 'smooth'});
  });

  // Shadow only once the bar has actually stuck, so it reads as lifted rather
  // than as a permanent border.
  // Sentinel sits in normal flow directly above the bar, so it leaves the
  // viewport at exactly the moment the bar sticks. Positioning it absolutely at
  // the document top instead would fire on the first pixel of scroll, long
  // before the bar was actually stuck.
  var probe = document.createElement('div');
  probe.setAttribute('aria-hidden', 'true');
  probe.style.cssText = 'height:1px;margin:0';
  bar.parentNode.insertBefore(probe, bar);
  if ('IntersectionObserver' in window) {
    new IntersectionObserver(function (en) {
      bar.classList.toggle('stuck', !en[0].isIntersecting);
    }).observe(probe);
  }

  function setOpen(open) {
    body.hidden = !open;
    tog.setAttribute('aria-expanded', open ? 'true' : 'false');
  }
  tog.addEventListener('click', function () {
    setOpen(body.hidden);
  });

  // Collapsed by default on a narrow screen, where the expanded bar costs about
  // half the viewport; open by default where there is room for it.
  setOpen(window.matchMedia('(min-width: 700px)').matches);

  syncOptions();
  apply();
})();
</script>
"""

pct = lambda a, b: round(100.0 * a / b) if b else 0
page = """<title>Glasgow 2026 — what we called, what happened</title>
<style>
:root{{
  --paper:#F4F6F7;--card:#FFFFFF;--ink:#131A1E;--ink-2:#4A585F;--ink-3:#7D8B92;
  --line:#DCE3E5;--line-2:#EDF1F2;--accent:#0C6E70;--accent-soft:#E2EFEF;
  --gold:#C08A2E;--silver:#8C959C;--bronze:#9C6638;--hit:#2C7A5B;--miss:#A8443F;
  --swim:#2F6BA8;--swim-soft:#E4EDF6;
  --serif:"Iowan Old Style","Palatino Linotype",Palatino,"Book Antiqua",Georgia,serif;
  --sans:ui-sans-serif,system-ui,"Segoe UI",Roboto,Helvetica,Arial,sans-serif;
  --mono:ui-monospace,"SF Mono","Cascadia Mono","Segoe UI Mono",Consolas,monospace;
}}
@media (prefers-color-scheme:dark){{:root{{
  --paper:#0F1519;--card:#161E23;--ink:#E6EDEF;--ink-2:#9FB0B7;--ink-3:#6F8087;
  --line:#26333A;--line-2:#1D272C;--accent:#4FB3B0;--accent-soft:#14282A;
  --gold:#D6A24A;--silver:#A8B2B8;--bronze:#B57F4E;--hit:#4BA37C;--miss:#D0716B;
  --swim:#6FA3D6;--swim-soft:#16222F;}}}}
:root[data-theme="dark"]{{
  --paper:#0F1519;--card:#161E23;--ink:#E6EDEF;--ink-2:#9FB0B7;--ink-3:#6F8087;
  --line:#26333A;--line-2:#1D272C;--accent:#4FB3B0;--accent-soft:#14282A;
  --gold:#D6A24A;--silver:#A8B2B8;--bronze:#B57F4E;--hit:#4BA37C;--miss:#D0716B;
  --swim:#6FA3D6;--swim-soft:#16222F;}}
:root[data-theme="light"]{{
  --paper:#F4F6F7;--card:#FFFFFF;--ink:#131A1E;--ink-2:#4A585F;--ink-3:#7D8B92;
  --line:#DCE3E5;--line-2:#EDF1F2;--accent:#0C6E70;--accent-soft:#E2EFEF;
  --gold:#C08A2E;--silver:#8C959C;--bronze:#9C6638;--hit:#2C7A5B;--miss:#A8443F;
  --swim:#2F6BA8;--swim-soft:#E4EDF6;}}
*{{box-sizing:border-box}}
body{{margin:0;background:var(--paper);color:var(--ink);font-family:var(--sans);
  font-size:16px;line-height:1.55;-webkit-font-smoothing:antialiased}}
.wrap{{max-width:1080px;margin:0 auto;padding:clamp(24px,5vw,64px) clamp(16px,4vw,32px) 96px}}
.eyebrow{{font-size:12px;letter-spacing:.14em;text-transform:uppercase;color:var(--accent);
  font-weight:600;margin:0 0 14px}}
h1{{font-family:var(--serif);font-weight:600;font-size:clamp(30px,5vw,50px);line-height:1.08;
  margin:0 0 16px;text-wrap:balance;letter-spacing:-.01em}}
.lede{{font-size:clamp(16px,2vw,18.5px);color:var(--ink-2);max-width:62ch;margin:0}}
.stats{{display:grid;gap:1px;background:var(--line);border:1px solid var(--line);
  border-radius:3px;overflow:hidden;margin:34px 0 0;
  grid-template-columns:repeat(auto-fit,minmax(158px,1fr))}}
.stat{{background:var(--card);padding:18px}}
.stat b{{display:block;font-family:var(--mono);font-variant-numeric:tabular-nums;
  font-size:27px;font-weight:600;letter-spacing:-.02em;line-height:1.1}}
.stat span{{display:block;font-size:12.5px;color:var(--ink-3);margin-top:5px;line-height:1.35}}
.note{{border-left:2px solid var(--accent);background:var(--card);padding:16px 18px;
  margin:24px 0 0;font-size:13.5px;color:var(--ink-2);border-radius:0 3px 3px 0}}
.note p{{margin:0 0 9px}} .note p:last-child{{margin:0}}
.note b{{color:var(--ink);font-weight:600}}
.rule{{border:0;border-top:1px solid var(--line);margin:44px 0 0}}
.ev{{border-bottom:1px solid var(--line);padding:30px 0}}
.ev-h{{display:flex;flex-wrap:wrap;align-items:baseline;gap:10px 12px;margin-bottom:18px}}
.ev-h h2{{font-family:var(--serif);font-size:24px;font-weight:600;margin:0;letter-spacing:-.01em}}
.sp{{font-size:10.5px;font-weight:600;letter-spacing:.09em;text-transform:uppercase;
  padding:2px 7px;border-radius:2px}}
.sp--ath{{background:var(--accent-soft);color:var(--accent)}}
.sp--swi{{background:var(--swim-soft);color:var(--swim)}}
.ev-meta{{display:flex;align-items:center;gap:10px;margin-left:auto}}
.fld{{font-size:12px;color:var(--ink-3);font-family:var(--mono)}}
.st{{font-size:11.5px;font-weight:600;letter-spacing:.05em;text-transform:uppercase;
  padding:3px 9px;border-radius:2px;white-space:nowrap}}
.st--called{{background:var(--hit);color:#fff}}
.st--near{{background:var(--accent-soft);color:var(--accent);box-shadow:inset 0 0 0 1px var(--accent)}}
.st--missed{{background:transparent;color:var(--miss);box-shadow:inset 0 0 0 1px var(--miss)}}
.st--unrated{{background:transparent;color:var(--ink-3);box-shadow:inset 0 0 0 1px var(--line)}}
.ev-b{{display:grid;gap:26px;grid-template-columns:1fr}}
@media(min-width:800px){{.ev-b{{grid-template-columns:1.15fr 1fr;gap:38px}}}}
.col h3{{font-size:11.5px;letter-spacing:.1em;text-transform:uppercase;color:var(--ink-3);
  font-weight:600;margin:0 0 10px;padding-bottom:8px;border-bottom:1px solid var(--line-2)}}
ol{{list-style:none;margin:0;padding:0}}
ol li{{display:grid;align-items:baseline;gap:10px;padding:7px 0;
  border-bottom:1px solid var(--line-2);font-size:14.5px;
  grid-template-columns:20px 1fr auto auto auto}}
ol li:last-child{{border-bottom:0}}
.pos,.pl{{font-family:var(--mono);font-size:12px;color:var(--ink-3);font-variant-numeric:tabular-nums}}
.nm{{min-width:0;overflow-wrap:anywhere}}
.cc{{font-family:var(--mono);font-size:11.5px;color:var(--ink-3);letter-spacing:.04em}}
.mk{{font-family:var(--mono);font-variant-numeric:tabular-nums;font-size:13px;color:var(--ink-2)}}
.pg{{font-family:var(--mono);font-variant-numeric:tabular-nums;font-size:13px;
  color:var(--accent);font-weight:600;min-width:52px;text-align:right}}
.m-gold .pl{{color:var(--gold);font-weight:700}}
.m-silver .pl{{color:var(--silver);font-weight:700}}
.m-bronze .pl{{color:var(--bronze);font-weight:700}}
.m-gold .nm{{font-weight:600}}
.tag{{font-family:var(--mono);font-size:11px;padding:2px 7px;border-radius:2px;
  white-space:nowrap;min-width:62px;text-align:center}}
.t-hit{{background:var(--accent-soft);color:var(--accent)}}
.t-miss{{background:transparent;color:var(--miss);box-shadow:inset 0 0 0 1px var(--line)}}
.t-un{{background:transparent;color:var(--ink-3);box-shadow:inset 0 0 0 1px var(--line)}}
.bar{{position:sticky;top:0;z-index:20;background:var(--paper);
  margin:0 0 8px;padding:12px 0 11px;border-bottom:1px solid transparent;
  transition:border-color .18s ease, box-shadow .18s ease}}
.bar.stuck{{border-bottom-color:var(--line);box-shadow:0 6px 18px -12px rgba(0,0,0,.45)}}
.bar-head{{display:flex;align-items:center;gap:10px 12px;flex-wrap:wrap}}
.ftog{{display:inline-flex;align-items:center;gap:7px;font:inherit;font-size:12.5px;
  font-weight:600;letter-spacing:.05em;text-transform:uppercase;cursor:pointer;
  padding:6px 11px;border:1px solid var(--line);border-radius:3px;
  background:var(--card);color:var(--ink-2)}}
.ftog:hover{{color:var(--ink);border-color:var(--ink-3)}}
.chev{{width:0;height:0;border-left:4.5px solid transparent;border-right:4.5px solid transparent;
  border-top:5.5px solid currentColor;transition:transform .18s ease}}
.ftog[aria-expanded="false"] .chev{{transform:rotate(-90deg)}}
.fsum{{font-size:13px;color:var(--ink-3);min-width:0;overflow:hidden;
  text-overflow:ellipsis;white-space:nowrap}}
.fsum.active{{color:var(--accent);font-weight:600}}
.bar-head #count{{margin-left:auto}}
.bar-body{{padding-top:13px}}
.bar-body[hidden]{{display:none}}
.bar-in{{display:flex;flex-wrap:wrap;align-items:flex-end;gap:14px 20px}}
.fg{{display:flex;flex-direction:column;gap:6px;min-width:0}}
.fg-grow{{flex:1 1 210px}}
.fl{{font-size:10.5px;letter-spacing:.11em;text-transform:uppercase;
  color:var(--ink-3);font-weight:600}}
.seg{{display:inline-flex;border:1px solid var(--line);border-radius:3px;overflow:hidden;
  background:var(--card)}}
.seg button{{font:inherit;font-size:13px;padding:6px 13px;border:0;cursor:pointer;
  background:transparent;color:var(--ink-2);border-right:1px solid var(--line)}}
.seg button:last-child{{border-right:0}}
.seg button:hover{{color:var(--ink);background:var(--accent-soft)}}
.seg button.on{{background:var(--accent);color:#fff}}
/* The dark accent is light, so white-on-accent loses contrast; flip the label. */
@media (prefers-color-scheme:dark){{.seg button.on{{color:#0F1519}}}}
:root[data-theme="dark"] .seg button.on{{color:#0F1519}}
:root[data-theme="light"] .seg button.on{{color:#fff}}
#evsel{{font:inherit;font-size:13.5px;padding:6px 9px;width:100%;
  background:var(--card);color:var(--ink);border:1px solid var(--line);border-radius:3px}}
#count{{font-family:var(--mono);font-size:12.5px;color:var(--ink-3);
  font-variant-numeric:tabular-nums;white-space:nowrap}}
.rst{{font:inherit;font-size:12.5px;padding:6px 12px;cursor:pointer;
  background:transparent;color:var(--ink-2);border:1px solid var(--line);border-radius:3px}}
.rst:hover{{color:var(--ink);border-color:var(--ink-3)}}
.seg button:focus-visible,.rst:focus-visible,#evsel:focus-visible{{
  outline:2px solid var(--accent);outline-offset:2px}}
#empty{{padding:44px 0;text-align:center;color:var(--ink-3);font-size:14.5px}}
.ev[hidden]{{display:none}}
@media (max-width:640px){{.fg-grow{{flex-basis:100%}}
  .bar{{padding:9px 0 8px}} .seg button{{padding:6px 10px;font-size:12.5px}}
  .bar-in{{gap:12px 16px}} .fg{{flex:1 1 100%}} .seg{{width:100%}}
  .seg button{{flex:1}} .fsum{{flex:1 1 100%;order:3}} .bar-head #count{{margin-left:0}}}}
footer{{margin-top:44px;padding-top:22px;border-top:1px solid var(--line);
  font-size:13px;color:var(--ink-3)}}
</style>
<div class="wrap">
  <p class="eyebrow">Commonwealth Games · Glasgow 2026 · Athletics &amp; Swimming</p>
  <h1>What we called before the gun, and what actually happened</h1>
  <p class="lede">Every prediction here was fixed on <b>{cutoff}</b>, four days before the
  Games opened. No Glasgow result is in the history behind them. Ordered by how sure we
  were, strongest calls first.</p>

  <div class="stats">
    <div class="stat"><b>{called}/{n}</b><span>the winner was our top pick ({calledp}%)</span></div>
    <div class="stat"><b>{t5}/{n}</b><span>the winner was somewhere in our five ({t5p}%)</span></div>
    <div class="stat"><b>{mi}/{mt}</b><span>medallists we had in a top five ({mip}%)</span></div>
    <div class="stat"><b>{ath}+{swm}</b><span>athletics and swimming events covered</span></div>
  </div>

  <div class="note">
    <p><b>Coverage, checked against the published programme rather than assumed.</b>
    Glasgow 2026 awarded <b>{gg} golds and {gm} medals</b> across every sport; athletics
    and swimming account for 115 of those golds. citius models individual events only —
    the registry contains no relays and no para classifications — so 74 of the 115 are
    modellable at all.</p>
    <p><b>Every one of those 74 now has a prediction</b> — athletics 40 of 40,
    swimming 34 of 34. Getting there took two more passes over the Commonwealth results
    system: the original swimming capture was taken while the meet was still running and
    held only 17 events, and the World Athletics feed never populated two of the seven
    competition days, leaving 16 athletics events with no final.</p>
    <p><b>All {n} have both a prediction and a finished final</b> ({ath} athletics,
    {swm} swimming) — {mt} medallists, every one of them below. The last three took
    finding: the heptathlon's points table is not on the results route at all but
    behind a tab on a different one, the women's 1500m freestyle was swum in two
    sections so its plain final page is empty, and the 10,000m walk final simply
    failed to load first time. All three were cross-checked against Wikipedia's
    medal tables before being used.</p>
    <p><b>Matched on names, not IDs.</b> The entry-list predictions and the results feeds
    use incompatible athlete keys, so podium finishers are matched on normalised names.
    <b>{mr}%</b> matched; anyone shown as <i>unrated</i> may be someone we did rate under
    a different spelling rather than someone we missed.</p>
    <p><b>One repair.</b> {cf} Scottish athletes were filed under Rwanda in the entry
    list and are shown here as SCO.</p>
    <p><b>Checked before publishing.</b> The card passes five assertions: probabilities
    rank with predicted marks in all 74 events, nobody is a top-three favourite while
    typically finishing outside the top ten, no predicted mark is implausible for its
    event, every field is coherent, and the specific sprinter who once reached second
    favourite on a 10.97 is now 64th. Two guards were added to get there — one for
    merged identities carrying an impossible spread, one for athletes credited for
    having no history at all.</p>
  </div>

  <hr class="rule">
{filters}
  <p id="empty" hidden>No events match those filters.</p>
  {blocks}

  <footer>Predictions from the citius model, cutoff {cutoff}. Marks as recorded by the
  results feeds. Percentages are the modelled chance of gold from the pre-tournament
  simulation, not betting odds.</footer>
</div>
{filterjs}""".format(
    cutoff=esc(meta["cutoff"]), called=meta["called"], n=meta["n_events"],
    calledp=pct(meta["called"], meta["n_events"]), t5=meta["winner_in_top5"],
    t5p=pct(meta["winner_in_top5"], meta["n_events"]), mi=meta["medallists_in_top5"],
    mt=meta["medallists_total"], mip=pct(meta["medallists_in_top5"], meta["medallists_total"]),
    npred=meta["n_predicted"], mr=meta["match_rate"], cf=meta["country_fixed"],
    ath=meta["ath_events"], swm=meta["swm_events"],
    gg=meta["games_golds"], gm=meta["games_medals"], blocks="\n".join(blocks),
    filters=FILTERS, filterjs=FILTER_JS)

out = os.path.join(S, "glasgow2026.html")
io.open(out, "w", encoding="utf-8").write(page)
print("wrote", out, len(page), "bytes,", len(events), "events")
