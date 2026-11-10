# Apply AFTER the running arm finishes (it reads backtest_athletics.R).
# Fix 1: ability-cache key ignores edits inside [post-cache] regions.
# Fix 2: CITIUS_BT_EVENTS restricts the arm to named events.
p = r'C:/dev/citiusverse/citiusdata/scripts/backtest_athletics.R'
s = open(p, encoding='utf-8').read()
def rep(a, b):
    global s
    assert s.count(a) == 1, ('not unique/absent', a[:80])
    s = s.replace(a, b)

# --- markers around the post-cache regions -----------------------------------
rep('# THIN-RECORD ARM ("version C", Pete 2026-11-09). For the listed events only:',
    '# [post-cache:begin] -- edits between these markers do NOT invalidate the ability\n'
    '# cache (ABIL_CODE_MD5 below): everything here runs after the cache is written.\n'
    '# THIN-RECORD ARM ("version C", Pete 2026-11-09). For the listed events only:')
# end of thin_refit(): first "}" line after "thin_refit <- function"
i = s.index('thin_refit <- function(')
j = s.index('\n}\n', i) + 3
s = s[:j] + '# [post-cache:end]\n' + s[j:]
rep('  # Thin-record arm (flag definition above). The long refit MIRRORS the',
    '  # [post-cache:begin]\n  # Thin-record arm (flag definition above). The long refit MIRRORS the')
rep('  # CHAMPIONSHIP OFFSET -- NOW GATED ON THE RACE ACTUALLY BEING A CHAMPIONSHIP.',
    '  # [post-cache:end]\n\n  # CHAMPIONSHIP OFFSET -- NOW GATED ON THE RACE ACTUALLY BEING A CHAMPIONSHIP.')

# --- Fix 1: code hash that skips post-cache regions --------------------------
rep('''if (is.na(this_script_md5)) cli::cli_alert_warning(
  "Could not md5 this script's own file; the fingerprint cannot detect a code edit here.")''',
'''if (is.na(this_script_md5)) cli::cli_alert_warning(
  "Could not md5 this script's own file; the fingerprint cannot detect a code edit here.")
# ABILITY-CODE HASH (2026-11-10, Pete: "don't rerun parts that didn't change"). The
# ability cache used the WHOLE script md5, so editing code that runs AFTER the cache
# (the thin-record/blend arms) forced a full refit of every meet: ~4-5 h per arm,
# twice on 2026-11-10. This hash drops the [post-cache:begin]..[post-cache:end]
# regions and keys the ABILITY cache on the rest; any other edit still invalidates
# it (conservative), and the RESULTS cache still uses the full script md5.
ABIL_CODE_MD5 <- tryCatch({
  .l <- readLines(here::here("citiusdata", "scripts", "backtest_athletics.R"), warn = FALSE)
  .b <- grepl("^\\\\s*# \\\\[post-cache:begin\\\\]", .l); .e <- grepl("^\\\\s*# \\\\[post-cache:end\\\\]", .l)
  if (sum(.b) != sum(.e) || !sum(.b)) stop("unbalanced or missing [post-cache] markers")
  .in <- cumsum(.b) - cumsum(c(FALSE, head(.e, -1))) > 0
  digest::digest(paste(.l[!.in], collapse = "\\n"), algo = "md5")
}, error = function(e) { cli::cli_alert_warning("ABIL_CODE_MD5 fell back to the full script md5: {conditionMessage(e)}"); this_script_md5 })''')
rep('''  script_md5 = this_script_md5, citius_git = citius_git,''',
    '''  script_md5 = this_script_md5, abil_code_md5 = ABIL_CODE_MD5, citius_git = citius_git,''')
rep('''ABIL_EXCLUDE <- c("n_sims", "marks_only", "project_tier", "project_round", "spread_scale",
                  "thin_record", "thin_record_md5", "thin_blend", "thin_blend_par")''',
'''ABIL_EXCLUDE <- c("n_sims", "marks_only", "project_tier", "project_round", "spread_scale",
                  "thin_record", "thin_record_md5", "thin_blend", "thin_blend_par",
                  # the full-script md5: abil_code_md5 stands in for it in this key
                  "script_md5")''')

# --- Fix 2: event filter -----------------------------------------------------
rep('''# Sample meets evenly across time rather than taking the most recent, so the
# backtest is not all one era.''',
'''# EVENT FILTER (CITIUS_BT_EVENTS, comma-separated event_ids; 2026-11-10). An arm that
# changes only some events need not fit or score the rest: they are identical to the
# control by construction (the invariance check proves it). Restricting `finals` here
# shrinks the meets, the entrants refitted (only=) and the races scored together.
# In the fingerprint, so a filtered arm can never share a results cache with a full one.
EVENT_FILTER <- trimws(strsplit(Sys.getenv("CITIUS_BT_EVENTS", ""), ",")[[1]])
EVENT_FILTER <- EVENT_FILTER[nzchar(EVENT_FILTER)]
if (length(EVENT_FILTER)) {
  .before <- uniqueN(finals$competition_id)
  finals <- finals[event_id %in% EVENT_FILTER]
  if (!nrow(finals)) cli::cli_abort("CITIUS_BT_EVENTS matched no finals: {.val {EVENT_FILTER}}.")
  cli::cli_alert_info("Event filter: {length(EVENT_FILTER)} event{?s}; {uniqueN(finals$competition_id)} of {.before} meets kept.")
}

# Sample meets evenly across time rather than taking the most recent, so the
# backtest is not all one era.''')
rep('''  thin_blend = THIN_BLEND,''', '''  event_filter = paste(sort(EVENT_FILTER), collapse = ","),
  thin_blend = THIN_BLEND,''')
open(p, 'w', encoding='utf-8', newline='').write(s)
print('patched')
