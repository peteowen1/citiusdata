# After a promotion in _deployed.R, every published card is stale until it is
# re-predicted under the new stamp -- the sanity scripts catch it, the site does
# not. This runs run_meet.ps1 for every card meet (entries kept, upload held),
# then publishes once. ~4 min per meet.
#
#   powershell -NoProfile -File citiusdata\scripts\refresh_all_cards.ps1
#   log: citiusdata\refresh_cards_log.txt ; aborts on the first failing meet
param([string[]]$Meets = @("birmingham2026", "lausanne2026", "silesia2026", "zurich2026", "brussels2026", "budapest2026"))
Set-Location "C:\dev\citiusverse"
$LOG = "C:\dev\citiusverse\citiusdata\refresh_cards_log.txt"
"=== START $(Get-Date -Format s) ===" | Out-File -Encoding utf8 $LOG
foreach ($m in $Meets) {
  "--- $m start $(Get-Date -Format s) ---" | Out-File -Append -Encoding utf8 $LOG
  & pwsh -NoProfile -File "C:\dev\citiusverse\citiusdata\scripts\run_meet.ps1" $m -SkipUpload -SkipEntries 2>&1 | Out-File -Append -Encoding utf8 $LOG
  if ($LASTEXITCODE -ne 0) { "!!! $m failed (exit $LASTEXITCODE) -- stopping before publish" | Out-File -Append -Encoding utf8 $LOG; exit 1 }
  "--- $m done $(Get-Date -Format s) ---" | Out-File -Append -Encoding utf8 $LOG
}
"--- publish start $(Get-Date -Format s) ---" | Out-File -Append -Encoding utf8 $LOG
Set-Location "C:\dev\citiusverse"
& Rscript citiusdata\scripts\export_athletics_blog.R 2>&1 | Out-File -Append -Encoding utf8 $LOG
"=== ALL DONE $(Get-Date -Format s) (exit $LASTEXITCODE) ===" | Out-File -Append -Encoding utf8 $LOG
