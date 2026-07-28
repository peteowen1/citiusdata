@echo off
REM Daily Glasgow 2026 refresh for the In The Game blog (citiusdata#5, option 1).
REM
REM Harvests results, rebuilds the five artefacts and uploads them to R2. Safe to
REM run repeatedly: nothing is cached and a later run supersedes an earlier one.
REM
REM Timing: Glasgow finals land roughly 19:00-21:30 BST, which is 04:00-06:30
REM AEST the NEXT day. A single morning run therefore captures a complete
REM competition day. Running it earlier just re-uploads yesterday's set with a
REM fresh generated_at, which is harmless but pointless.
REM
REM Register it (run once, from an elevated prompt if it complains):
REM   schtasks /create /tn "citius blog refresh" /tr "C:\dev\citiusverse\citiusdata\scripts\refresh_blog.cmd" /sc daily /st 07:30
REM Remove it:
REM   schtasks /delete /tn "citius blog refresh" /f
REM
REM NOT registered automatically - creating standing automation on the machine is
REM a decision to take deliberately, not a side effect of a script existing.
REM
REM Failure is visible two ways: this log, and the site itself. The export
REM publishes `harvest_ok`, so a failed feed shows a red "Results feed
REM unavailable" banner rather than a green stamp over stale numbers.

setlocal
set REPO=C:\dev\citiusverse
set LOG=%REPO%\citiusdata\blog\refresh.log

cd /d "%REPO%" || (echo [%date% %time%] FAIL cd %REPO% >> "%LOG%" & exit /b 1)

echo [%date% %time%] START >> "%LOG%"
Rscript "%REPO%\citiusdata\scripts\export_blog_data.R" >> "%LOG%" 2>&1
if errorlevel 1 (
  echo [%date% %time%] FAIL export_blog_data.R exit=%errorlevel% >> "%LOG%"
  exit /b 1
)
echo [%date% %time%] OK >> "%LOG%"
endlocal
