@echo off
cd /d "%~dp0"
"C:\Program Files\R\R-4.6.0\bin\Rscript.exe" "%~dp0run_app.R" > "%~dp0shiny_stdout.log" 2> "%~dp0shiny_stderr.log"
