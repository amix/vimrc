@echo off

REM Script to run the unit-tests for the MRU Vim plugin on MS-Windows

SETLOCAL
SET VIMPRG="vim.exe"
REM SET VIMPRG="C:\Program Files (x86)\vim\vim82\vim.exe"
REM SET VIMPRG="C:\Program Files (x86)\vim\vim73\vim.exe"
SET VIM_CMD=%VIMPRG% -N -u NONE -U NONE -i NONE --not-a-term

%VIM_CMD% -S unit_tests.vim

echo MRU unit test results
type results.txt

findstr /I FAIL results.txt > nul 2>&1
if %ERRORLEVEL% EQU 0 echo ERROR: Some test failed.
if %ERRORLEVEL% NEQ 0 echo SUCCESS: All the tests passed.
