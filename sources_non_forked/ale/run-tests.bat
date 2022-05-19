@echo off
REM Run tests on Windows.
REM
REM To run these tests, you should set up your Windows machine with the same
REM paths that are used in AppVeyor.

set tests=test/*.vader test/*/*.vader test/*/*/*.vader test/*/*/*/*.vader

REM Use the first argument for selecting tests to run.
if not "%1"=="" set tests=%1

set VADER_OUTPUT_FILE=%~dp0\vader_output
REM Automatically re-run Windows tests, which can fail some times.
set tries=0

:RUN_TESTS
set /a tries=%tries%+1
type nul > "%VADER_OUTPUT_FILE%"
C:\vim\vim\vim80\vim.exe -u test/vimrc "+Vader! %tests%"
set code=%ERRORLEVEL%

IF %code% EQU 0 GOTO :SHOW_RESULTS
IF %tries%  GEQ 2 GOTO :SHOW_RESULTS
GOTO :RUN_TESTS

:SHOW_RESULTS
type "%VADER_OUTPUT_FILE%"
del "%VADER_OUTPUT_FILE%"

exit /B %code%
