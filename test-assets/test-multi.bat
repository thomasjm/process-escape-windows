@ECHO OFF

:loop
if [%1]==[] goto :end
if "%~1"=="" (
    echo.
) else (
    echo %1
)
shift
goto :loop
:end
