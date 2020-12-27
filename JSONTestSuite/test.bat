@setlocal EnableDelayedExpansion

@set failed=0

@for /r %%i in (test_parsing\*) do @(
  set "name=%%~ni"
  echo|set /p="%%~nxi ... "
  "%~dp0bin\jzon-parsing" "%%i"

  if !ERRORLEVEL! EQU 0 (
    if "!name:~0,1!" == "n" (
      set failed=1
      echo ERROR PASSED
    ) else (
      echo OK
    )
  ) else if !ERRORLEVEL! EQU 1 (
    if "!name:~0,1!" == "y" (
      set failed=1
      echo ERROR FAILED
    ) else (
      echo OK
    )
  ) else (
    echo ERROR !errlvl!
  )
)

@if %failed% neq 0 exit /b %failed%

@endlocal
