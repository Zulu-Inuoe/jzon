@setlocal EnableDelayedExpansion

@for /r %%i in (test_parsing\*) do @(
  set "name=%%~ni"
  echo|set /p="%%~nxi ... "
  "%~dp0bin\jzon-parsing" "%%i"

  set errlvl=!ERRORLEVEL!

  if !errlvl! EQU 0 (
    if "!name:~0,1!" == "n" (
      echo ERROR PASSED
    ) else (
      echo OK
    )
  ) else if !errlvl! EQU 1 (
    if "!name:~0,1!" == "y" (
      echo ERROR FAILED
    ) else (
      echo OK
    )
  ) else (
    echo ERROR !errlvl!
  )
)

@endlocal
