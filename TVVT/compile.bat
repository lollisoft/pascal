set PRJPATH=Q:\develop\Projects\PASCAL\TVVT

set TURBO=Q:\develop\Tools\tp
set PATH=%TURBO%;%TURBO%\tpu
set SWITCHES=/U%TURBO%\tpu /E%TURBO%\tpu /V /GD /B /M /L /$R- /$X+ /$B- /$v-
set TV=Q:\develop\Projects\PASCAL\TV

rem cd %TV%\tools
rem C:\winnt\system32\cmd /C tpc %SWITCHES% strings.pas

rem cd %TV%\editor
rem C:\winnt\system32\cmd /C tpc %SWITCHES% editor.pas

rem cd %PRJPATH%

C:\winnt\system32\cmd /C tpc %SWITCHES% %1


