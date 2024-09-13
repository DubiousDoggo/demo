@REM see https://www.robvanderwoude.com/parameters.php for info on batch params

py %~dp0\build_font.py %~dp0\..\fonts\var_font.png %~dp0\..\src\ 

@set out-dir=%~dp0\..\bin\
.\cc65\bin\cl65.exe ^
    -t c64 ^
    -C c64-asm.cfg ^
    -Ln "%out-dir%%~n1.lbl" ^
    -g ^
    -u __EXEHDR__ ^
    %~f1 ^
    -o %out-dir%%~n1.prg ^
    -Wl "--mapfile,%out-dir%%~n1.map" ^
    -Wl "--dbgfile,%out-dir%%~n1.dbg" ^