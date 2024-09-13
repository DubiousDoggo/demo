py %~dp0\build_font.py %~dp0\..\fonts\var_font.png %~dp0\..\src\

.\cc65\bin\cl65.exe -t c64 -C c64-asm.cfg -g -Ln %~dpn1.lbl -u __EXEHDR__ %~f1 -o %~dpn1.prg -Wl "--mapfile,%~dpn1.map" -Wl "--dbgfile,%~dpn1.dbg"