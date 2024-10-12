
# C64 UNTITLED DEMO PROJECT

## FILES TO INSTALL
These are not included in the repo and must be downloaded yourself:
 - C64Debugger
 - VICE
The scripts expect each of these to be installed in a directory with that name.

cc65 is included as a git submodule.

## BUILD
### The easy way
The included .vscode task file has a build task that will do all the build for you.  
If you're not on vscode, the included makefile should do the trick. run `make` from the parent directory.

### Manual Method
run these commands from the parent directory:  
`py ./scripts/build_font.py ./fonts/var_font.png ./src/`  
`.\cc65\bin\cl65.exe -t c64 -C c64-asm.cfg -g ./src/test1.asm -o ./bin/test1.prg -Ln "./bin/test1.lbl" -Wl "--mapfile,./bin/test1.map" -Wl "--dbgfile,./bin/test1.dbg"`  

## RUN
run `./scripts/run.bat` from the parent directory.

If you're not on Windows, good luck!
