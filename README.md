
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

### Manual Method
run `./scripts/build.bat ./src/test1.asm` from the parent directory.

## RUN
run `./scripts/run.bat` from the parent directory.

If you're not on Windows, good luck!