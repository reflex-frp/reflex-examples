# Reflex Examples

## How to Compile?

To build with GHC, use the nix-shell command to enter the sandbox shell and use cabal (which is supplied by the sandbox):

```
$ nix-shell -A shells.ghc
[nix-shell:~/path]$ cabal new-build all
```

To build with GHCJS:

```
$ nix-shell -A shells.ghcjs
[nix-shell:~/path]$ cabal --project-file=cabal-ghcjs.project --builddir=dist-ghcjs new-build all
```

## About the Examples

### Basic Todo

ghcjs ok, webkit2gtk ok.


### Drag and Drop

GHCJS:
Note that the arduino.jpg should be either in the directory containing index.html 
or the index.html should be modified to point to arduino.jpg.

Webkit2gtk:
Note that the arduino.jpg should be in the directory the program is started.



### File Input

Webkit2gtk-version starts but gives "GLib-GIO-Error: No GSettings schemas are installed on the system Trace/breakpoint trap."

### Keyboard

### Nasa Pod

See the README.md at nasa-pod.

### Othello

See the README.md at othello. As an additionl todo-item, there are png's ready 
to be applied.


### Peg Solitaire

GHCJS:
The svgs are in static/images -directory. You could, e.g., copy the 
static-directory into the peg-solitaire.jsexe-directory so that the images are
found.

Webkit2gtk:
The svgs are in static/images -directory. The program assumes that it is started
at the peg-solitaire -directory to find the images. It starts and works without 
images if started elsewhere.

Font-error should be fixed. 

### Websocket Echo

### Xhr Blob


GHCJS:
- Copy the xhrblob.jsexe-diroctory to xhr-blob/out -directory.
- E.g. `cd xhr-blob/out` and then `cp -R ../../dist-ghcjs/build/x86_64-linux/ghcjs-0.2.1/xhrblob-0.1.0.0/c/xhrblob/build/xhrblob/xhrblob.jsexe/ .`
- (Note that the path maybe different.)
- The xhr-blob/out/index.html refers to xhr-blob/out/xhrblob.jsexe/all.js. 
- Serve the files of xhr-blob/out with a webserver (e.g. `warp -d . -p 8000`).
- Start you browser and point it to the localhost:8000. 
- You should see a jpg and contents of the out.stats-file.


To try the following, you should take a look of the code and uncomment couple 
of lines.

Webkit2gtk:
If trying to access the file directly:
file:///home/.../reflex-examples/:243:76: CONSOLE ERROR XMLHttpRequest cannot load file:///home/.../reflex-examples/out.stats. Cross origin requests are only supported for HTTP.

If serving files with web-server (fix this):
CONSOLE ERROR Origin null is not allowed by Access-Control-Allow-Origin.
CONSOLE ERROR XMLHttpRequest cannot load http://localhost/out.stats due to access control checks.


