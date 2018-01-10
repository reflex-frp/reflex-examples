# Reflex Examples

## How to Compile?

First, get the repo with `git clone` and `cd` into the directory, and 
after that make sure that the reflex-platform is in place:

```
$ git submodule update --init --recursive
```


To build with GHC, use the nix-shell command to enter the sandbox shell and 
use cabal (which is supplied by the sandbox):

```
$ nix-shell -A shells.ghc
$ cabal new-build all
```

To build with GHCJS:

```
$ nix-shell -A shells.ghcjs
$ cabal --project-file=cabal-ghcjs.project --builddir=dist-ghcjs new-build all
```

You can also build examples separately by replacing all with exe:name, e.g.
```
$ cabal new-build exe:othello
$ cabal --project-file=cabal-ghcjs.project --builddir=dist-ghcjs new-build exe:othello
```


For further information, check the instructions on 
[project-development](https://github.com/reflex-frp/reflex-platform/blob/develop/docs/project-development.md)
and 
[project-skeleton](https://github.com/ElvishJerricco/reflex-project-skeleton).


## About the Examples

### Basic Todo

An example on how to use listWithKey, textInput and mergeWith to implement
a basic todo-list application.

ghcjs ok, webkit2gtk ok.


### Drag and Drop

An example on how to apply dom-api and wrapDomEvent to implement a basic drag 
and drop functionality. 

GHCJS:
Note that the arduino.jpg should be either in the directory containing index.html 
or the index.html should be modified to point to arduino.jpg.

Webkit2gtk:
Note that the arduino.jpg should be in the directory the program is started.


### File Input

Another example on how to apply dom-api. 

ghcjs ok.  Webkit2gtk-version starts but gives "GLib-GIO-Error: No GSettings schemas are installed on the system Trace/breakpoint trap."


### Keyboard

An example on how to keep focus on textInput. It uses dom-api.

ghcjs ok, webkit2gtk ok.


### Nasa Pod

An example on how to use Xhr-api.

See the README.md at nasa-pod.

Ghcjs version is ok but webkit2gtk version seems to have problems.


### Othello

See the README.md at othello. As an additionl todo-item, there are png's ready 
to be applied.


### Peg Solitaire

This uses embedFile to add css-files to the generated code.


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

An example on how to use websocket-api.

### Simple Websocket Chat

This uses backend-ws-chat and ws-chat. This is a modification of websocket 
echo example and the example at 
[websockets](https://github.com/jaspervdj/websockets) library.

How to use:
- start then ws-serber (backendwschat somewhere in the dist-newstyle-dir)
- start frontend (webkit2gtk version works, that is, wschat somewhere in dist-newstyle dir)


TODO: extend a bit to show how to apply common-lib.


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
- If trying to access the file directly:
file:///home/.../reflex-examples/:243:76: CONSOLE ERROR XMLHttpRequest cannot load file:///home/.../reflex-examples/out.stats. Cross origin requests are only supported for HTTP.
- If serving files with web-server (fix this):
CONSOLE ERROR Origin null is not allowed by Access-Control-Allow-Origin.
CONSOLE ERROR XMLHttpRequest cannot load http://localhost/out.stats due to access control checks.


