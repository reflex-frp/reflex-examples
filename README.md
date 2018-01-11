# Reflex Examples

Depending on example, the executable or library can be compiled either with ghc 
or ghcjs, or both. One of the aims of the "project"-setup is to allow frontend 
code to be compiled separately from the backend code, and to have a common library
enabling communication between the front and back. As the library is written
in Haskell, too, that is, the same language frontend and backend use, the 
communication is easy to setup. It is easy to share common 
data-structures and functions operating on them between front and back.

As the frontend can be ghcjs, webkit2gtk, android and ios, the 
compiler and associated tools can vary from that of backend and backend code
is not typically compiled in the frontend environment.

The project-setup of this repo demonstrates, how to setup enviroments for
the frontend, common parts and backend. Especially, the `default.nix`-file
should correspond with the `cabal.project` and `cabal-ghcjs.project` files.

Most of the examples are only meant for the frontend usage without any
backend functionality. The simple websocket chat -example below demonstrates
frontend/common/backend functionality.


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


Note that if you have already obtained examples but want to update the
reflex-platform, you can try, e.g., 
```
git submodule foreach "(git checkout master; git pull --recurse-submodules)&"
```


## Before Trying Out

If you haven't read the following yet, they help to build up a mental image 
what is going on:

- [The reflex introduction](https://blog.qfpl.io/posts/reflex/basics/introduction/). 
- [Beginner Friendly Step by Step Tutorial for Reflex-Dom](https://github.com/hansroland/reflex-dom-inbits/blob/master/tutorial.md)
- [Real World Reflex](https://github.com/mightybyte/real-world-reflex/blob/master/index.md)
- Other [docs](http://docs.reflex-frp.org/en/latest/)

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

See the README.md at othello. 

Note that there is a static directory containing png's. At the moment they 
are not used in the program. It makes a nice exercise to add them (so that they
work with both js and webkit2gtk).


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

Simple websocket chats come with two flavours. Note that the clients (ws-chats) 
can be compiled with both ghc and ghcjs together with the common parts, while
the backend servers (backend-ws-chats) use ghc only.

Note also that the `default.nix` sets the environments so that backends are 
only build with ghc and the front can be build with both ghc and ghcjs. The 
`cabal.project` and `cabal-ghcjs.project` have similar separation: failing to
do that can lead to trying to build and link your z-lib to snap-server with
ghcjs and seeing it to not work after one hour or so.


#### Simple ws chat 1

This uses backend-ws-chat and ws-chat. This is a modification of websocket 
echo example and the example at 
[websockets](https://github.com/jaspervdj/websockets) library.

How to use:
- start then ws-server (backendwschat somewhere in the dist-newstyle-dir)
- start frontend (webkit2gtk version works, that is, wschat somewhere in dist-newstyle dir)


#### Simple ws chat 2

This extends the first example and show the common (common-ws-chat2) lib in 
action. In addition to that, the focus is set into the input fields.

Remember to try the webkit2gtk version of the chat at the same time with 
js-version. Server, chat and common lib names end with "2".


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


## TODO list

There are some todo-items scattered in the code. Many of the examples could
benefit with some little additions and detailed explanations of what is
happening and why things are implemented in the way they are (and is there
alternatives).


Here are random ideas that could have examples, too:

- an example of runEventWriter and its usage
- a host example
- how to set cors policy with pointers for the server side 
- debuggind aids (tracing, how to write to console, etc)
- where to find api-docs (on nixos), 
  how to control the hoogle-building for you own modules
- more learning aids, like good links to fix/mfix explanations and how to 
  recognize when the laziness is lost with fix/mfix
- when to use jsaddle libs (import JSDOM) and when ghcjs (import GHCJS.DOM)
