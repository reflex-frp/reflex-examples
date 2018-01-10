{}:
(import ./reflex-platform {}).project ({ pkgs, ... }: {
  packages = {
    common = ./common;
    backend = ./backend;
    frontend = ./frontend;
    basictodo = ./BasicTodo;
    draganddrop = ./drag-and-drop;
    fileinput = ./fileinput;
    nasapod = ./nasa-pod;
    keyboard = ./Keyboard;
    othello = ./othello;
    pegsolitaire = ./peg-solitaire;
    websocketecho = ./websocket-echo;
    xhrblob = ./xhr-blob;
    wschat = ./ws-chat;
    wschat2 = ./ws-chat2;
    commonwschat2 = ./common-ws-chat2;
    backendwschat = ./backend-ws-chat;
    backendwschat2 = ./backend-ws-chat2;
  };

  /* android.frontend = { */
  /*   executableName = "frontend"; */
  /*   applicationId = "org.example.frontend"; */
  /*   displayName = "Example Android App"; */
  /* }; */

  /* ios.frontend = { */
  /*   executableName = "frontend"; */
  /*   bundleIdentifier = "org.example.frontend"; */
  /*   bundleName = "Example iOS App"; */
  /* }; */

  shells = {
    ghc = ["keyboard" "basictodo" "draganddrop" "fileinput" "nasapod"
        "othello" "pegsolitaire" "websocketecho" "xhrblob"
        "wschat" "backendwschat"
        "wschat2" "backendwschat2" "commonwschat2"
    ];
    ghcjs = ["keyboard" "basictodo" "draganddrop" "fileinput" "nasapod"
        "othello" "pegsolitaire" "websocketecho" "xhrblob"
        "wschat"
        "wschat2" "commonwschat2"
    ];
    /* ghc = ["common" "backend" "frontend"]; */
    /* ghcjs = ["common" "frontend"]; */
  };
  tools = ghc: with ghc; [
    pkgs.haskellPackages.ghc-mod
    pkgs.haskellPackages.hasktags
    pkgs.haskellPackages.haskdogs
    pkgs.haskellPackages.hdevtools
    pkgs.haskellPackages.hindent
    pkgs.haskellPackages.hsimport
    pkgs.haskellPackages.hlint
    pkgs.haskellPackages.pointfree
    pkgs.haskellPackages.pointful
    pkgs.haskellPackages.stylish-haskell
  ];
})
