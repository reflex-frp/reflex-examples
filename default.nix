{}:
(import ./reflex-platform {}).project ({ pkgs, ... }: {
  packages = {
    common = ./common;
    # backend = ./backend;
    /* frontend = ./frontend; */
    basictodo = ./BasicTodo;
    draganddrop = ./drag-and-drop;
    fileinput = ./fileinput;
    nasapod = ./nasa-pod;
    keyboard = ./Keyboard;
    othello = ./othello;
    pegsolitaire = ./peg-solitaire;
    websocketecho = ./websocket-echo;
    xhrblob = ./xhr-blob;
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
    ];
    ghcjs = ["keyboard" "basictodo" "draganddrop" "fileinput" "nasapod"
        "othello" "pegsolitaire" "websocketecho" "xhrblob"
    ];
    /* ghc = ["frontend"]; */
    /* ghcjs = ["frontend"]; */
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
