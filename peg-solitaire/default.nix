{ mkDerivation, reflex, reflex-dom, file-embed
}:

mkDerivation {
  pname = "peg-solitairec";
  version = "0.1";
  src = builtins.filterSource (path: type: baseNameOf path != ".git") ./.;
  isExecutable = true;
  buildDepends = [
    array
    reflex
    reflex-dom
    file-embed
  ];
  license = null;
}
