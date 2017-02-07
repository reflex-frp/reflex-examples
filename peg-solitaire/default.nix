{ mkDerivation, reflex, reflex-dom, array, text, file-embed
}:

mkDerivation {
  pname = "peg-solitairec";
  version = "0.1.1";
  src = builtins.filterSource (path: type: baseNameOf path != ".git") ./.;
  isExecutable = true;
  buildDepends = [
    array
    reflex
    reflex-dom
    text
    file-embed
  ];
  license = null;
}
