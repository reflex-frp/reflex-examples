{ mkDerivation, reflex, reflex-dom, file-embed, array, containers, split,
  deepseq, transformers
}:

mkDerivation {
  pname = "peg-solitairec";
  version = "0.1";
  src = builtins.filterSource (path: type: baseNameOf path != ".git") ./.;
  isExecutable = true;
  buildDepends = [
    array
    containers
    split
    reflex
    reflex-dom
    file-embed
    deepseq
    transformers
  ];
  license = null;
}
