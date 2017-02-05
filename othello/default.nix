{ mkDerivation, reflex, reflex-dom, file-embed, array, containers, split,
  deepseq, text, transformers
}:

mkDerivation {
  pname = "othello";
  version = "0.1.1";
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
    text
    transformers
  ];
  license = null;
}
