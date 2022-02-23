{ system ? builtins.currentSystem
, withHoogle ? false # to spin up localhost:8080 hoogle use: nix-shell --arg withHoogle true -A shells.ghc --command "hoogle server -p 8080 --local"
, obelisk ? import ./.obelisk/impl {
    inherit system;
    iosSdkVersion = "13.2";
  }
}:
with obelisk;
project ./. ({ hackGet, ... }: {
  inherit withHoogle;
  packages = {
    reflex-dom-echarts = hackGet ./deps/reflex-dom-echarts;
    echarts-jsdom = hackGet ./deps/echarts-jsdom;
  };
})
