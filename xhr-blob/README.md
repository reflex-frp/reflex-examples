This example uses Reflex.Dom.Xhr to retrieve an image file as a [Blob](https://developer.mozilla.org/en-US/docs/Web/API/Blob) using XMLHttpRequest and display the result.

To build in GHC, run:
    
    ./build-ghc

To run in webkitgtk:
    cd out
    ./Main

To build in GHJCS, run:

    ./build-ghcjs

Once built, open out/index.html in your browser. Some browsers may not allow XMLHttpRequest over the "file" protocol (e.g., chrome). To get around the issue, you can run a local server, e.g.:
    
    cd out
    python -m SimpleHTTPServer 8000

Once the server has started, go to http://localhost:8000 in your browser.
