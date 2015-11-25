This example uses Reflex.Dom and a Javascript FFI imported FileReader to demonstrate the fileInput widget by creating data URLs from loaded image files.
Eventually most of the FileReader stuff will become available via ghcjs-dom, and the Reflex binding to it will be in reflex-dom.

To build this module in GHC, enter the try-reflex shell and run:

    ghc src/Main.hs

To build this module in GHCJS, enter the try-reflex shell and run:

    ghcjs src/Main.hs
