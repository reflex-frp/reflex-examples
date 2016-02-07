{-# LANGUAGE OverloadedStrings #-}
import Snap
import Snap.Util.FileServe
import Control.Monad
import Network.WebSockets
import Network.WebSockets.Snap

main :: IO ()
main = quickHttpServe $ do
  route [ ("", serveDirectory "frontend.jsexe")
        , ("api", handleApi)
        ]

handleApi :: MonadSnap m => m ()
handleApi = runWebSocketsSnap $ \pc -> do
  conn <- acceptRequest pc
  forever $ sendDataMessage conn =<< receiveDataMessage conn
