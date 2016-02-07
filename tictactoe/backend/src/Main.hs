{-# LANGUAGE OverloadedStrings #-}
import Snap
import Snap.Util.FileServe
import Control.Monad
import Network.WebSockets
import Network.WebSockets.Snap
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Concurrent
import Control.Exception

main :: IO ()
main = do
  state <- newMVar (1, mempty)
  quickHttpServe $ do
    route [ ("", serveDirectory "frontend.jsexe")
          , ("api", handleApi state)
          ]

handleApi :: MonadSnap m => MVar (Integer, Map Integer Connection) -> m ()
handleApi state = runWebSocketsSnap $ \pc -> do
  conn <- acceptRequest pc
  let addMyConnection = modifyMVar state $ \(nextId, cs) -> return ((succ nextId, Map.insert nextId conn cs), nextId)
      removeMyConnection myId = modifyMVar state $ \(nextId, cs) -> return ((nextId, Map.delete myId cs), ())
  bracket addMyConnection removeMyConnection $ do
    forever $ \_ -> do
      m <- receiveDataMessage conn
      withMVar state $ \(_, state) -> do
        forM_ state $ \dstConn -> do
          sendDataMessage dstConn m
