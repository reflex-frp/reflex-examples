{-
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE RankNTypes #-}
module Host4 (
    go4
  ) where

import Data.Maybe (isJust)
import Control.Monad (unless, forM_)
import Control.Monad.Identity (Identity(..))
import Control.Monad.IO.Class (liftIO)
import Data.IORef (readIORef)
import System.IO

import Data.Dependent.Sum

import Reflex
import Reflex.Host.Class

-- I'm going to assume that you've read through Host3.hs prior to this.

-- Previously we used this:
--
-- data Input t = Input {
--    ieOpen :: Event t ()
--  , ieRead :: Event t String
--  }
--
-- for our input events.

-- We're going to wind our input back to 'Event t String' while keeping
-- our outputs as they were.
data Output t = Output {
    oeWrite :: Event t String
  , oeQuit  :: Event t ()
  }

-- We're doing this because Reflex already has support for an event that fires
-- after the event network has been set up.

-- We get that support through the monad transformer 'PostBuildT':

-- Our application type changes to:
type SampleApp4 t m =
  ( Reflex t
  , MonadHold t m
  ) => Event t String
    -> PostBuildT t m (Output t)

-- There is a class associated with 'PostBuildT':
-- class (Reflex t, Monad m) => PostBuild t m | m -> t where
--  getPostBuild :: m (Event t ())

-- So we can use our concrete application type if we like:
guest :: SampleApp4 t m
-- or we can tread a little lighter and specify the constraints that need:
-- guest :: (Reflex t, MonadHold t m, MonadFix m, PostBuild t m) => Event t String -> m (Output t)
guest eRead = do
  -- We use the typeclass to get hold of then post-build event whenever we need it:
  eOpen <- getPostBuild

  -- The rest of this function is the same as in Host3.hs
  let
    eMessage =       ffilter (/= "/quit") eRead
    eQuit    = () <$ ffilter (== "/quit") eRead
    eWrite   = leftmost [
        "Hi"      <$  eOpen
      , ("> " ++) <$> eMessage
      , "Bye"     <$  eQuit
      ]
  return $ Output eWrite eQuit

host :: (forall t m. SampleApp4 t m)
     -> IO ()
host myGuest =
  runSpiderHost $ do
    (eOpen, eOpenTriggerRef) <- newEventWithTriggerRef
    (eRead, eReadTriggerRef) <- newEventWithTriggerRef

    -- This is the only line that is different from the 'host' function in Host3.hs
    Output eWrite eQuit <- runHostFrame $ runPostBuildT (myGuest eRead) eOpen
    -- We pass 'runPostBuildT' a value of the appropriate type and the event we plan on
    -- using to signal that the event network is ready, and then we're good to go.

    hWrite <- subscribeEvent eWrite
    hQuit  <- subscribeEvent eQuit

    liftIO $ hSetBuffering stdin LineBuffering

    let
      readPhase = do
        mWrite <- readEvent hWrite >>= sequence
        mQuit  <- readEvent hQuit >>= sequence
        return (mWrite, mQuit)

      handleOutputs mWrite mQuit = do
        forM_ mWrite $ liftIO . putStrLn
        return $ isJust mQuit

      fireAndProcess t v = do
        mETrigger <- liftIO $ readIORef t
        (mWrite, mQuit) <- case mETrigger of
          Nothing ->
            return (Nothing, Nothing)
          Just eTrigger ->
            fireEventsAndRead [eTrigger :=> Identity v] readPhase
        handleOutputs mWrite mQuit

      fireProcessAndLoop t v k = do
        quit <- fireAndProcess t v
        unless quit
          k

      loop = do
        input <- liftIO getLine
        fireProcessAndLoop eReadTriggerRef input loop

    fireProcessAndLoop eOpenTriggerRef () loop

go4 :: IO ()
go4 =
  host guest
