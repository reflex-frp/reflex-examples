{-
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Host5 (
    go5
  ) where

import Data.Maybe (isJust)
import Control.Monad (unless)
import Control.Monad.Identity (Identity(..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Ref
import Data.IORef (readIORef)
import System.IO

import Data.Dependent.Sum

import Reflex
import Reflex.Host.Class
import Reflex.PerformEvent.Base

-- I'm going to assume that you've read through Host4.hs prior to this.

-- Previously we were dealing with event networks which performed no IO.
--
-- Instead, our applications used output events to signal to the 'host' function
-- that IO should be performed.  We could have gotten fancy and used a free monad
-- to interpret those output events, but that would have been unnecessarily distracting.

-- For some domains, we'll want the users to be able to perform IO from withing their
-- event networks.

-- We can support this by bringing the 'PerformEventT' monad transformer into play.
-- (Note that above we have imported 'Reflex.PerformEvent.Base')

-- We're going to change our application type so that we can do arbitrary IO from
-- withing the network.  This will involve paring our output type to a single 'Event t ()'
-- that signals when the user wants to exit.

-- We need to add quite a few extra constraints to our application type tot make this
-- work, but that (or using 'ConstraintKinds' to gather them together) can help to
-- make things easier for our users.

-- Other than that, the main thing to look out for is the use of 'PerformEventT':
type SampleApp5 t m = ( Reflex t
                      , Ref m ~ Ref IO
                      , ReflexHost t
                      , MonadRef (HostFrame t)
                      , Ref (HostFrame t) ~ Ref IO
                      , MonadIO (HostFrame t)
                      )
                  => Event t String
                  -> PostBuildT t (PerformEventT t m) (Event t ())

-- There is a class associated with 'PerformEventT':
--
-- class (Reflex t, Monad (Performable m), Monad m) => PerformEvent t m | m -> t where
--  type Performable m :: * -> *
--  performEvent :: Event t (Performable m a) -> m (Event t a)
--  performEvent_ :: Event t (Performable m ()) -> m ()

-- For the purposes of this example, I'll stick with the concrete type rather than
-- spelling out all of the constraints or setting up the necessary constraint synonym.
guest :: SampleApp5 t m
guest eRead = do
  eOpen <- getPostBuild

  let
    -- We are adding a new command, so we filter messages based on a leading "/"
    eMessage =       ffilter ((/= "/") . take 1) eRead
    -- The new command will be used to print the README.md file in the same directory
    -- that this program is run from.
    eCat     = () <$ ffilter (== "/cat") eRead
    eQuit    = () <$ ffilter (== "/quit") eRead

  -- We use 'performEvent' if we care about the value returned by the IO action.
  -- We will get an event that fires with the value of the IO action when it completes.
  --
  -- We need to use 'liftIO' with both functions from 'PerformEvent'
  eCatOut <- performEvent $ liftIO (readFile "README.md") <$ eCat

  let
    eWrite   = leftmost [
        "Hi"      <$  eOpen
      , ("> " ++) <$> eMessage
      -- We fold the contents of the file into our event network for displaying strings
      -- to the user.
      ,               eCatOut
      , "Bye"     <$  eQuit
      ]

  -- We use 'performEvent_' if we don't care about the value returned by the IO action:
  performEvent_ $ (liftIO . putStrLn) <$> eWrite

  return eQuit

host :: (forall t m. SampleApp5 t m)
     -> IO ()
host myGuest =
  runSpiderHost $ do
    (eOpen, eOpenTriggerRef) <- newEventWithTriggerRef
    (eRead, eReadTriggerRef) <- newEventWithTriggerRef

    -- We _could_ use 'runPerformEventT' to deal with the 'PerformEventT' layer, and then
    -- use 'runHostFrame' as before, but then we'd have to deal with the other things that
    -- 'runPerformEventT' performs.
    --
    -- Instead, we'll use 'hostPerformEventT', which deals with all of that for us.
    -- If I can come up with a good example where I need to deviate from that, I'll add it
    -- to this set of examples.
    --
    -- The end result is that we'll get a tuple, containing the output of the guest
    -- application and a 'FireCommand'.
    (eQuit, FireCommand fire) <- hostPerformEventT $ runPostBuildT (myGuest eRead) eOpen
    -- The 'FireCommand' carries a function to use instead of 'fireEventsAndRead'.
    -- The new function is used to weave the IO actions into the event processing.
    -- The biggest change that we see as a user is that we get a list of return values
    -- from the 'fire' function (since multiple events might be happening per frame due
    -- to events being triggered by 'performEvent').

    hQuit  <- subscribeEvent eQuit

    liftIO $ hSetBuffering stdin LineBuffering

    -- We need to make a handful of changes to our main loop and auxiliary functions to
    -- deal with the removal of the write event and the new 'fire' function:
    let
      readPhase =
        -- We no longer have a write event to read
        readEvent hQuit >>= sequence

      handleOutputs lmQuit = do
        liftIO . putStrLn $ "lmQuit: " ++ show lmQuit
        -- We handle the outputs by quitting if any of our
        -- results where not 'Nothing'
        return $ any isJust lmQuit

      -- We need to put a type signature here so that we don't lose track of the fact that the function
      -- is polymorphic in 'a'.
      --
      -- We either need to be concrete about 'm' ('SpiderHost Global') and 't' ('SpiderTimeline Global'), or
      -- we need to move these functions out of the 'host' function - and then the type signatures get
      -- _really_ fun.
      fireAndProcess :: Ref (SpiderHost Global) (Maybe (EventTrigger (SpiderTimeline Global) a))
                     -> a
                     -> (SpiderHost Global) Bool
      fireAndProcess t v = do
        mETrigger <- liftIO $ readIORef t
        lmQuit <- case mETrigger of
          Nothing ->
            -- We change our default value to reflect that
            -- we are now returning a list
            return []
          Just eTrigger ->
            fire [eTrigger :=> Identity v] readPhase
        handleOutputs lmQuit

      fireProcessAndLoop t v k = do
        quit <- fireAndProcess t v
        unless quit
          k

      loop = do
        input <- liftIO getLine
        fireProcessAndLoop eReadTriggerRef input loop

    fireProcessAndLoop eOpenTriggerRef () loop

{-
handleOutputs :: MonadIO m
              => [Maybe ()]
              -> m Bool
handleOutputs lmQuit = do
  liftIO . putStrLn $ "lmQuit: " ++ show lmQuit
  -- We handle the outputs by quitting if any of our
  -- results where not 'Nothing'
  return $ any isJust lmQuit

fireAndProcess :: ( Reflex t
                  , MonadReflexHost t m
                  , MonadReadEvent t (ReadPhase m)
                  , MonadRef m
                  , Ref m ~ Ref IO
                  , MonadIO m
                  ) => FireCommand t m
                    -> EventHandle t ()
                    -> Ref m (Maybe (EventTrigger t a))
                    -> a
                    -> m Bool
fireAndProcess (FireCommand fire) hQuit t v = do
  mETrigger <- liftIO $ readIORef t
  lmQuit <- case mETrigger of
    Nothing ->
      -- We change our default value to reflect that
      -- we are now returning a list
      return []
    Just eTrigger ->
      -- fire [eTrigger :=> Identity v] (readPhase hQuit)
      fire [eTrigger :=> Identity v] (readEvent hQuit >>= sequence)
  handleOutputs lmQuit

fireProcessAndLoop :: ( Reflex t
                      , MonadReflexHost t m
                      , MonadReadEvent t (ReadPhase m)
                      , MonadRef m
                      , Ref m ~ Ref IO
                      , MonadIO m
                      ) => FireCommand t m
                        -> EventHandle t ()
                        -> Ref m (Maybe (EventTrigger t a))
                        -> a
                        -> m ()
                        -> m ()
fireProcessAndLoop fc hQuit t v k = do
  quit <- fireAndProcess fc hQuit t v
  unless quit
    k
-}

go5 :: IO ()
go5 =
  host guest
