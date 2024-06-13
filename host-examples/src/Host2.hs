{-
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE RankNTypes #-}
module Host2 (
    go2
  ) where

import Data.Maybe (isJust)
import Control.Monad (unless)
import Control.Monad.Identity (Identity(..))
import Control.Monad.IO.Class (liftIO)
import Data.IORef (readIORef)
import System.IO

import Data.Dependent.Sum

import Reflex
import Reflex.Host.Class

-- I'm going to assume that you've read through Host1.hs prior to this.

-- We are going to update the type of our applications.
--
-- Previously we had a 'Behavior t Int' as an output, and now we have
-- an 'Event t ()' as an output.
--
-- In this case we're going to use that event to signal when the
-- application wants to stop, so that we can exit cleanly.
type SampleApp2 t m =
  ( Reflex t
  , MonadHold t m
  ) => Event t String
    -> m (Event t ())

-- This is our sample application.
--
-- Every time our input 'Event t String' fires, we're going to check
-- to see if the 'String' value is "/quit".
--
-- We return an event that fires when this is the case.
--
-- It's boring for now, but we'll build on it.
guest :: SampleApp2 t m
guest e = do
  let
    eQuit = () <$ ffilter (== "/quit") e
  return eQuit

-- This is the code that runs our FRP applications.
host :: (forall t m. SampleApp2 t m)
     -> IO ()
host myGuest =
  -- We use the Spider implementation of Reflex.
  runSpiderHost $ do

    -- We create a new event and a trigger for the event.
    (e, eTriggerRef) <- newEventWithTriggerRef

    -- We set up our basic event network to use with 'myGuest e'.
    eQuit <- runHostFrame $ myGuest e
    -- eQuit :: Event t ()
    -- This gives us an 'Event t ()' which signals the intent to quit.

    -- We want to be able to work out when that event has fired, so
    -- we subscribe to the event.
    hQuit <- subscribeEvent eQuit
    -- hQuit :: EventHandle t ()
    --
    -- This gives us an event handle, which we can use to read
    -- our output events.

    -- A little bit of set up:
    liftIO $ hSetBuffering stdin LineBuffering

    -- We define our main loop.
    --
    -- We're not using 'forever' anymore, because we want to be
    -- able to exit cleanly from this loop.

    let
      loop = do
        -- We get a line from stdin
        input <- liftIO getLine
        -- and we print it out for debugging purposes
        liftIO $ putStrLn $ "Input Event: " ++ show input

        -- We read the event trigger
        mETrigger <- liftIO $ readIORef eTriggerRef
        mQuit <- case mETrigger of
          -- If no one is listening, we do nothing
          Nothing -> do
            return Nothing

          -- If there is someone listening, we fire our input
          -- events and read from the output events.
          Just eTrigger -> do
            -- The firing of the events happens as usual, except:
            -- fireEventsAndRead :: [DSum (EventTrigger t) Identity] -> ReadPhase m a -> m a
            fireEventsAndRead [eTrigger :=> Identity input] $ do
              -- we now have a read phase that happens after the events have been fired.

              -- The main thing that we do in the 'ReadPhase' is call 'readEvent' and 
              -- deal with its output.

              -- The event may not be occurring, so there's a 'Maybe' in there:
              -- readEvent :: EventHandle t a -> m (Maybe (m a))
              mValue <- readEvent hQuit
              -- and we shuffle this into a form that we can use with 'sequence':
              sequence mValue

        -- Again, there is a helper functions that reads the trigger
        -- reference, fires the trigger if it is not 'Nothing', and then
        -- reads an output event from a particular event handle.
        --
        -- The above block could be replaced with:
        -- mQuit <- fireEventRefAndRead eTriggerRef input hQuit

        -- The result of this block is
        --   mQuit :: Maybe ()
        -- which has filtered up through a few layers to get to us, but is still
        -- perfectly serviceable.

        -- We print out the value for debugging purposes:
        liftIO $ putStrLn $ "Output Event: " ++ show mQuit
        -- and then use it to determine if we'll continue with the loop:
        unless (isJust mQuit)
          loop

    -- This starts the actual loop
    loop

-- Now we can run our sample application ('guest') using
-- our code for hosting this kind of applications ('host').
go2 :: IO ()
go2 =
  host guest
