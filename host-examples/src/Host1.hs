{-
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE RankNTypes #-}
module Host1 (
    go1
  ) where

import Control.Monad (forever)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Identity (Identity(..))
import Control.Monad.IO.Class (liftIO)
import Data.IORef (readIORef)
import System.IO

import Data.Dependent.Sum

import Reflex
import Reflex.Host.Class

-- First we define a type for our applications.
--
-- In this case, our applications will take an
-- 'Event t String' as input return a
-- 'Behavior t Int' as output.
--
-- While we're at it, we capture various
-- typeclass constraints that we know we're
-- going to need in this type synonym.
type SampleApp1 t m =
  ( Reflex t
  , MonadHold t m
  , MonadFix m
  ) => Event t String
    -> m (Behavior t Int)

-- This is our sample FRP application.
--
-- It doesn't care what kind of event it gets
-- as an input, because we're just using it to
-- count the events that are occurring.
guest :: SampleApp1 t m
guest e = do
  -- increment every time the input event fires
  d <- foldDyn (+) 0 (1 <$ e)
  -- return the running count as a behavior
  return $ current d

-- This is the code that runs our FRP applications.
host :: (forall t m. SampleApp1 t m)
     -> IO ()
host myGuest =
  -- We use the Spider implementation of Reflex.
  runSpiderHost $ do

    -- We create a new event and a trigger for the event.
    (e, eTriggerRef) <- newEventWithTriggerRef
    -- e           :: Event t a
    -- eTriggerRef :: Ref m (Maybe (EventTrigger t a))
    --
    -- This gives us an event - which we need so that
    -- we can provide an input to 'myGuest' - and an event
    -- trigger.
    --
    -- 'Ref' is an abstraction over things like 'IORef' etc..
    --
    -- If the event isn't being used - or if it stops
    -- being used due to changes in the network - the 'Ref' will
    -- hold 'Nothing'.
    --
    -- If something is interested in the event, then the 'Ref'
    -- will hold 'Just t' where 't' is a trigger for the event.

    -- Now we set up our basic event network for use with 'myGuest e'.
    b <- runHostFrame $ myGuest e
    -- This will give us a 'Behavior Int' which we'll use a little later.

    -- At this point the event network is set up, but there are no
    -- events firing and so nothing much is happening.
    --
    -- We address that by putting together an event loop to handle
    -- the firing of the event we are intersted in.
    --
    -- In this case we're just going to read lines from stdin
    -- and fire our event with the resulting 'String' values.

    -- First we make sure stdin is buffering things by line.
    liftIO $ hSetBuffering stdin LineBuffering
    -- then we start our loop:
    forever $ do
      -- We get a line from stdin
      input <- liftIO getLine
      -- and we print some debugging output, just to show that we
      -- do things like that with no ill effect
      liftIO $ putStrLn $ "Input Event: " ++ show input

      -- Now we read the reference holding our trigger
      mETrigger <- liftIO $ readIORef eTriggerRef
      case mETrigger of
        -- If the value is 'Nothing', then the guest FRP network
        -- doesn't care about this event at the moment, so we do nothing.
        Nothing -> do
          return ()
        -- In other host settings, where we have events that might be
        -- expensive to handle from the host side, we might read the
        -- reference first and then skip the expensive operation when
        -- no one is listening.

        -- If there is someone listening, we get hold of the trigger and
        -- use that to fire the events.
        Just eTrigger -> do
          -- fireEvents :: [DSum (EventTrigger t) Identity] -> m ()
          fireEvents [eTrigger :=> Identity input]
        -- 'DSum' comes from 'dependent-sum', and allows us to deal with
        -- collections of events with different types in a homogenous way,
        -- but without giving up type-safety.  It's really nifty, and worth
        -- playing around with if you have a moment.
        --
        -- At the moment we're only firing one event, so it's not that
        -- exciting.

      -- There is a helper function that reads the trigger reference and fires
      -- the trigger if it is not 'Nothing', so we could replace the above
      -- block with:
      -- fireEventRef eTriggerRef input

      -- After each time that we fire the events, we read the output
      -- 'Behavior'.  We do that using 'sample' - to get the current
      -- value of the 'Behavior' inside of the event network - and
      -- 'runHostFrame' - to cause the event network to process another
      -- moment in time so that we can get hold of that value on the
      -- outside of the event network.
      output <- runHostFrame $ sample b

      -- We'll print our output here
      liftIO $ putStrLn $ "Output Behavior: " ++ show output

-- Now we can run our sample application ('guest') using
-- our code for hosting this kind of applications ('host').
go1 :: IO ()
go1 =
  host guest
