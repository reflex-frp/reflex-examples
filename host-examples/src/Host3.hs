{-
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE RankNTypes #-}
module Host3 (
    go3
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

-- I'm going to assume that you've read through Host2.hs prior to this.

-- We're going to introduce a more complex interface to our application in this example.

-- We have our input events:
data Input t = Input {
    -- where ieOpen fires when the application starts
    ieOpen :: Event t ()
    -- and ieRead fires whenever the user enters a line of text
  , ieRead :: Event t String
  }

-- We also have output events:
data Output t = Output {
    -- where oeWrite is fired to signal that we should print a line of text to the screen
    oeWrite :: Event t String
    -- and ieQuit is fired to signal that we should exit the application
  , oeQuit  :: Event t ()
  }

-- Our new application type connects these together.
type SampleApp3 t m =
  ( Reflex t
  , MonadHold t m
  ) => Input t
    -> m (Output t)

-- This leads to our first sample application that isn't indisputably 100% boring.
guest :: SampleApp3 t m
guest (Input eOpen eRead) = do
  let
    -- If the user types something other than "/quit", we interpret that as a message.
    eMessage =       ffilter (/= "/quit") eRead
    -- If the user types "/quit", we should probably exit.
    eQuit    = () <$ ffilter (== "/quit") eRead

    -- We'll be polite, and issue greeting and parting messages to the user.
    -- Other than that we'll just be echoing their input up until they quit.
    -- Perhaps it's 99% boring, but it's progress.
    eWrite   = leftmost [
        "Hi"      <$  eOpen
      , ("> " ++) <$> eMessage
      , "Bye"     <$  eQuit
      ]

  return $ Output eWrite eQuit

-- This is the code that runs our FRP applications.
host :: (forall t m. SampleApp3 t m)
     -> IO ()
host myGuest =
  runSpiderHost $ do
    (eOpen, eOpenTriggerRef) <- newEventWithTriggerRef
    (eRead, eReadTriggerRef) <- newEventWithTriggerRef

    Output eWrite eQuit <- runHostFrame $ myGuest $ Input eOpen eRead

    hWrite <- subscribeEvent eWrite
    hQuit  <- subscribeEvent eQuit

    liftIO $ hSetBuffering stdin LineBuffering

    -- Everything up to here should be familiar.

    -- The rest of the code is similar to what we same in Host2.hs, but
    -- this time I've refactored a bit so that I don't have to repeat
    -- myself as much.

    -- The plan is that we're going to fire the 'eOpen' event, and then
    -- enter into a loop where we read a line from the user, then fire
    -- the 'eRead' event over and over.
    --
    -- We're trying to write a general host here, so we can't assume
    -- anything about the output events that we're listening to.
    --
    -- It could be the case that we fire the 'eOpen' event and the
    -- 'eQuit' event is fired immediately in return.  We have to handle
    -- anything that the deranged mind of a user might throw at us.
    --
    -- For this particular host, we want to respond in the same way to
    -- the events that we read after firing either of the input events.
    -- To that end, we separate out the common bits.
    let
      -- We have a piece of code that reads from the event handles:
      readPhase = do
        -- This version of the host reads from both events and
        -- returns both values, regardless of what they are.
        mWrite <- readEvent hWrite >>= sequence
        mQuit  <- readEvent hQuit >>= sequence
        -- If it matched what we wanted from our host, we could have
        -- read from the quit event and suppressed the results of the
        -- write event if the quit event had fired at the same time.
        --
        -- It seems arbitrary here, but in other domains it could be
        -- just what you want to prevent a write-after-close problem.

        return (mWrite, mQuit)

      -- We have a piece of code that responds to our output events:
      handleOutputs mWrite mQuit = do
        case mWrite of
          Nothing -> return ()
          -- If we had a write event, print the 'String' value from
          -- event:
          Just w -> liftIO . putStrLn $ w
        -- We can do this a little more simply with:
        --   forM_ mWrite $ liftIO . putStrLn

        -- Convert the occurrence of the quit event into a 'Bool':
        return $ isJust mQuit

      -- We have a piece of code to fire an event and deal with the
      -- response from the output events.
      fireAndProcess t v = do
        mETrigger <- liftIO $ readIORef t
        (mWrite, mQuit) <- case mETrigger of
          Nothing ->
            return (Nothing, Nothing)
          Just eTrigger ->
            fireEventsAndRead [eTrigger :=> Identity v] readPhase
        -- This will return a 'Bool' indicating whether the quit event
        -- has fired.
        handleOutputs mWrite mQuit

      -- We have a piece of code that uses that to guard some kind
      -- of continuation:
      fireProcessAndLoop t v k = do
        quit <- fireAndProcess t v
        unless quit
          k

      -- We use 'fireProcessAndLoop' to define our main event loop:
      loop = do
        input <- liftIO getLine
        fireProcessAndLoop eReadTriggerRef input loop


    -- and we also use it to fire off our open event and start the
    -- event loop:
    fireProcessAndLoop eOpenTriggerRef () loop

-- Now we can run our sample application ('guest') using
-- our code for hosting this kind of applications ('host').
go3 :: IO ()
go3 =
  host guest
