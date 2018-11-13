{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE FlexibleContexts      #-}

module Frontend.Examples.ScreenKeyboard.Main where

{-
 - buttons + real keyboard both writing to a text box
 -}

import           Control.Monad               (forM)
import           Control.Monad.Fix (MonadFix)
import qualified Data.List.NonEmpty          as DL (head)
import           Data.Monoid                 ((<>))
import qualified Data.Text                   as T
import           Data.Text (Text)
import           GHCJS.DOM.HTMLElement       (focus)
import           GHCJS.DOM.HTMLInputElement  hiding (setValue)
import           Language.Javascript.JSaddle
import           Reflex
import           Reflex.Dom                  hiding (mainWidget)
import           Reflex.Dom.Core             (mainWidget)

-- import Language.Javascript.JSaddle.Warp


insertAt :: Int -> Char -> T.Text -> T.Text
insertAt n c v = T.take n v <> T.singleton c <> T.drop n v

fromListE :: Reflex t => [Event t a] -> Event t a
fromListE = fmap DL.head . mergeList

performArg :: (PerformEvent t m, MonadJSM (Performable m))
  => (b -> JSM a) -> Event t b -> m (Event t a)
performArg f x = performEvent (fmap (liftJSM . f) x)

inputW
  :: ( DomBuilder t m
     , Prerender js m
     , PerformEvent t m
     , MonadFix m
     , TriggerEvent t m
     )
  => Event t Char
  -> m ()
inputW buttonE = do
  rec
    let
      html = _inputElement_raw input     -- html element
      cur = current $ value input        -- actual string
    input <- inputElement $ def
      & inputElementConfig_setValue .~ fmap snd newStringE
    newStringE <- doStuff cur html buttonE
  return ()

doStuff
  :: ( DomBuilder t m
     , Prerender js m
     , PerformEvent t m
     , TriggerEvent t m
     )
  => Behavior t Text
  -> RawInputElement (DomBuilderSpace m)
  -> Event t Char
  -> m (Event t (Int, Text))
doStuff cur html buttonE = do
  posCharE :: Event t (Char, Int) <- prerender (return never) $ do
    ev <- performArg (\c -> (,) c <$> getSelectionStart html) buttonE
    delay 0.1 (fmap snd ev)
      >>= performArg (\n -> setSelectionStart html (n+ 1)
      >> setSelectionEnd html (n + 1))
    performArg (const $ focus html) buttonE -- keep the focus right
    return ev
  let
    newStringE = attachWith (\v (c, n) -> (n + 1, insertAt n c v)) cur posCharE
  return newStringE

keys :: DomBuilder t m => m [Event t Char]
keys = forM "qwerty" $ \c -> fmap (const c) <$> button [c] -- OverloadedLists

app
  :: ( DomBuilder t m
     , MonadFix m
     , PerformEvent t m
     , TriggerEvent t m
     , Prerender js m
     )
  => m ()
app = el "div" $ elClass "div" "keys" keys >>= inputW . fromListE

main :: IO ()
main = run $ mainWidget app


