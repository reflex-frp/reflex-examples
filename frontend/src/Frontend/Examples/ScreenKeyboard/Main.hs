{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeFamilies          #-}

module Frontend.Examples.ScreenKeyboard.Main where

{-
 - buttons + real keyboard both writing to a text box
 -}

import           Control.Monad               (forM, void)
import           Control.Monad.Fix           (MonadFix)
import qualified Data.List.NonEmpty          as DL (head)
import           Data.Monoid                 ((<>))
import qualified Data.Text                   as T
import           Data.Text (Text)
import           GHCJS.DOM.HTMLElement       (focus)
import           GHCJS.DOM.HTMLInputElement  hiding (setValue)
import           Language.Javascript.JSaddle
import           Reflex.Dom

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
     , PerformEvent t m
     , MonadFix m
     , MonadJSM (Performable m)
     , DomBuilderSpace m ~ GhcjsDomSpace
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
     , PerformEvent t m
     , MonadJSM (Performable m)
     )
  => Behavior t Text
  -> HTMLInputElement
  -> Event t Char
  -> m (Event t (Int, Text))
doStuff cur html buttonE = do
  posCharE :: Event t (Char, Int) <- do
    ev <- performArg (\c -> (,) c <$> getSelectionStart html) buttonE
    void $ (flip performArg) (fmap snd ev) $ \n -> do
      setSelectionStart html (n + 1)
      setSelectionEnd html (n + 1)
    void $ performArg (const $ focus html) buttonE -- keep the focus right
    return ev
  let
    newStringE = attachWith (\v (c, n) -> (n + 1, insertAt n c v)) cur posCharE
  return newStringE

keys :: DomBuilder t m => m [Event t Char]
keys = forM "qwerty" $ \c -> fmap (const c) <$> button [c] -- OverloadedLists

app
  :: ( DomBuilder t m
     , Prerender js t m
     )
  => m ()
app = el "div" $ prerender_ blank $ elClass "div" "keys" keys >>= inputW . fromListE
