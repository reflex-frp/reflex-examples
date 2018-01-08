{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}

{-
 - buttons + real keyboard both writing to a text box
 -}

import           Control.Monad               (void, forM)
import qualified Data.List.NonEmpty          as DL (head)
import           Data.Monoid                 ((<>))
import qualified Data.Text                   as T
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

performArg :: MonadWidget t m => (b -> JSM a) -> Event t b -> m (Event t a)
performArg f x = performEvent (fmap (liftJSM . f) x)

inputW :: forall m t . MonadWidget t m => Event t Char -> m ()
inputW buttonE = do
    rec let newStringE =
                attachWith (\v (c,n) -> (n + 1,insertAt n c v)) cur posCharE
            cur = current $ value input        -- actual string
            html = _textInput_element input    -- html element
        input <- textInput $ def & setValue .~ fmap snd newStringE
        posCharE :: Event t (Char,Int)
          <- performArg (\c -> (,) c <$> getSelectionStart html) buttonE
        _ <- delay 0.1 (fmap snd posCharE)
                >>= performArg (\n -> setSelectionStart html (n+ 1)
                >> setSelectionEnd html (n + 1))
    void $ performArg (const $ focus html) buttonE -- keep the focus right

keys :: MonadWidget t m => m [Event t Char]
keys = forM "qwerty" $ \c -> fmap (const c) <$> button [c] -- OverloadedLists

app :: forall t m. MonadWidget t m => m ()
app = el "div" $ elClass "div" "keys" keys >>= inputW . fromListE

main :: IO ()
main = run $ mainWidget app


