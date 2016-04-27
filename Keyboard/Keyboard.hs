{-# LANGUAGE RecursiveDo, OverloadedLists, ScopedTypeVariables  #-}

{- 
 - buttons + real keyboard both writing to a text box
 -}

import Reflex
import Reflex.Dom
import Control.Monad
import Control.Monad.Trans
import Control.Lens ((^.))
import qualified Data.List.NonEmpty
import GHCJS.DOM.Element (focus)
import GHCJS.DOM.HTMLInputElement  hiding (setValue)-- (getSelectionStart,setSelectionStart,setSelectionEnd)
import GHCJS.DOM.Selection  -- (getSelectionStart,setSelectionStart,setSelectionEnd)
import GHCJS.DOM.Types (HTMLInputElement)

insertAt :: Int -> a -> [a] -> [a]
insertAt n c v = take n v ++ [c] ++ drop n v

fromListE :: Reflex t => [Event t a] -> Event t a
fromListE = fmap Data.List.NonEmpty.head . mergeList

performArg :: MonadWidget t m => (b -> IO a) -> Event t b -> m (Event t a)
performArg f x = performEvent (fmap (liftIO . f) x)


inputW  :: forall m t .MonadWidget t m => Event t Char -> m ()
inputW buttonE = do
    rec let newStringE = attachWith (\v (c,n) -> (n + 1,insertAt n c v)) cur posCharE
            cur = current $ input ^. textInput_value  -- actual string 
            element = input ^. textInput_element --html element
        input <- textInput $ def & setValue .~ fmap snd newStringE 
        posCharE :: Event t (Char,Int) <- performArg (\c -> ((,) c) <$> getSelectionStart element) buttonE
        delay 0.1 (fmap snd posCharE) >>= performArg (\n -> setSelectionStart element (n+ 1) >> setSelectionEnd element (n + 1)) 
    void $ performArg (const $ focus element) buttonE -- keep the focus right

keys :: MonadWidget t m => m [Event t Char]
keys = forM "qwerty" $ \c -> fmap (const c) <$> button [c]


main = mainWidget $ el "div" $ elClass "div" "keys" keys >>= inputW . fromListE

