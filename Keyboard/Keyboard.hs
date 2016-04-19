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
import GHCJS.DOM.HTMLTextAreaElement  (castToHTMLTextAreaElement ,getSelectionStart)
import GHCJS.DOM.Types (HTMLInputElement)

insertAt :: Int -> a -> [a] -> [a]
insertAt n c v = take n v ++ [c] ++ drop n v

fromListE :: Reflex t => [Event t a] -> Event t a
fromListE = fmap Data.List.NonEmpty.head . mergeList

performArg :: MonadWidget t m => (b -> IO a) -> Event t b -> m (Event t a)
performArg f x = performEvent (fmap (liftIO . f) x)


-- inputW  :: forall m t .MonadWidget t m => Event t Char -> m ()
inputW track buttonE = do
    rec let newStringE = attachWith (\v (c,n) -> insertAt n c v) cur posCharE
            cur = current $ input ^. textInput_value  -- actual string 
            element = input ^. textInput_element --html element
        input <- textInput $ def & setValue .~ newStringE 
        posCharE <- performArg (track element) buttonE
    void $ performArg (const $ focus element) buttonE -- keep the focus right

keys :: MonadWidget t m => m [Event t Char]
keys = forM "qwerty" $ \c -> fmap (const c) <$> button [c]

broken,working :: HTMLInputElement -> Char -> IO (Char,Int)
broken e c = ((,) c) <$> getSelectionStart (castToHTMLTextAreaElement e)
working e c =  return (c,100) 

main = mainWidget $ el "div" $ elClass "div" "keys" keys >>= inputW broken . fromListE

