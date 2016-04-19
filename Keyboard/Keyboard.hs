{-# LANGUAGE RecursiveDo, OverloadedLists, ScopedTypeVariables  #-}

{- 
 - Stripped version of todo list: just add new todo and delete an old one
 -}

import Reflex
import Reflex.Dom
import Control.Monad
import Control.Monad.Trans
import Control.Lens ((^.))
import qualified Data.List.NonEmpty
import GHCJS.DOM.Element (focus)
import GHCJS.DOM.HTMLTextAreaElement  (castToHTMLTextAreaElement ,getSelectionStart)

insertAt :: Int -> a -> [a] -> [a]
insertAt n c v = take n v ++ [c] ++ drop n v

fromListE :: Reflex t => [Event t a] -> Event t a
fromListE = fmap Data.List.NonEmpty.head . mergeList

performArg :: MonadWidget t m => (b -> IO a) -> Event t b -> m (Event t a)
performArg f x = performEvent (fmap (liftIO . f) x)


inputW  :: forall m t .MonadWidget t m => Event t Char -> m ()
inputW buttonE = do
    rec let newStringE = attachWith (\v (c,n) -> insertAt n c v) cur posCharE
            cur = current $ input ^. textInput_value  -- actual string 
            element = input ^. textInput_element --html element
            broken,working :: Char -> IO (Char,Int)
            broken c = ((,) c) <$> getSelectionStart (castToHTMLTextAreaElement element)
            working c =  return (c,100) 
        input <- textInput $ def & setValue .~ newStringE 
        posCharE <- performArg working buttonE
    void $ performArg (const $ focus element) buttonE -- keep the focus right

keys :: MonadWidget t m => m [Event t Char]
keys = forM "qwerty" $ \c -> fmap (const c) <$> button [c]

main = mainWidget $ el "div" $ elClass "div" "keys" keys >>= inputW . fromListE

