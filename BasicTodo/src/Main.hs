{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}

{-
 - Stripped version of todo list: just add new todo and delete an old one
 -}

import           Control.Lens
import qualified Data.Map     as M
import qualified Data.Text    as T
import           Reflex
import           Reflex.Dom                  hiding (mainWidget)
import           Reflex.Dom.Core             (mainWidget)


type MM a = M.Map Int a

-- add a new value to a map, automatically choosing an unused key
new ::  a -> MM a -> MM a
new v m = case M.maxViewWithKey m of
    Nothing          -> [(0,v)] -- overloadedlists
    Just ((k, _), _) -> M.insert (succ k) v m

-- output the ul of the elements of the given map and return the delete
-- event for each key
ulW :: MonadWidget t m => Dynamic t (MM T.Text) -> m (Dynamic t (MM (Event t Int)))
ulW xs = elClass "ul" "list" $ listWithKey xs $ \k x -> elClass "li" "element" $ do
    dynText x -- output the text
    fmap (const k) <$> elClass "div" "delete" (button "x")
    -- tag the event of button press with the key of the text

-- output an input text widget with auto clean on return and return an
-- event firing on return containing the string before clean
inputW :: MonadWidget t m => m (Event t T.Text)
inputW = do
    rec let send = ffilter (==13) $ view textInput_keypress input
            -- send signal firing on *return* key press
        input <- textInput $ def & setValue .~ fmap (const "") send
        -- textInput with content reset on send
    return $ tag (current $ view textInput_value input) send
    -- tag the send signal with the inputText value BEFORE resetting

-- circuit ulW with a MM String kept updated by new strings from the passed
-- event and deletion of single element in the MM
listW :: MonadWidget t m => Event t T.Text -> m ()
listW e = do
    rec xs <- foldDyn ($) M.empty $ mergeWith (.)
        -- live state, updated by two signals
            [   fmap new e -- insert a new text
            ,   switch . current $ zs  -- delete text at specific keys
            ]
        bs <- ulW xs -- delete signals from outputted state
        let zs = fmap (mergeWith (.) . map (fmap M.delete) . M.elems) bs
            -- merge delete events
    return ()

app :: forall t m. MonadWidget t m => m ()
app = el "div" $ inputW >>= listW

main :: IO ()
main = run $ mainWidget app

