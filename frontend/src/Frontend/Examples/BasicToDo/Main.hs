{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Frontend.Examples.BasicToDo.Main where
{-
 - Stripped version of todo list: just add new todo and delete an old one
 -}

import           Control.Lens
import qualified Data.Map     as M
import qualified Data.Text    as T
import           Reflex
import           Reflex.Dom                  hiding (mainWidget)
import           Reflex.Dom.Core             (mainWidget)
import           Control.Monad.Fix (MonadFix)


type MM a = M.Map Int a

-- add a new value to a map, automatically choosing an unused key
new :: a -> MM a -> MM a
new v m = case M.maxViewWithKey m of
    Nothing          -> [(0,v)] -- overloadedlists
    Just ((k, _), _) -> M.insert (succ k) v m

-- output the ul of the elements of the given map and return the delete
-- event for each key
ulW :: (DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m)
  => Dynamic t (MM T.Text) -> m (Dynamic t (MM (Event t Int)))
ulW xs = elClass "ul" "list" $ listWithKey xs $ \k x -> elClass "li" "element" $ do
    dynText x -- output the text
    fmap (const k) <$> elClass "div" "delete" (button "x")
    -- tag the event of button press with the key of the text

-- output an input text widget with auto clean on return and return an
-- event firing on return containing the string before clean
inputW :: (DomBuilder t m, MonadFix m) => m (Event t T.Text)
inputW = do
    rec let send = keypress Enter input
            -- send signal firing on *return* key press
        input <- inputElement $ def
          & inputElementConfig_setValue .~ fmap (const "") send
          & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
            ("placeholder" =: "Write task and press enter")
        -- inputElement with content reset on send
    return $ tag (current $ _inputElement_value input) send
    -- tag the send signal with the inputText value BEFORE resetting

-- circuit ulW with a MM String kept updated by new strings from the passed
-- event and deletion of single element in the MM
listW :: (DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m)
  => Event t T.Text -> m ()
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

app :: (DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m) => m ()
app = el "div" $ inputW >>= listW

main :: IO ()
main = run $ mainWidget app

