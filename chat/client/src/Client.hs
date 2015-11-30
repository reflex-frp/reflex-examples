{-# LANGUAGE OverloadedStrings, QuasiQuotes, RecursiveDo, LambdaCase, ScopedTypeVariables #-}
module Client where

import Common.Api

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.Aeson
import Data.Map (Map)
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import Data.Text.Encoding
import Data.Time.Format
import GHCJS.DOM.Element
import Reflex.Dom
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import qualified Data.Text as T

main :: IO ()
main = mainWidgetWithHead headTag $ elAttr "div" flexContainer $ do
  (nick, newChannel, chats, chat) <- elAttr "div" flexNav chatSettings
  elAttr "div" flexContent $ do
    rec let wsUp = mconcat [ fmapMaybe (fmap $ (:[]) . Up_Message) $ attachWith ($) (message <$> current nick <*> current chat) send
                           , fmapMaybe (fmap $ (:[]) . Up_RemoveNick) (tag (current nick) $ updated nick)
                           , fmapMaybe (fmap $ (:[]) . Up_AddNick) (updated nick)
                           , attachWithMaybe (\n c -> fmap ((:[]) . Up_JoinChannel c) n) (current nick) newChannel
                           -- TODO Rejoin channels when nick changes
                           -- TODO Remove channel subscriptions when nick changes
                           ]
        wsDown <- openWebSocket wsUp
        let newMsg = fmapMaybe (\x -> case x of Just (Down_Message e) -> Just e; _ -> Nothing) wsDown
        listWithKey chats $ \k c -> do
          let relevantMessage = ffilter (\m -> Just True == (chatMessage <$> k <*> pure m)) newMsg
          history relevantMessage =<< mapDyn (== k) chat
        send <- inputGroupWithButton ComponentSize_Medium "Enter message..." $ icon "paper-plane-o"
    return ()
  where
    flexContainer = "style" =: "display: flex;"
    flexNav = "style" =: "flex: 1 1 20%; order: 1;"
    flexContent = "style" =: "flex: 1 1 80%; order: 2; display: flex; flex-direction: column; margin-top: auto; overflow: hidden; height: 100vh;"

headTag :: MonadWidget t m => m ()
headTag = forM_ [ "//maxcdn.bootstrapcdn.com/font-awesome/4.5.0/css/font-awesome.min.css"
                , "//maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css"
                ] $ \x -> elAttr "link" ("rel" =: "stylesheet" <> "href" =: x) $ return ()

data Chat = Chat_DirectMessage Nick
          | Chat_Channel ChannelId
          deriving (Show, Read, Eq, Ord)

chatSettings :: MonadWidget t m => m ( Dynamic t (Maybe Nick) -- ^ Current Nick
                                     , Event t ChannelId -- ^ New Channel event
                                     , Dynamic t (Map (Maybe Chat) Chat) -- ^ Connected Chats
                                     , Dynamic t (Maybe Chat) -- ^ Currently selected Chat
                                     )
chatSettings = do
  n <- el "div" nickInput
  rec (_, selDm, r, dms) <- listGroupWithInsert addDirectMessage (T.unpack . unNick) selC
      (newC, selC, c, cs) <- listGroupWithInsert addChannel (T.unpack . unChannelId) selDm
  let dmsToChats = Map.fromList . map (\(k, v) -> (fmap Chat_DirectMessage k, Chat_DirectMessage v)) . Map.toList
      csToChats = Map.fromList . map (\(k, v) -> (fmap Chat_Channel k, Chat_Channel v)) . Map.toList
  chats <- combineDyn (\dms' cs' -> Map.union (dmsToChats dms') (csToChats cs')) dms cs
  chat <- combineDyn (\r c -> case (r, c) of
    (Just r', _) -> Just $ Chat_DirectMessage r'
    (_, Just c') -> Just $ Chat_Channel c'
    _ -> Nothing) r c
  return (n, newC, chats, chat)

chatMessage :: Chat -> Envelope Message -> Bool
chatMessage c (Envelope _ m) = case c of
  Chat_DirectMessage n -> n == _message_from m || Left n == _message_to m
  Chat_Channel c -> Right c == _message_to m

chatDestination :: Chat -> Destination
chatDestination c = case c of
  Chat_DirectMessage n -> Left n
  Chat_Channel chan -> Right chan

message :: Maybe Nick -> Maybe Chat -> String -> Maybe Message
message sender chat msg = Message <$> sender <*> (chatDestination <$> chat) <*> validateNonBlank (T.pack msg)

nickInput :: MonadWidget t m => m (Dynamic t (Maybe Nick))
nickInput = do
  n <- inputGroupWithButton ComponentSize_Small "Set Nick" $ text "Set"
  nick <- holdDyn Nothing $ fmap (validNick . T.pack) n
  (nickMsgAttr, nickMsg) <- splitDyn <=< forDyn nick $ \case
    Nothing -> ("style" =: "color: red;", "No nickname set!")
    Just n -> ("style" =: "color: green;", "Hi, " <> (T.unpack $ unNick n) <> "!")
  elDynAttr "small" nickMsgAttr $ dynText nickMsg
  return nick

addDirectMessage :: MonadWidget t m => m (Event t Nick)
addDirectMessage = do
  addDM <- inputGroupWithButton ComponentSize_Small "Add DM" $ icon "plus"
  return $ fmapMaybe (validNick . T.pack) addDM

addChannel :: MonadWidget t m => m (Event t ChannelId)
addChannel = do
  addC <- inputGroupWithButton ComponentSize_Small "Add Channel" $ icon "plus"
  return $ fmapMaybe (fmap ChannelId . validateNonBlank . T.pack) addC

validNick :: Text -> Maybe Nick
validNick = fmap Nick . validateNonBlank

displayMessage :: MonadWidget t m => Dynamic t (Envelope Message) -> m ()
displayMessage em = do
  t <- mapDyn _envelope_time em
  m <- mapDyn _envelope_contents em
  let timestampFormat = formatTime defaultTimeLocale "%r"
  elAttr "span" ("style" =: "color: lightgray; font-family: monospace;") $ dynText =<< mapDyn (\x -> "(" <> timestampFormat x <> ") ") t
  elAttr "span" ("style" =: "color: red;") $ dynText =<< mapDyn ((<>": ") . T.unpack . unNick . _message_from) m
  el "span" $ dynText =<< mapDyn (T.unpack . _message_body) m

history :: MonadWidget t m => Event t (Envelope Message) -> Dynamic t Bool -> m ()
history newMsg visible = do
  showHide <- mapDyn (\v -> if v then historyAttr else "style" =: "display: none;") visible

  msgs <- foldDyn (\new old -> reverse $ new : reverse old) [] newMsg
  (hEl, _) <- elDynAttr' "div" showHide $ simpleList msgs (el "div" . displayMessage)
  scroll <- delay 0.1 newMsg
  performEvent_ $ fmap (\_ -> let h = _el_element hEl in liftIO $ elementSetScrollTop h =<< elementGetScrollHeight h) scroll
  where
    historyAttr = "style" =: "overflow: auto; height: calc(100% - 34px);"

data ComponentSize = ComponentSize_Small
                   | ComponentSize_Medium
                   | ComponentSize_Large
                   deriving (Show, Read, Eq, Ord, Enum, Bounded)

inputSize c = case c of
  ComponentSize_Small -> "input-sm"
  ComponentSize_Medium -> ""
  ComponentSize_Large -> "input-lg"

buttonSize c = case c of
  ComponentSize_Small -> "btn-sm"
  ComponentSize_Medium -> ""
  ComponentSize_Large -> "btn-lg"
 
icon :: MonadWidget t m => String -> m ()
icon x = elClass "i" ("fa fa-" <> x) $ return ()

buttonClass :: MonadWidget t m => String -> m a -> m (Event t ())
buttonClass klass child = liftM (domEvent Click . fst) $ elAttr' "button" ("type" =: "button" <> "class" =: klass) child

inputGroupWithButton :: MonadWidget t m => ComponentSize -> String -> m () -> m (Event t String)
inputGroupWithButton sz placeholder buttonChild = do
  divClass "input-group" $ do
    rec t <- textInput $ def & attributes .~ (constDyn $ "class" =: ("form-control " <> inputSize sz) <> "placeholder" =: placeholder)
                             & setValue .~ ("" <$ submit)
        b <- elClass "span" "input-group-btn" $ buttonClass ("btn btn-default " <> buttonSize sz) buttonChild
        let submit = tag (current (value t)) $ leftmost [textInputGetEnter t, b]
    return submit

validateNonBlank :: Text -> Maybe Text
validateNonBlank t = if T.null (T.strip t) then Nothing else Just t

listGroupWithInsert :: (Ord a, MonadWidget t m) => m (Event t a) -> (a -> String) -> Event t b -> m ( Event t a -- ^ New item
                                                                                                    , Event t (Maybe a) -- ^ Selection Event
                                                                                                    , Dynamic t (Maybe a) -- ^ Selected item
                                                                                                    , Dynamic t (Map (Maybe a) a) -- ^ Items
                                                                                                    )
listGroupWithInsert input toString deselect = do
  a <- el "div" input
  as <- foldDyn (\x -> Map.insert (Just x) x) Map.empty a
  rec aSel <- divClass "list-group" $ selectViewListWithKey_ aSel' as $ \_ v s -> do
        style <- forDyn s $ \active -> "style" =: "cursor: pointer;" <> "class" =: ("list-group-item" <> if active then " active" else "")
        liftM (domEvent Click . fst) $ elDynAttr' "a" style $ dynText =<< mapDyn toString v
      aSel' <- holdDyn Nothing $ leftmost [fmap (const Nothing) deselect, aSel, fmap Just a]
  return (a, leftmost [aSel, fmap Just a], aSel', as)

openWebSocket :: MonadWidget t m => Event t [Up] -> m (Event t (Maybe Down))
openWebSocket wsUp = do
  wv <- askWebView
  host <- liftIO $ getLocationHost wv
  protocol <- liftIO $ getLocationProtocol wv
  let wsProtocol = case protocol of
                     "file:" -> "ws:"
                     "http:" -> "ws:"
                     "https:" -> "wss:"
                     _ -> error "Unrecognized protocol: " <> protocol
      wsHost = case protocol of
                 "file:" -> "localhost:8000"
                 _ -> host
  ws <- webSocket (wsProtocol <> "//" <> wsHost <> "/api") $ def
    & webSocketConfig_send .~ fmap (fmap (LBS.toStrict . encode)) wsUp
  return $ fmap (decode' . LBS.fromStrict)$ _webSocket_recv ws
