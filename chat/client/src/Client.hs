{-# LANGUAGE OverloadedStrings, QuasiQuotes, RecursiveDo, LambdaCase, ScopedTypeVariables #-}
module Client where

import Common.Api

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.Aeson
import Data.Either
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
main = mainWidgetWithHead headTag bodyTag

headTag :: MonadWidget t m => m ()
headTag = forM_ [ "//maxcdn.bootstrapcdn.com/font-awesome/4.5.0/css/font-awesome.min.css"
                , "//maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css"
                ] $ \x -> elAttr "link" ("rel" =: "stylesheet" <> "href" =: x) $ return ()

bodyTag :: MonadWidget t m => m ()
bodyTag = elAttr "div" flexContainer $ do
  rec (nick, newChannel, chats, chat) <- elAttr "div" flexNav $ chatSettings newDm
      newDm <- elAttr "div" flexContent $ do
        rec let wsUp = mconcat [ fmapMaybe (fmap $ (:[]) . Up_Message) $ attachWith ($) (message <$> current nick <*> current chat) send
                               , fmapMaybe (fmap $ (:[]) . Up_RemoveNick) (tag (current nick) $ updated nick)
                               , fmapMaybe (fmap $ (:[]) . Up_AddNick) (updated nick)
                               , attachWithMaybe (\n c -> fmap ((:[]) . Up_JoinChannel c) n) (current nick) newChannel
                               -- TODO Rejoin channels when nick changes
                               -- TODO Remove channel subscriptions when nick changes
                               ]
            wsDown <- openWebSocket wsUp
            let newMsg = fmapMaybe (\x -> case x of Just (Down_Message e) -> Just e; _ -> Nothing) wsDown
                newDm = attachWithMaybe (\isNew m -> if isNew m then Just m else Nothing) (dmIsNew <$> current nick <*> current chats) newMsg
            messages <- foldDyn (\n ms -> reverse $ n : reverse ms) [] newMsg
            listWithKey chats $ \k c -> do
              relevantMessages <- mapDyn (filter (\m -> Just True == (relevantMessage <$> k <*> pure m))) messages
              history relevantMessages =<< mapDyn (== k) chat
            send <- inputGroupWithButton ComponentSize_Medium "Enter message..." $ icon "paper-plane-o"
        return newDm
  return ()
  where
    flexContainer = "style" =: "display: flex;"
    flexNav = "style" =: "flex: 1 1 20%; order: 1;"
    flexContent = "style" =: "flex: 1 1 80%; order: 2; display: flex; flex-direction: column; margin-top: auto; overflow: hidden; height: 100vh;"

data Chat = Chat_DirectMessage Nick
          | Chat_Channel ChannelId
          deriving (Show, Read, Eq, Ord)

chatSettings
  :: MonadWidget t m
  => Event t (Envelope Message) -- ^ New DM
  -> m ( Dynamic t (Maybe Nick) -- ^ Current Nick
       , Event t ChannelId -- ^ New Channel event
       , Dynamic t (Map (Maybe Chat) Chat) -- ^ Connected Chats
       , Dynamic t (Maybe Chat) -- ^ Currently selected Chat
       )
chatSettings newDm = do
  n <- el "div" nickInput
  rec dm <- el "div" addDirectMessage
      dms <- foldDyn (\x -> Map.insert (Just $ Chat_DirectMessage x) (Chat_DirectMessage x)) Map.empty $ leftmost [dm, fmap (_message_from . _envelope_contents) newDm]
      dmClick <- listGroup dms dmSelection showChatName
      dmSelection <- holdDyn Nothing $ leftmost [dmClick, Nothing <$ cClick]
      c <- el "div" addChannel
      cs <- foldDyn (\x -> Map.insert (Just $ Chat_Channel x) (Chat_Channel x)) Map.empty c
      cClick <- listGroup cs cSelection showChatName
      cSelection <- holdDyn Nothing $ leftmost [cClick, Nothing <$ dmClick]
      chats <- combineDyn Map.union dms cs
      chat <- combineDyn (\r c -> case (r, c) of
        (Just r', _) -> r
        (_, Just c') -> c
        _ -> Nothing) dmSelection cSelection
  return (n, c, chats, chat)

dmIsNew :: Maybe Nick -> Map (Maybe Chat) Chat -> Envelope Message -> Bool
dmIsNew n cs m =
  let from = _message_from $ _envelope_contents m
      dm = isLeft $ _message_to $ _envelope_contents m
  in dm && Just from /= n && isNothing (Map.lookup (Just (Chat_DirectMessage from)) cs)

relevantMessage :: Chat -> Envelope Message -> Bool
relevantMessage c (Envelope _ m) = case c of
  Chat_DirectMessage n -> (isLeft (_message_to m) && n == _message_from m) || Left n == _message_to m
  Chat_Channel c -> Right c == _message_to m

showChatName :: Chat -> String
showChatName c = case c of
  Chat_DirectMessage n -> T.unpack $ unNick n
  Chat_Channel ch -> T.unpack $ unChannelId ch

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

history :: MonadWidget t m => Dynamic t [Envelope Message] -> Dynamic t Bool -> m ()
history msgs visible = do
  showHide <- mapDyn (\v -> if v then historyAttr else "style" =: "display: none;") visible
  (hEl, _) <- elDynAttr' "div" showHide $ simpleList msgs (el "div" . displayMessage)
  scroll <- delay 0.1 (updated msgs)
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

listGroup :: (Ord k, MonadWidget t m) => Dynamic t (Map k v) -> Dynamic t k -> (v -> String) -> m (Event t k)
listGroup as selection toString = divClass "list-group" $ selectViewListWithKey_ selection as $ \_ v s -> do
  style <- forDyn s $ \active -> "style" =: "cursor: pointer;" <> "class" =: ("list-group-item" <> if active then " active" else "")
  liftM (domEvent Click . fst) $ elDynAttr' "a" style $ dynText =<< mapDyn toString v

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
