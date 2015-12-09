{-# LANGUAGE OverloadedStrings, QuasiQuotes, RecursiveDo, LambdaCase, ScopedTypeVariables #-}
module Client where

import Common.Api

import Control.Lens
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.Aeson
import Data.Either
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import Data.Text.Encoding
import Data.Time.Format
import GHCJS.DOM.Element
import Reflex.Dom
import System.Random
import Text.RawString.QQ
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T

main :: IO ()
main = mainWidgetWithHead headTag bodyTag

headTag :: MonadWidget t m => m ()
headTag = do
  forM_ [ "//maxcdn.bootstrapcdn.com/font-awesome/4.5.0/css/font-awesome.min.css" --TODO Make these links local
        , "//maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css"
        ] $ \x -> elAttr "link" ("rel" =: "stylesheet" <> "href" =: x) $ return ()
  el "style" $ text customCss

bodyTag :: MonadWidget t m => m ()
bodyTag = divClass "flex-container" $ do
  rec (nick, newChannel, chats, chat) <- divClass "flex-nav" $ chatSettings newDm
      (newDm, wsOpen) <- divClass "flex-content" $ do
        rec let channels = rights . map chatDestination . Set.toList
                wsUp = mconcat [ fmapMaybe (fmap $ (:[]) . Up_Message) $ attachWith (\n (c, m) -> message n c m) (current nick) send
                               , fmap ((:[]) . Up_RemoveNick) (tag (current nick) $ updated nick)
                               , fmap ((:[]) . Up_AddNick) $ leftmost [updated nick, tag (current nick) $ traceEvent "wsOpen" wsOpen]
                               , tag ((\cs n -> map (\c -> Up_LeaveChannel c n) $ channels cs) <$> current chats <*> current nick) (updated nick)
                               -- ^ Leave Channels with old Nick when Nick changes
                               , attachDynWith (\n c -> [Up_JoinChannel c n]) nick newChannel
                               , attachWith (\cs n -> map (\c -> Up_JoinChannel c n) $ channels cs) (current chats) $ leftmost [updated nick, tag (current nick) wsOpen]
                               -- ^ Rejoin Channels with new Nick when Nick changes
                               ]
            (wsDown, wsOpen) <- openWebSocket wsUp
            let newMsg = fmapMaybe (\x -> case x of Just (Down_Message e) -> Just e; _ -> Nothing) wsDown
            messages <- foldDyn (\n ms -> reverse $ n : reverse ms) [] newMsg
            chats' <- mapDyn (Map.mapKeys Just . Map.fromSet id) chats
            sendMsgs <- listWithKey chats' $ \k c -> do
              relevantMessages <- mapDyn (filter (\m -> Just True == (relevantMessage <$> k <*> pure m))) messages
              msg <- history relevantMessages =<< mapDyn (== k) chat
              return $ fmap ((,) k) msg
            send <- liftM (switch . current) $ mapDyn (leftmost . Map.elems) sendMsgs
        return (newMsg, wsOpen)
  return ()

data Chat = Chat_DirectMessage Nick
          | Chat_Channel ChannelId
          deriving (Show, Read, Eq, Ord)

chatSettings
  :: forall t m. MonadWidget t m
  => Event t (Envelope Message) -- ^ New Message
  -> m ( Dynamic t Nick -- Current Nick
       , Event t ChannelId -- New Channel event
       , Dynamic t (Set Chat) -- Connected Chats
       , Dynamic t (Maybe Chat) -- Currently selected Chat
       )
chatSettings newMsg = divClass "chat-settings" $ do
  n <- el "div" nickInput
  rec (c, cs, cClick) <- chatSettingsGroup "CHANNELS" addChannel (Set.singleton $ ChannelId "reflex") never Chat_Channel cSelection
      (_, dms, dmClick) <- chatSettingsGroup "DIRECT MESSAGES" addDirectMessage Set.empty newDm Chat_DirectMessage dmSelection
      dms' <- mapDyn (Set.map Chat_DirectMessage) dms
      cs' <- mapDyn (Set.map Chat_Channel) cs
      chats <- combineDyn Set.union dms' cs'
      chat <- holdDyn Nothing $ leftmost [ fmap Chat_Channel <$> cClick
                                         , fmap Chat_DirectMessage <$> dmClick
                                         ]
      cSelection <- mapDyn (join . fmap ((^? _Right) . chatDestination)) chat
      dmSelection <- mapDyn (join . fmap ((^? _Left) . chatDestination)) chat
      let newDm = attachWithMaybe (\isNew m -> if isNew m then Just (Set.singleton $ _message_from $ _envelope_contents m) else Nothing) (dmIsNew <$> current n <*> current chats) newMsg
  return (n, c, chats, chat)
  where
    chatSettingsGroup :: (Ord a)
                      => String
                      -> (Event t () -> m (Event t a))
                      -> Set a
                      -> Event t (Set a)
                      -> (a -> Chat)
                      -> Dynamic t (Maybe a)
                      -> m ( Event t a
                           , Dynamic t (Set a)
                           , Event t (Maybe a)
                           )
    chatSettingsGroup heading addItem items0 newItems toChat sel = do
      rec new <- chatSettingsHeading heading items addItem
          items <- foldDyn Set.union items0 $ fmap Set.singleton new <> newItems
          let msg = \k -> fmap (const ()) $ ffilter (\m -> Just True == (relevantMessage <$> (toChat <$> k) <*> pure m)) newMsg
          items' <- mapDyn (Map.mapKeys Just . Map.fromSet toChat) items
          click <- listGroup items' sel msg chatListItem
      return (new, items, click)
    chatSettingsHeading :: String -> Dynamic t (Set a) -> (Event t () -> m (Event t b)) -> m (Event t b)
    chatSettingsHeading label xs child = divClass "nav-group-heading" $ do
      elClass "span" "nav-group-heading-label" $ text label
      text " ("
      display =<< mapDyn Set.size xs
      text ")"
      add <- liftM (domEvent Click) $ iconDyn (constDyn "plus-circle pull-right fa-fw nav-group-add")
      popup add never child

chatListItem :: MonadWidget t m => Dynamic t Chat -> Dynamic t Int -> m ()
chatListItem c n = do
  iconDyn =<< mapDyn (\c' -> if isLeft (chatDestination c') then "circle fa-fw fa-sm" else "hashtag fa-fw fa-sm") c
  text " "
  boldName <- mapDyn (\num -> if num > 0 then "style" =: "font-weight: bold;" else mempty) n
  elDynAttr "span" boldName $ dynText =<< mapDyn showChatName c
  dyn =<< mapDyn (\num -> if num > 0 then elClass "span" "badge" (text $ show num) else return ()) n
  return ()

dmIsNew :: Nick -> Set Chat -> Envelope Message -> Bool
dmIsNew n cs m =
  let from = _message_from $ _envelope_contents m
      dm = isLeft $ _message_to $ _envelope_contents m
  in dm && from /= n && (Chat_DirectMessage from `Set.notMember` cs)

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

message :: Nick -> Maybe Chat -> String -> Maybe Message
message sender chat msg = Message <$> pure sender <*> (chatDestination <$> chat) <*> validateNonBlank (T.pack msg)

nickInput :: MonadWidget t m => m (Dynamic t Nick)
nickInput = do
  anonNumber <- liftIO $ getStdRandom $ randomR (1::Integer, 1000000)
  let startingNick = Nick $ T.pack $ "anon-" <> show anonNumber
  rec editNick <- elClass "h4" "profile" $ do
        iconDyn =<< mapDyn (\n -> {- if isNothing n then "circle-o offline" else -} "circle online") nick
        dynText =<< mapDyn ((" "<>) . T.unpack . unNick) nick
        iconDyn (constDyn "pencil pull-right fa-fw nav-group-add")
      n <- popup (domEvent Click editNick) never $ \focus ->
        inputGroupWithButton ComponentSize_Small "Set Nick" focus $ text "Set"
      nick <- holdDyn startingNick $ fmapMaybe (validNick . T.pack) n
  return nick

addDirectMessage :: MonadWidget t m => Event t () -> m (Event t Nick)
addDirectMessage focus = do
  addDM <- inputGroupWithButton ComponentSize_Small "Add DM" focus $ icon "plus"
  return $ fmapMaybe (validNick . T.pack) addDM

addChannel :: MonadWidget t m => Event t () -> m (Event t ChannelId)
addChannel focus = do
  addC <- inputGroupWithButton ComponentSize_Small "Add Channel" focus $ icon "plus"
  return $ fmapMaybe (fmap ChannelId . validateNonBlank . T.pack) addC

validNick :: Text -> Maybe Nick
validNick = fmap Nick . validateNonBlank

displayMessage :: MonadWidget t m => Dynamic t (Envelope Message) -> m ()
displayMessage em = do
  t <- mapDyn _envelope_time em
  m <- mapDyn _envelope_contents em
  let timestampFormat = formatTime defaultTimeLocale "%r %Z"
  elClass "span" "message-timestamp" $ dynText =<< mapDyn (\x -> "(" <> timestampFormat x <> ") ") t
  elClass "span" "message-from" $ dynText =<< mapDyn ((<>": ") . T.unpack . unNick . _message_from) m
  elClass "span" "message-body" $ dyn =<< mapDyn (mapM displayMessageElement . messageElements . _message_body) m
  return ()

data MessageElement = MessageElement_Text Text
                    | MessageElement_Icon Text
                    deriving (Show, Read, Eq, Ord)

emoticons :: Map Text String
emoticons = Map.fromList [ (":)", "smile-o")
                         , (":(", "frown-o")
                         , (":|", "meh-o")
                         , ("<3", "heart")
                         ]

messageElements :: Text -> [MessageElement]
messageElements t = foldl findEmoticon' [MessageElement_Text t] (Map.keys emoticons)
  where
    findEmoticon' :: [MessageElement] -> Text -> [MessageElement]
    findEmoticon' ms e = case ms of
      [] -> []
      (x:xs) -> case x of
        MessageElement_Text t' -> findEmoticon e t' ++ findEmoticon' xs e
        _ -> x : findEmoticon' xs e
    findEmoticon e m = case T.breakOn e m of
      ("", "") -> []
      (_, "") -> [MessageElement_Text m]
      (pre, xs) -> let (match, post) = T.splitAt (T.length e) xs
                   in if T.null pre
                         then MessageElement_Icon match : findEmoticon e post
                         else MessageElement_Text pre : MessageElement_Icon match : findEmoticon e post

displayMessageElement :: MonadWidget t m => MessageElement -> m ()
displayMessageElement me = case me of
  MessageElement_Text t -> text $ T.unpack t
  MessageElement_Icon t -> case Map.lookup t emoticons of
    Just i -> icon i
    Nothing -> text $ T.unpack t

history :: MonadWidget t m => Dynamic t [Envelope Message] -> Dynamic t Bool -> m (Event t String)
history msgs visible = do
  (showMsgs, showInput) <- splitDyn =<< mapDyn (\v -> if v then (historyAttr, mempty) else (hidden, hidden)) visible
  (hEl, _) <- elDynAttr' "div" showMsgs $ simpleList msgs (el "div" . displayMessage)
  focus <- delay 0.1 $ fmap (const ()) $ ffilter id $ updated visible
  newMsg <- elDynAttr "div" showInput $ inputGroupWithButton ComponentSize_Medium "Enter message..." focus $ icon "paper-plane-o"
  scroll <- delay 0.1 (updated msgs)
  performEvent_ $ fmap (\_ -> let h = _el_element hEl in liftIO $ elementSetScrollTop h =<< elementGetScrollHeight h) scroll
  return newMsg
  where
    historyAttr = "class" =: "history"
    hidden = "style" =: "display: none;"

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
icon = void . iconDyn . constDyn

iconDyn :: MonadWidget t m => Dynamic t String -> m (El t)
iconDyn k = do
  i <- mapDyn (\x -> "class" =: ("fa fa-" <> x)) k
  liftM fst $ elDynAttr' "i" i $ return ()

buttonClass :: MonadWidget t m => String -> m a -> m (Event t ())
buttonClass klass child = liftM (domEvent Click . fst) $ elAttr' "button" ("type" =: "button" <> "class" =: klass) child

inputGroupWithButton :: MonadWidget t m => ComponentSize -> String -> Event t () -> m () -> m (Event t String)
inputGroupWithButton sz placeholder focus buttonChild = do
  divClass "input-group" $ do
    rec t <- textInput $ def & attributes .~ (constDyn $ "class" =: ("form-control " <> inputSize sz) <> "placeholder" =: placeholder)
                             & setValue .~ ("" <$ submit)
        b <- elClass "span" "input-group-btn" $ buttonClass ("btn btn-default " <> buttonSize sz) buttonChild
        let submit = tag (current (value t)) $ leftmost [textInputGetEnter t, b]
    performEvent_ $ fmap (\_ -> liftIO $ elementFocus $ _textInput_element t) focus
    return submit

validateNonBlank :: Text -> Maybe Text
validateNonBlank t = if T.null (T.strip t) then Nothing else Just t

listGroup :: (Ord k, MonadWidget t m) => Dynamic t (Map k v) -> Dynamic t k -> (k -> Event t ()) -> (Dynamic t v -> Dynamic t Int -> m ()) -> m (Event t k)
listGroup as selection msg child = divClass "list-group" $ selectViewListWithKey_ selection as $ \k v s -> do
  style <- forDyn s $ \active -> "style" =: "cursor: pointer;" <> "class" =: ("list-group-item" <> if active then " active" else "")
  n <- foldDyn ($) 0 $ leftmost [ fmap (\_ -> (+) 1) $ gate (not <$> current s) $ msg k
                                , fmap (\_ -> (const 0)) $ ffilter id (updated s)
                                ]
  liftM (domEvent Click . fst) $ elDynAttr' "a" style $ child v n

popup :: MonadWidget t m => Event t () -> Event t () -> (Event t () -> m (Event t a)) -> m (Event t a)
popup open close child = do
  rec newItem <- liftM (switch . current) $ widgetHold (return never) $
        leftmost [ fmap (\_ -> p) open
                 , fmap (\_ -> return never) newItem
                 -- TODO close with escape button
                 ]
  return $ fmapMaybe id newItem
  where
    p = elAttr "div" ("style" =: "position: relative;") $ divClass "dropdown-menu nav-group-popup" $ do
      closeBtn <- liftM (domEvent Click . fst) $ elAttr' "span" ("class" =: "fa-stack popup-close") $ do
        icon "circle fa-stack-1x popup-close-bg"
        icon "times-circle fa-stack-1x popup-close-fg"
      c <- child =<< getPostBuild
      return $ leftmost [fmap Just c, Nothing <$ close, Nothing <$ closeBtn]

gateDyn :: Reflex t => Dynamic t Bool -> Event t a -> Event t a
gateDyn d e = attachDynWithMaybe (\b a -> if b then Just a else Nothing) d e

openWebSocket :: MonadWidget t m => Event t [Up] -> m (Event t (Maybe Down), Event t ())
openWebSocket wsUp = do
  wv <- askWebView
  host <- getLocationHost wv
  protocol <- getLocationProtocol wv
  let wsProtocol = case protocol of
        "" -> "ws:" -- We're in GHC
        "about:" -> "ws:" -- We're in GHC
        "file:" -> "ws:"
        "http:" -> "ws:"
        "https:" -> "wss:"
        _ -> error $ "Unrecognized protocol: " <> show protocol
      wsHost = case protocol of
        "" -> "localhost:8000" -- We're in GHC
        "about:" -> "localhost:8000" -- We're in GHC
        "file:" -> "localhost:8000"
        _ -> host
  rec ws <- webSocket (wsProtocol <> "//" <> wsHost <> "/api") $ def
        & webSocketConfig_send .~ send
      websocketReady <- holdDyn False $ fmap (const True) $ _webSocket_open ws
      websocketNotReady <- mapDyn not websocketReady
      buffer <- foldDyn (++) [] $ gateDyn websocketNotReady wsUp
      let send = fmap (fmap (LBS.toStrict . encode)) $ leftmost [ gateDyn websocketReady wsUp
                                                                , tag (current buffer) (_webSocket_open ws)
                                                                ]
  return $ (fmap (decode' . LBS.fromStrict)$ _webSocket_recv ws, _webSocket_open ws)

customCss :: String
customCss = [r|
    .list-group-item {
      background-color: #4D394B;
      border: none;
    }
    a.list-group-item {
      color: #ab9ba9;
    }
    a.list-group-item:hover {
      background-color: #3E313C;
      border: none;
      color: #ab9ba9;
    }
    .list-group-item.active {
      background-color: #4C9689;
      border-color: #4C9689;
    }
    .list-group-item.active:hover {
      background-color: #4C9689;
      border-color: #4C9689;
    }
    .fa-sm {
      font-size: 75%;
    }
    .flex-container {
      |] <> displayFlex <> [r|
    }
    .flex-nav { |] <>
      flex 1 1 "220px" <>
      order 1 <> [r|
      background-color: #4D394B;
      min-width: 220px;
    }
    .flex-content { |] <>
      displayFlex <>
      flex 1 1 "100%" <>
      order 2 <> [r|
      flex-direction: column;
      -webkit-flex-direction: column;
      margin-top: auto;
      overflow: hidden;
      height: 100vh;
    }
    .chat-settings {
      height: 100vh;
      overflow-y: auto;
      overflow-x: hidden;
    }
    .message-timestamp {
      color: lightgray;
      font-family: monospace;
    }
    .message-from {
      color: red;
      font-weight: bold;
    }
    .message-body {
    }
    .nav-group-heading {
      color: #ab9ba9;
      padding-left: 15px;
    }
    .nav-group-heading-label {
      font-weight: bold;
    }
    .nav-group-add {
      color: #ab9ba9;
      padding-right: 20px;
      cursor: pointer;
    }
    .popup-close {
      cursor: pointer;
      position: absolute;
      right: -20px;
      top: -20px;
      font-size: 150%;
      z-index: 3;
    }
    .popup-close-bg {
      color: white;
      font-size: 110%;
    }
    .popup-close-fg {
      color: #4D394B;
    }
    .nav-group-popup {
      display: block;
      margin-right: 10px;
      box-shadow: 6px 12px 24px rgba(0,0,0,0.75);
    }
    .badge {
      background-color: #EB4D5C;
      color: white;
    }
    .profile {
      color: white;
      padding-top: 10px;
      padding-left: 15px;
      padding-bottom: 10px;
    }
    .offline {
      color: #ab9ba9;
    }
    .online {
      color: #38978D;
    }
    .history {
      overflow: auto;
      height: calc(100% - 34px);
    }
  |]
  where
    displayFlex = [r|
      display: -webkit-box;
      display: -moz-box;
      display: -ms-flexbox;
      display: -webkit-flex;
      display: flex;
    |]
    wrap v k = k <> ":" <> v <> ";"
    flex grow shrink basis =
      let gsb = show grow <> " " <> show shrink <> " " <> basis
      in unlines $ map (wrap gsb) [ "-webkit-box-flex"
                                  , "-moz-box-flex"
                                  , "-webkit-flex"
                                  , "-ms-flex"
                                  , "flex"
                                  ]
    order ix = unlines $ map (wrap (show ix)) [ "-webkit-box-ordinal-group"
                                              , "-moz-box-ordinal-group"
                                              , "-ms-flex-order"
                                              , "-webkit-order"
                                              , "order"
                                              ]


