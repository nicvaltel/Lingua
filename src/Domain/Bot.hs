{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Domain.Bot
  ( botStartup,
    ChatModel (..),
    Action (..),
    handleTranslate,
  )
where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Maybe
import Data.Text (Text, pack)
import Debug.Trace (traceShow)
import qualified Domain.Model as M
import Telegram.Bot.API as Telegram
import Telegram.Bot.Simple
import Telegram.Bot.Simple.UpdateParser

data ChatState
  = InitSate
  deriving (Show, Eq)

newtype ChatModel
  = ChatModel ChatState
  deriving (Show, Eq)

data Action
  = NoAction
  | RecordMsg Int (Maybe Text) Int Text
  deriving (Show, Read)

botStartup :: (MonadIO m) => String -> (Action -> ChatModel -> Eff Action ChatModel) -> m ()
botStartup tokenStr handleAction = do
  let token = Token . pack $ tokenStr
  env <- liftIO $ defaultTelegramClientEnv token
  liftIO $ startBot_ (conversationBot updateChatId (incexpBotApp handleAction)) env

emptyChatModel :: ChatModel
emptyChatModel = ChatModel InitSate

incexpBotApp :: (Action -> ChatModel -> Eff Action ChatModel) -> BotApp ChatModel Action
incexpBotApp handleAction = BotApp {botInitialModel = emptyChatModel, botAction = flip handleUpdate, botHandler = handleAction, botJobs = []}

handleUpdate :: ChatModel -> Update -> Maybe Action
handleUpdate _ update = do
  msg <- updateMessage update
  usr <- messageFrom msg
  let Telegram.UserId usrId = Telegram.userId usr
  let Telegram.MessageId msgId = Telegram.messageMessageId msg
  let usrIdInt = fromIntegral usrId :: Int
  let msgIdInt = fromIntegral msgId :: Int
  let usrName = Telegram.userUsername usr
  let parser = RecordMsg usrIdInt usrName msgIdInt <$> plainText
  parseUpdate parser update

handleTranslate :: M.BotDBModel a => a -> Action -> ChatModel -> Eff Action ChatModel
handleTranslate pool action model = traceShow action $
  case action of
    NoAction -> pure model
    RecordMsg usrId mayUsrname _ word -> do
      let usrname = fromMaybe (pack $ "user_" <> show usrId) mayUsrname
      model <# do
        maybeUser :: Maybe M.User <- liftIO $ M.getUserById pool usrId
        when (isNothing maybeUser) $ liftIO $ M.createUser pool usrId usrname >> pure ()
        _ <- liftIO $ M.insertMsg pool usrId word
        case maybeUser of
          Just _ -> do
            translation <- liftIO $ M.translateWord pool word
            case translation of
              Right (wRom, wRus) -> replyString $ wRom <> ": " <> wRus
              Left err -> replyString . pack $ show err
          Nothing -> do
            replyString "Я бот-переводчик с румынского на русский язык. Напишите слово, чтобы я перевел его."
            replyString "Sunt un bot traducător din limba rusă în limba română. Scrieți un cuvânt pentru a-l traduce."
        pure NoAction
  where
    replyString :: Text -> BotM ()
    replyString = reply . toReplyMessage
