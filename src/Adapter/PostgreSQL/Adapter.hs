{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Adapter.PostgreSQL.Adapter
  ( getUserById,
    createUser,
    insertMsg,
    translateWord,
  )
where

import Adapter.PostgreSQL.Common
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple
import Domain.Model
  ( Message (..),
    MessageError (..),
    TranslateError (..),
    User (..),
    UserId,
    Username,
  )
import qualified Domain.Model as M
import UnliftIO (throwString)
import Data.Either.Combinators(mapLeft)

getUserById :: PG r m => UserId -> m (Maybe User)
getUserById uid = do
  result :: [(Int, Text, UTCTime)] <- withConn $ \conn -> query conn qryStr (Only uid)
  case result of
    [] -> pure Nothing
    [(userId, username, created)] -> pure $ Just User {userId, username, created}
    _ -> throwString $ "Should not happen: several userId's in users table in DB - id = " ++ show uid
  where
    qryStr = "select user_id, username, created from rusrom.users where user_id = ?"

createUser :: PG r m => UserId -> Username -> m User
createUser userId username = do
  result :: [Only UTCTime] <- withConn $ \conn -> query conn qryStr (userId, username)
  case result of
    [Only created] -> pure User {userId, username, created}
    _ -> throwString $ "Should not happen: cannot create user in users table in DB - username = " ++ show username
  where
    qryStr = "insert into rusrom.users (user_id, username) values (?, ?) returning created"

insertMsg :: PG r m => UserId -> Text -> m (Either MessageError Message)
insertMsg uId text = do
  mayUser <- getUserById uId
  case mayUser of
    Nothing -> pure (Left $ UserDoesNotExist uId)
    Just _ -> do
      result :: [(Int, UTCTime)] <- withConn $ \conn -> query conn qryStr (uId, text)
      case result of
        [(messageId, sent)] -> pure $ Right Message {messageId, uId, text, sent}
        _ -> throwString $ "Should not happen: cannot create user in users table in DB - username = " ++ show uId ++ " text = " ++ show text
  where
    qryStr = "insert into rusrom.messages (user_id, text) values (?,?) returning id, sent"

translateWord :: PG r m => Text -> m (Either Text (Text, Text))
translateWord wordUnclean = do
  let word = M.cleanWord wordUnclean
  if M.isRussianWord word
    then (mapLeft (\_ -> "Переведи с русского на румынский: " <> M.getCleanText word)) <$> (translateRus2Rom word)
    else (mapLeft (\_ -> "Переведи с румынского на русский: " <> M.getCleanText word)) <$> (translateRom2Rus word)


translateRus2Rom :: PG r m => M.CleanText -> m (Either TranslateError (Text, Text))
translateRus2Rom word = do
  result :: [(Int, Int, Text, Text)] <- withConn $ \conn -> query conn qryStr (Only $ M.getCleanText word)
  pure $ M.findBestWord result
  where
    qryStr =
      " select   \
      \ t.idTranslation,  \
      \ rus.id , \
      \ rus.word_rus, \
      \ rom.word_rom \
      \ from   \
      \ rusrom.word_rus  rus  \
      \ join rusrom.translations t on rus.word_rus = ? and t.idWord = rus.id   \
      \ join rusrom.word_rom rom on t.idTranslation = rom.id   \
      \ ; "

translateRom2Rus :: PG r m => M.CleanText -> m (Either TranslateError (Text, Text))
translateRom2Rus word = do
  let qryStr = if M.isRomanWord word then qryStrRom else qryStrEng
  result :: [(Int, Int, Text, Text)] <- withConn $ \conn -> query conn qryStr (Only $ M.getCleanText word)
  pure $ M.findBestWord result
  where
    qryStrEng =
      "select  \
      \ t.idTranslation, \
      \ rom.id , \
      \ rom.word_rom ,\
      \ rus.word_rus \
      \ from  \
      \  rusrom.word_rom  rom \
      \  join rusrom.translations t on rom.word_eng = ? and t.idWord = rom.id  \
      \  join rusrom.word_rus rus on t.idTranslation = rus.id  \
      \  ;"

    qryStrRom =
      " select  \
      \ t.idTranslation, \
      \ rom.id , \
      \ rom.word_rom ,\
      \ rus.word_rus \
      \ from  \
      \  rusrom.word_rom  rom \
      \  join rusrom.translations t on rom.word_rom = ? and t.idWord = rom.id  \
      \  join rusrom.word_rus rus on t.idTranslation = rus.id  \
      \ ;"
