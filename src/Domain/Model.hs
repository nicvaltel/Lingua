{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Domain.Model
  ( BotDBModel (..),
    UserId,
    Username,
    MessageId,
    User (..),
    Message (..),
    MessageError (..),
    TranslateError (..),
    findBestWord,
    isRomanWord,
    cleanWord,
    CleanText (..),
    isRussianWord,
  )
where

import Data.Char (ord)
import Data.List (minimumBy, sortBy)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Internal.Fusion as TS (stream)
import qualified Data.Text.Internal.Fusion.Common as TS
import Data.Time (UTCTime)
import GHC.Exts (sortWith)

type UserId = Int

type Username = Text

type MessageId = Int

data User = User
  { userId :: UserId,
    username :: Username,
    created :: UTCTime
  }
  deriving (Show, Eq)

data Message = Message
  { messageId :: MessageId,
    uId :: Int,
    text :: Text,
    sent :: UTCTime
  }
  deriving (Show, Eq)

newtype MessageError = UserDoesNotExist UserId
  deriving (Show, Eq)

data TranslateError = WordNotFound
  deriving (Eq)

instance Show TranslateError where
  show WordNotFound = "Слово не найдено"

class BotDBModel a where
  getUserById :: a -> UserId -> IO (Maybe User)
  insertMsg :: a -> UserId -> Text -> IO (Either MessageError Message)
  createUser :: a -> UserId -> Username -> IO User
  translateWord :: a -> Text -> IO (Either TranslateError (Text, Text))

findBestWord :: [(Int, Int, Text, Text)] -> Either TranslateError (Text, Text)
findBestWord [] = Left WordNotFound
findBestWord [(_, _, wordRom, worsRus)] = Right (wordRom, worsRus)
findBestWord wrds =
  Right
    . (\(_, _, wRom, wRus) -> (wRom, wRus))
    . minimumBy sortFunc
    $ wrds
  where
    sortFunc (idTrans0, idWord0, _, _) (idTrans1, idWord1, _, _) =
      case idTrans0 `compare` idTrans1 of
        GT -> GT
        LT -> LT
        EQ -> idWord0 `compare` idWord1

newtype CleanText = CleanText {getCleanText :: Text} -- lowered and cleaned from \ş -> ș and \ţ -> ț

isRussianWord :: CleanText -> Bool
isRussianWord (CleanText word) = TS.any (\c -> c >= 'а' && c <= 'я') (TS.stream word)

isRomanWord :: CleanText -> Bool
isRomanWord (CleanText word) = any (`textElem` word) literalsRomana
  where
    literalsRomana :: [Char]
    literalsRomana = "ăâîșț" -- CleanText is lowered and cleaned from \ş -> ș and \ţ -> ț
    textElem :: Char -> Text -> Bool
    textElem c t = TS.any (== c) (TS.stream t)

cleanWord :: Text -> CleanText
cleanWord =
  CleanText
    . T.map (\c -> if c == 'ş' then 'ș' else c)
    . T.map (\c -> if c == 'ţ' then 'ț' else c)
    . T.toLower
