{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib
  ( runBot,
  )
where

import qualified Adapter.PostgreSQL.Adapter as PG
import qualified Adapter.PostgreSQL.Common as PG
import Control.Monad.Reader (ReaderT (runReaderT))
import qualified Domain.Bot as B
import qualified Domain.Model as M

newtype BotLib = BotLib {unBotLib :: PG.AppState}

instance M.BotDBModel BotLib where
  getUserById pool uId = runReaderT (PG.getUserById uId) (unBotLib pool)
  insertMsg pool uId txt = runReaderT (PG.insertMsg uId txt) (unBotLib pool)
  createUser pool uId uName = runReaderT (PG.createUser uId uName) (unBotLib pool)
  translateWord pool word = runReaderT (PG.translateWord word) (unBotLib pool)

runBot :: IO ()
runBot = do
  Right pgCfg <- PG.readDBConfig "app-db-config.env"
  PG.withAppState pgCfg $ \pool ->
    B.botStartup (PG.getToken pgCfg) (B.handleTranslate $ BotLib pool)
