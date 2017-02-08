{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Server ( runServer
              ) where

import           Api
import           Control.Monad.IO.Class
import           Control.Monad.Logger     (runStderrLoggingT)
import           Data.String.Conversions
import           Data.Text
import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.Sqlite
import           Handlers
import           Models
import           Network.Wai
import           Network.Wai.Handler.Warp as Warp
import           Servant

server :: ConnectionPool -> Server UserApi
server pool =
  userAddH :<|> userGetH
  where
    userAddH newUser = liftIO $ userAdd newUser pool
    userGetH name    = liftIO $ userGet name pool

app :: ConnectionPool -> Application
app pool = serve api $ server pool

mkApp :: FilePath -> IO Application
mkApp sqliteFile = do
  pool <- runStderrLoggingT $ createSqlitePool (cs sqliteFile) 5

  runSqlPool (runMigration migrateAll) pool
  return $ app pool

runServer :: FilePath -> IO ()
runServer sqliteFile =
  Warp.run 6666 =<< mkApp sqliteFile
