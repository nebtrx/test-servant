module Handlers
    ( userAdd
    , userGet
    ) where

import           Api
import           Control.Monad.IO.Class
import           Control.Monad.Logger     (runStderrLoggingT)
import           Data.String.Conversions
import           Data.Text
import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.Sqlite
import           Models
import           Network.Wai
import           Network.Wai.Handler.Warp as Warp
import           Servant

userAdd :: User -> ConnectionPool -> IO (Maybe (Key User))
userAdd newUser pool = flip runSqlPersistMPool pool $ do
  exists <- selectFirst [UserName ==. userName newUser] []
  case exists of
    Nothing -> Just <$> insert newUser
    Just _  -> return Nothing

userGet :: Text -> ConnectionPool-> IO (Maybe User)
userGet name pool = flip runSqlPersistMPool pool $ do
  mUser <- selectFirst [UserName ==. name] []
  return $ entityVal <$> mUser
