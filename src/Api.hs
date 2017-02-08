{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Api where

import           Data.Proxy
import           Data.Text
import           Database.Persist
import           Models
import           Servant.API

type UserApi = "user" :> "add" :> ReqBody '[JSON] User :> Post '[JSON] (Maybe (Key User))
      :<|> "user" :> "get" :> Capture "name" Text  :> Get  '[JSON] (Maybe User)

api :: Proxy UserApi
api = Proxy
