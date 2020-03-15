{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
-- | This module introduce aliases to use for @servant-generic@ types and functions writing.

module Lib.Server.Types
       ( AppServer
       , CookieHeaders
       , withAuthResult
       ) where

import qualified Lib.App.Error as AppErr
import Lib.Db.Users.User
import Servant.API.Generic (ToServantApi)
import Servant.Server.Generic (AsServerT)
import Servant.Auth.Server
import Servant (Headers)

import Lib.App (App)

------ servant-generics
type AppServer = AsServerT App


------ servant-auth
type CookieHeaders =
  Headers '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] 

withAuthResult ::  AppErr.WithError m => (UserId -> m a) -> (AuthResult UserId -> m a)
withAuthResult func ar =
  case ar of
    Authenticated ac -> func ac
    _ -> AppErr.throwError $ AppErr.notAllowed "unauthorized"
