{-# OPTIONS_GHC -Wno-warnings-deprecations  #-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module App where

import Data.Text
import Data.Proxy
import Data.Type.Equality

import GHC.TypeLits.List
import GHC.TypeLits.SymbolList

import Control.Monad.Except

import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp

import Servant.Server
import Servant.Auth.Server

import User
import API

-- * Running the application

app ::  JWTSettings -> Wai.Application
app jwtSettings =
  serveWithContext (Proxy @API) authContext server

  where
    authContext :: Context '[JWTSettings, CookieSettings]
    authContext = jwtSettings :. defaultCookieSettings :. EmptyContext

-- * Servant server

server :: Server API
server (Authenticated userInfo) =
  deleteApplication userInfo
server _ = throwAll err401

-- * Delete application endpoint

deleteApplication :: UserInfo -> Handler Text
deleteApplication userInfo =
  withUser userInfo $ \case
    MkSomeUser roles user -> case containsRole deleteApplicationRole roles of
      SDoes Refl -> deleteApplicationSafe user
      SDoesNot Refl -> throwError $ err402

deleteApplicationSafe
  :: (Contains "Orthanc.Delete.Application" roles ~ 'True)
  => User roles
  -> Handler Text
deleteApplicationSafe _ = return "Done"

deleteApplicationRole :: Proxy "Orthanc.Delete.Application"
deleteApplicationRole = Proxy

-- * Helpers

withUser :: UserInfo -> (SomeUser -> r) -> r
withUser UserInfo{..} callback =
  let strUserRoles = fmap unpack userRoles
  in reifySymbols strUserRoles $ \symbolList ->
      let someUser = MkSomeUser symbolList $ UnsafeMkUser userName
      in callback someUser
