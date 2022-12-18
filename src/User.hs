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
module User where

import Data.Text
import Data.Kind (Type)
import Data.Aeson

import qualified Data.Map as M

import GHC.TypeLits
import GHC.TypeLits.List

import Servant.Auth.Server

import Control.Lens

import Crypto.JWT

data User :: [Symbol] -> Type where
  UnsafeMkUser :: { name :: Text } -> User s

data SomeUser :: Type where
  MkSomeUser :: KnownSymbols rs => SymbolList rs -> User rs -> SomeUser

data UserInfo = UserInfo
  { userName  :: Text
  , userRoles :: [Text]
  } deriving (Show, Eq)

instance FromJWT UserInfo where
  decodeJWT claims =
    let mUserInfo = do
          subject <- claims ^. claimSub
          subjectText <- subject ^? string
          roles <- getRoles claims
          return $ UserInfo subjectText roles
    in maybe (Left "missing information") Right mUserInfo

instance ToJWT UserInfo where
  encodeJWT _ = emptyClaimsSet

getRoles :: ClaimsSet -> Maybe [Text]
getRoles claims =
  let extraClaims = view unregisteredClaims claims
  in case M.lookup "roles" extraClaims of
    Nothing -> Nothing
    Just roles ->
      let result :: Result [Text] = fromJSON roles
      in case result of
        Error _ -> Nothing
        Success parsedResults -> Just parsedResults

randomUserInfo :: UserInfo
randomUserInfo = UserInfo "joonas" ["Orthanc.Delete.Application"]
