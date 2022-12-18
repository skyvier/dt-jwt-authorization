{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module API where

import Data.Text

import Servant.API
import Servant.Auth.Server

import User

type API = Auth '[JWT] UserInfo :> Get '[PlainText] Text
