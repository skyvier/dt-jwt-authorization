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
module Main where

import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp

import Servant.Auth.Server

import App

main :: IO ()
main =
  let key = fromSecret "test-secret"
      jwtSettings = defaultJWTSettings key
  in run 1337 $ app jwtSettings
