{-# OPTIONS_GHC -Wno-warnings-deprecations  #-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module GHC.TypeLits.SymbolList where

import Data.Kind
import Data.Proxy
import Data.Type.Equality

import GHC.TypeLits
import GHC.TypeLits.List

import Unsafe.Coerce

type family Contains (r :: Symbol) (rs :: [Symbol]) :: Bool

data SContains :: Symbol -> [Symbol] -> Type where
  SDoes :: (Contains r rs :~: 'True) -> SContains r rs
  SDoesNot :: (Contains r rs :~: 'False) -> SContains r rs

-- When something is do difficult to do at the type level, do it at the term
-- level and fake a type level proof, right?
containsRole
  :: forall expectedRole allRoles. (KnownSymbol expectedRole, KnownSymbols allRoles)
  => Proxy expectedRole
  -> SymbolList allRoles
  -> SContains expectedRole allRoles
containsRole pExpected sbAll =
  let allRoles = symbolsVal sbAll
      expectedRole = symbolVal pExpected
  in if expectedRole `Prelude.elem` allRoles
      then SDoes (unsafeCoerce Refl)
      else SDoesNot (unsafeCoerce Refl)
