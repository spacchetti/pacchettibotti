{-# LANGUAGE TemplateHaskell #-}
module PacchettiBotti.Static where

import Spago.Prelude

import qualified Spago.TH

bowerPackagesJson :: ByteString
bowerPackagesJson =
  $(Spago.TH.embedURLWithFallback
    "https://raw.githubusercontent.com/purescript/registry/master/bower-packages.json"
    "resources/bower-packages.json")
