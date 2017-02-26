{-# LANGUAGE TemplateHaskell #-}
module Data.Aeson.Cpm
    (
      deriveJSON'
    )
where

----------------------------------------
-- LOCAL
----------------------------------------

----------------------------------------
-- SITE-PACKAGES
----------------------------------------
import Data.Aeson.TH

----------------------------------------
-- STDLIB
----------------------------------------
import Language.Haskell.TH

deriveJSON' :: (String -> String) -> Name -> Q [Dec]
deriveJSON' f = deriveJSON (defaultOptions { fieldLabelModifier = f })
