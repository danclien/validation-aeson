{-# LANGUAGE FlexibleInstances #-}

module Validation
       ( VError(..)
       , V
       , VP
       , Text32
       , mkText32
       ) where

import           Data.Validation.Parsing
import           Data.Validation.Aeson
import           Control.Applicative
import           Control.Lens
import           Data.Aeson
import qualified Data.Text as T
import           Data.Validation

-- Types for domain validation
data VError = MustBeLessThan32Characters T.Text
            | MustNotBeEmpty
            deriving (Eq, Show)

type V pLog pError a = ParserV pLog pError T.Text VError a

---- Types for parsing validation
type VP a = V AesonLog AesonError a

-- Basic validated type
newtype Text32 = Text32 { unText32 :: T.Text
                        } deriving (Eq, Show)

-- Smart constructor
mkText32 :: T.Text -> V a b Text32
mkText32 t
  | T.null t         = _Failure # mkError MustNotBeEmpty
  | T.length t > 32  = _Failure # mkError (MustBeLessThan32Characters t)
  | otherwise        = _Success # Text32 t

-- Aeson instance
instance FromJSON (VP Text32) where
  parseJSON = withStringV $ pure . mkText32
