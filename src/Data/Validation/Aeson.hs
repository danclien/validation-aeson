{-# LANGUAGE FlexibleContexts #-}

module Data.Validation.Aeson
       ( module Data.Validation.Parsing
       , AesonLog(..)
       , AesonError(..)
       , (.::)
       , withObjectV
       , withStringV
       ) where

import           Control.Applicative
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Text as T
import           Data.Validation
import           Data.Validation.Parsing

data AesonLog = JsonKey T.Text
              | JsonIndex Int
              deriving (Eq, Show)

data AesonError = KeyNotFound
                | IncorrectType { expected :: AesonValueType }
                deriving (Eq, Show)

data AesonValueType = AesonObject
                    | AesonArray
                    | AesonString
                    | AesonNumber
                    | AesonBool
                    | AesonNull
                    deriving (Eq, Show)

-- Aeson helper function to prevent key not found and incorrect type errors
(.::) :: FromJSON (ParserV AesonLog AesonError log err a) =>
  Object
  -> T.Text
  -> Parser (ParserV AesonLog AesonError log err a)
obj .:: key = fmap f childParser
  where f = prependParserLog (JsonKey key)
        childParser = obj .:? key .!= keyNotFoundError
        keyNotFoundError = _Failure # mkParserError KeyNotFound

withObjectV :: (Object -> Parser (ParserV AesonLog AesonError log err a))
               -> Value
               -> Parser (ParserV AesonLog AesonError log err a)
withObjectV f (Object o) = f o
withObjectV _ _ = incorrectTypeError AesonObject

withStringV :: (T.Text -> Parser (ParserV AesonLog AesonError log err a))
               -> Value
               -> Parser (ParserV AesonLog AesonError log err a)
withStringV f (String s) = f s
withStringV _ _ = incorrectTypeError AesonString

incorrectTypeError :: AesonValueType -> Parser (ParserV AesonLog AesonError log err a)
incorrectTypeError t = pure $ _Failure # mkParserError (IncorrectType t)

