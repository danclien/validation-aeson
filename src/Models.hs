{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Models
       ( module Validation
       , Parent
       , mkParent
       , Child
       , mkChild
       ) where

import           Data.Validation.Parsing
import           Data.Validation.Aeson
import           Control.Applicative
import           Data.Aeson

import           Validation

-- Models
data Parent = Parent { parentName :: Text32
                     , parentChild :: Child
                     } deriving (Eq, Show)

data Child = Child { childName :: Text32
                   } deriving (Eq, Show)

-- Smart constructors for models
mkParent :: V a b Text32 -> V a b Child -> V a b Parent
mkParent name child = Parent <$> name >: "name"
                             <*> child >: "child"

mkChild :: V a b Text32 -> V a b Child
mkChild name = Child <$> name >: "name"

-- Aeson instances
instance FromJSON (VP Child) where
  parseJSON = withObjectV $ \o ->
    mkChild <$> o .:: "name"

instance FromJSON (VP Parent) where
  parseJSON = withObjectV $ \o ->
    mkParent <$> o .:: "name"
             <*> o .:: "child"
