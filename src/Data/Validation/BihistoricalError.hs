module Data.Validation.BihistoricalError where

import Data.Bifunctor
import Data.Semigroup

data BihistoricalError leftErr rightErr leftLog rightLog =
  BihistoricalError { err      :: Either leftErr rightErr
                    , leftLog  :: leftLog
                    , rightLog :: rightLog
                    } deriving (Eq, Show)

instance Bifunctor (BihistoricalError leftErr rightErr) where
  first  f (BihistoricalError e l r) = BihistoricalError e (f l) r
  second g (BihistoricalError e l r) = BihistoricalError e l     (g r)

-- Constructors
mkLeftError :: leftErr -> BihistoricalError leftErr rightErr [a] [b]
mkLeftError leftErr = BihistoricalError (Left leftErr) [] []

mkRightError :: rightErr -> BihistoricalError leftErr rightErr [a] [b]
mkRightError rightErr = BihistoricalError (Right rightErr) [] []

-- Set log
setLeftLog :: leftLog
              -> BihistoricalError leftErr rightErr leftLog rightLog
              -> BihistoricalError leftErr rightErr leftLog rightLog
setLeftLog a = first (const a)

setRightLog :: rightLog
               -> BihistoricalError leftErr rightErr leftLog rightLog
               -> BihistoricalError leftErr rightErr leftLog rightLog
setRightLog b = second (const b)

-- Prepend
prependLeftLog :: Semigroup leftLog =>
                  leftLog
                  -> BihistoricalError leftErr rightErr leftLog rightLog
                  -> BihistoricalError leftErr rightErr leftLog rightLog
prependLeftLog a = first (a <>)

prependRightLog :: Semigroup rightLog =>
                   rightLog
                   -> BihistoricalError leftErr rightErr leftLog rightLog
                   -> BihistoricalError leftErr rightErr leftLog rightLog
prependRightLog b = second (b <>)

prependLeftListLog :: a
                      -> BihistoricalError leftErr rightErr [a] rightLog
                      -> BihistoricalError leftErr rightErr [a] rightLog
prependLeftListLog a = prependLeftLog [a]

prependRightListLog :: b
                       -> BihistoricalError leftErr rightErr leftLog [b]
                       -> BihistoricalError leftErr rightErr leftLog [b]
prependRightListLog b = prependRightLog [b]

-- Append
appendLeftLog :: Semigroup leftLog =>
                 leftLog
                 -> BihistoricalError leftErr rightErr leftLog rightLog
                 -> BihistoricalError leftErr rightErr leftLog rightLog
appendLeftLog a = first (<> a)

appendRightLog :: Semigroup rightLog =>
                  rightLog
                  -> BihistoricalError leftErr rightErr leftLog rightLog
                  -> BihistoricalError leftErr rightErr leftLog rightLog
appendRightLog b = second (<> b)

appendLeftListLog :: a
                     -> BihistoricalError leftErr rightErr [a] rightLog
                     -> BihistoricalError leftErr rightErr [a] rightLog
appendLeftListLog a = appendLeftLog [a]

appendRightListLog :: b
                      -> BihistoricalError leftErr rightErr leftLog [b]
                      -> BihistoricalError leftErr rightErr leftLog [b]
appendRightListLog b = appendRightLog [b]