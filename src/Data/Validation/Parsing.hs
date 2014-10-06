module Data.Validation.Parsing
       ( module Data.Validation.BihistoricalError
       , ParserV
       , prependParserLog
       , prependLog
       , (>:)
       , mkError
       , mkParserError
       ) where

import           Data.Validation.BihistoricalError
import           Data.Bifunctor
import           Data.Validation

type ParserV parserLog parserErr log err a =
  AccValidation [BihistoricalError parserErr err [parserLog] [log]] a

prependParserLog :: parserLog
                    -> ParserV parserLog parserErr log err a
                    -> ParserV parserLog parserErr log err a
prependParserLog = first . fmap . prependLeftListLog

prependLog :: log
              -> ParserV parserLog parserErr log err a
              -> ParserV parserLog parserErr log err a
prependLog = first . fmap . prependRightListLog

-- Inline prependLog
(>:) :: ParserV parserLog parserErr log err a
        -> log
        -> ParserV parserLog parserErr log err a
v >: l = prependLog l v

---- Helper methods for creating errors
mkError :: err -> [BihistoricalError parserErr err [a] [b]]
mkError e = [mkRightError e]

mkParserError :: parserErr -> [BihistoricalError parserErr err [a] [b]]
mkParserError parserErr = [mkLeftError parserErr]
