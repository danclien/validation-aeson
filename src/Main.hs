{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

import           Data.Aeson
import qualified Data.ByteString as BS

import           Models

main :: IO ()
main = do
  printHeader "`Child` without Aeson"
  let child       = mkChild (mkText32 "Child") :: VP Child
  let childNoName = mkChild (mkText32 "") :: VP Child
  let childLong   = mkChild (mkText32 "Reallyreallyreallyreallyreallyreallyreallyname") :: VP Child
  print child
  print childNoName
  print childLong

  printHeader "`Parent` without Aeson"
  let parent       = mkParent (mkText32 "Parent") child :: VP Parent
  let parentNoName = mkParent (mkText32 "") childNoName :: VP Parent
  let parentLong   = mkParent (mkText32 "Parent") childLong :: VP Parent
  print parent
  print parentNoName
  print parentLong

  printHeader "`Child` with Aeson"
  _ <- printAeson "data/childsuccess.json" :: IO (Either String (VP Child))
  _ <- printAeson "data/childfailure.json" :: IO (Either String (VP Child))

  printHeader "`Parent` with Aeson"
  _ <- printAeson "data/parentsuccess.json" :: IO (Either String (VP Parent))
  _ <- printAeson "data/parentfailure.json" :: IO (Either String (VP Parent))
  _ <- printAeson "data/parentfailure2.json" :: IO (Either String (VP Parent))

  printHeader "Done"

printHeader :: String -> IO ()
printHeader s = do
  putStrLn ""
  putStrLn s
  putStrLn $ replicate (length s) '='
  putStrLn ""

printAeson :: (Show a, FromJSON (VP a)) => String -> IO (Either String (VP a))
printAeson file = do
  jsonData <- BS.readFile file
  let v = eitherDecodeStrict' jsonData
  print v
  putStrLn ""
  return v


