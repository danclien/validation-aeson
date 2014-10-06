{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

import           Data.Aeson
import qualified Data.ByteString as BS

import           Models

main :: IO ()
main = do
  printHeader "`Child` without Aeson"
  let child       = mkChild (mkText32 "Child") :: VP Child
  -- AccSuccess (Child {childName = Text32 {unText32 = "Child"}})
  let childNoName = mkChild (mkText32 "") :: VP Child
  -- AccFailure [BihistoricalError {err = Right MustNotBeEmpty, leftLog = [], rightLog = ["name"]}]
  let childLong   = mkChild (mkText32 "Reallyreallyreallyreallyreallyreallyreallyname") :: VP Child
  -- AccFailure [BihistoricalError {err = Right (MustBeLessThan32Characters "Reallyreallyreallyreallyreallyreallyreallyname"), leftLog = [], rightLog = ["name"]}]
  print child
  print childNoName
  print childLong

  printHeader "`Parent` without Aeson"
  let parent       = mkParent (mkText32 "Parent") child :: VP Parent
  -- AccSuccess (Parent {parentName = Text32 {unText32 = "Parent"}, parentChild = Child {childName = Text32 {unText32 = "Child"}}})
  let parentNoName = mkParent (mkText32 "") childNoName :: VP Parent
  -- AccFailure [BihistoricalError {err = Right MustNotBeEmpty, leftLog = [], rightLog = ["name"]},BihistoricalError {err = Right MustNotBeEmpty, leftLog = [], rightLog = ["child","name"]}]
  let parentLong   = mkParent (mkText32 "Parent") childLong :: VP Parent
  -- AccFailure [BihistoricalError {err = Right (MustBeLessThan32Characters "Reallyreallyreallyreallyreallyreallyreallyname"), leftLog = [], rightLog = ["child","name"]}]
  print parent
  print parentNoName
  print parentLong

  printHeader "`Child` with Aeson"
  _ <- printAeson "data/childsuccess.json" :: IO (Either String (VP Child))
  -- Right (AccSuccess (Child {childName = Text32 {unText32 = "Sue"}}))
  _ <- printAeson "data/childfailure.json" :: IO (Either String (VP Child))
  -- Right (AccFailure [BihistoricalError {err = Right MustNotBeEmpty, leftLog = [JsonKey "name"], rightLog = ["name"]}])

  printHeader "`Parent` with Aeson"
  _ <- printAeson "data/parentsuccess.json" :: IO (Either String (VP Parent))
  -- Right (AccSuccess (Parent {parentName = Text32 {unText32 = "Parent Bob"}, parentChild = Child {childName = Text32 {unText32 = "Sue"}}}))
  _ <- printAeson "data/parentfailure.json" :: IO (Either String (VP Parent))
  -- Right (AccFailure [BihistoricalError {err = Right MustNotBeEmpty, leftLog = [JsonKey "name"], rightLog = ["name"]},BihistoricalError {err = Right MustNotBeEmpty, leftLog = [JsonKey "child",JsonKey "name"], rightLog = ["child","name"]}])
  _ <- printAeson "data/parentfailure2.json" :: IO (Either String (VP Parent))
  -- Right (AccFailure [BihistoricalError {err = Right MustNotBeEmpty, leftLog = [JsonKey "name"], rightLog = ["name"]},BihistoricalError {err = Left KeyNotFound, leftLog = [JsonKey "child"], rightLog = ["child"]}])

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