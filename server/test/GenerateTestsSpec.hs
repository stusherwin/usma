{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module GenerateTestsSpec where

import           Control.Monad (forM_)
import qualified Data.ByteString.Char8 as BC (pack, unpack, length)
import qualified Data.ByteString.Lazy as BL (fromStrict)
import           Data.Char (isDigit)
import           Data.Function (on)
import           Data.List (isSuffixOf, sortBy, intercalate)
import           Test.Hspec
import           Test.Hspec.Wai
import           System.Directory (listDirectory)

import Config (getConfig)
import App
import CompareV2Api
import DomainV2

import TestHelpers

recordingsDir :: String
recordingsDir = "server/test/requests/"

generateTestsSpec :: Spec
generateTestsSpec = do
  config <- runIO $ getConfig "test.config"
  runIO $ recreateDatabase config

  with (pure $ app mockFetchProductImageV1 mockFetchProductImageV2 config) $ do
    describe "Generated tests" $ do
      files <- runIO $ sortBy (compare `on` (readInt . takeWhile isDigit)) <$> listDirectory recordingsDir
      let requests = filter (isSuffixOf "req.txt") files `zip` filter (isSuffixOf "resp.txt") files
      forM_ requests $ \(reqFile, respFile) -> do
        req :: RecordedRequest <- runIO $ read <$> readFile (recordingsDir ++ reqFile)
        resp :: RecordedResponse <- runIO $ read <$> readFile (recordingsDir ++ respFile)
        let headers = filter (\(n, _) -> n == "content-type" || n == "content-length" )$ reqHeaders req
        runIO $ putStrLn "it \"should do something\" $ do"
        runIO $ putStrLn $ unlines 
          [ unwords 
            [ "  request"
            , show $ reqMethod req
            , show $ reqPath req
            , if null $ headers
                then "[]" 
                else "\n      " ++ (show headers)
            , if BC.length (reqBody req) <= 2
                then show $ reqBody req 
                else "\n      " ++ (show $ reqBody req)
            ]
          , unwords [ "    `shouldRespondWith`", show $ respStatus resp ]
          , unwords [ "      { matchHeaders = [" 
                    , intercalate "," $ map (\(n, v) -> show n ++ " <:> " ++ show v) $ respHeaders resp
                    , "]"
                    ]
          , unwords [ "      , matchBody ="
                    , "bodyEquals' (ignoreField \"orderCreatedDate\")"
                    , show $ respBody resp
                    ]
          , "      }"
          ]
        it ((BC.unpack $ reqMethod req) ++ " " ++ (BC.unpack $ reqPath req)) $ do
          request (reqMethod req) (reqPath req) (reqHeaders req) (BL.fromStrict $ reqBody req) `shouldRespondWith` ResponseMatcher
            { matchStatus = respStatus resp 
            , matchBody = bodyEquals' (ignoreField "orderCreatedDate") $ respBody resp
            , matchHeaders = map (\(n, v) -> n <:> v) $ respHeaders resp
            }
        let v2Path = toApiV2Path $ BC.unpack $ reqPath req
        runIO $ putStrLn $ unlines 
          [ unwords 
            [ "  request"
            , show $ reqMethod req
            , show $ v2Path
            , if null $ headers
                then "[]" 
                else "\n      " ++ (show headers)
            , if BC.length (reqBody req) <= 2
                then show $ reqBody req 
                else "\n      " ++ (show $ reqBody req)
            ]
          , unwords [ "    `shouldRespondWith`", show $ respStatus resp ]
          , unwords [ "      { matchHeaders = [" 
                    , intercalate "," $ map (\(n, v) -> show n ++ " <:> " ++ show v) $ respHeaders resp
                    , "]"
                    ]
          , unwords [ "      , matchBody ="
                    , "bodyEquals' (ignoreField \"orderCreatedDate\")"
                    , show $ respBody resp
                    ]
          , "      }"
          ]
        it ((BC.unpack $ reqMethod req) ++ " " ++ v2Path) $ do
          request (reqMethod req) (BC.pack v2Path) (reqHeaders req) (BL.fromStrict $ reqBody req) `shouldRespondWith` ResponseMatcher
            { matchStatus = respStatus resp 
            , matchBody = bodyEquals' (ignoreField "orderCreatedDate") $ respBody resp
            , matchHeaders = map (\(n, v) -> n <:> v) $ respHeaders resp
            }

toApiV2Path :: String -> String
toApiV2Path path = case splitOn '/' path of 
  ("":"api":g:"v1":rest) -> intercalate "/" $ "":"api":g:"v2":rest
  _ -> path