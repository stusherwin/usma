{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
import           Prelude ()
import           Prelude.Compat
import           Control.Monad (forM_)
import           Data.ByteString.Char8 (pack, unpack)
import           Data.ByteString.Lazy (fromStrict)
import           Data.Char (isDigit)
import           Data.Function (on)
import           Data.List (isSuffixOf, sortBy, intercalate)
import qualified Network.Wai.Handler.Warp as Warp
import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.Matcher
import           System.Directory (listDirectory, getCurrentDirectory)
import           System.Process (readProcessWithExitCode)

import Config
import App
import CompareV2Api
import UpgradeDB

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  businessLogicSpec

withApp :: (Warp.Port -> IO ()) -> IO ()
withApp action =
  Warp.testWithApplication (pure (app config)) action

config :: Config
config = Config { port = 8083
                , connectionString = "postgres://postgres:Archim3de5post@localhost:5432/col_ord_test"
                , connectionStringV2 = "postgres://postgres:Archim3de5post@localhost:5432/col_ord_test"
                }

recordingsDir :: String
recordingsDir = "server/data/recordings/"

businessLogicSpec :: Spec
businessLogicSpec =
  with (pure $ app config) $ do
    dir <- runIO $ getCurrentDirectory
    runIO $ putStrLn dir
    runIO $ runScript (connectionString config) $ dir ++ "/server/test/delete-db.sql"
    runIO $ runScript (connectionString config) $ dir ++ "/server/database/database_setup.sql"
    runIO $ upgradeDB config
    runIO $ runScript (connectionString config) $ dir ++ "/server/test/create-groups.sql"
    describe "API V2" $ do
      files <- runIO $ sortBy (compare `on` (readInt . takeWhile isDigit)) <$> listDirectory recordingsDir
      let requests = filter (isSuffixOf "req.txt") files `zip` filter (isSuffixOf "resp.txt") files
      forM_ requests $ \(reqFile, respFile) -> do
        req :: RecordedRequest <- runIO $ read <$> readFile (recordingsDir ++ reqFile)
        resp :: RecordedResponse <- runIO $ read <$> readFile (recordingsDir ++ respFile)
        it ((unpack $ reqMethod req) ++ " " ++ (unpack $ reqPath req)) $ do
          request (reqMethod req) (reqPath req) (reqHeaders req) (fromStrict $ reqBody req) `shouldRespondWith` ResponseMatcher
            { matchStatus = respStatus resp 
            , matchBody = bodyEquals $ respBody resp
            , matchHeaders = map (\(n, v) -> n <:> v) $ respHeaders resp
            }
        let v2Path = pack $ toApiV2Path $ unpack $ reqPath req
        it ((unpack $ reqMethod req) ++ " " ++ (unpack $ v2Path)) $ do
          request (reqMethod req) v2Path (reqHeaders req) (fromStrict $ reqBody req) `shouldRespondWith` ResponseMatcher
            { matchStatus = respStatus resp 
            , matchBody = bodyEquals $ respBody resp
            , matchHeaders = map (\(n, v) -> n <:> v) $ respHeaders resp
            }

toApiV2Path :: String -> String
toApiV2Path path = case splitOn '/' path of 
  ("api":g:"v1":rest) -> intercalate "/" $ "api":g:"v2":rest
  _ -> path

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn ch list = f list [[]] where
  f _ [] = []
  f [] ws = map reverse $ reverse ws
  f (x:xs) ws | x == ch = f xs ([]:ws)
  f (x:xs) (w:ws) = f xs ((x:w):ws)
