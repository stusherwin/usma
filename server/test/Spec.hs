{-# LANGUAGE OverloadedStrings #-}
import           Prelude ()
import           Prelude.Compat

import           GHC.Generics
import           Network.HTTP.Client       hiding (Proxy, port)
import           Network.HTTP.Types
import           Network.Wai
import qualified Network.Wai.Handler.Warp         as Warp

import           Servant
import           Servant.Client
import           Servant.Server
import           Servant.QuickCheck

import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.Matcher

import Config
import App

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  businessLogicSpec

withApp :: (Warp.Port -> IO ()) -> IO ()
withApp action =
  Warp.testWithApplication (pure (app config)) action

config = Config { port = 8083
                , connectionString = "postgres://v1_user:password@localhost:5432/col_ord"
                , connectionStringV2 = "postgres://v2_user:password@localhost:5432/col_ord"
                }

businessLogicSpec :: Spec
businessLogicSpec =
  around withApp $ do
    baseUrl <- runIO $ parseBaseUrl "http://localhost"
    manager <- runIO $ newManager defaultManagerSettings
    let clientEnv port = mkClientEnv manager (baseUrl { baseUrlPort = port })
    describe "stuff" $ do
      it "should do stuff" $ \port -> do
        True `shouldBe` False
    -- let createUser = client (Proxy :: Proxy UserApi)
    -- baseUrl <- runIO $ parseBaseUrl "http://localhost"
    -- manager <- runIO $ newManager defaultManagerSettings
    -- let clientEnv port = mkClientEnv manager (baseUrl { baseUrlPort = port })

    -- describe "POST /user" $ do
    --   it "should create a user with a high enough ID" $ \port -> do
    --     result <- runClientM (createUser 50001) (clientEnv port)
    --     result `shouldBe` (Right $ User { name = "some user", user_id = 50001})
    --   it "will it fail with a too-small ID?" $ \port -> do
    --     result <- runClientM (createUser 4999) (clientEnv port)
    --     result `shouldBe` (Right $ User { name = "some user", user_id = 50001})