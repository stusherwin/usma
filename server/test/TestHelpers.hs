{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, LambdaCase, ViewPatterns #-}

module TestHelpers where

-- import           Control.Monad (forM_)
import qualified Data.ByteString as B (ByteString)
import qualified Data.ByteString.Char8 as BC (breakSubstring, dropWhile, drop, concat, length, takeWhile)
import qualified Data.ByteString.Lazy as BL (ByteString, fromStrict, toStrict)
import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.Matcher
-- import           Network.Wai.Test (SResponse)
import           Data.Aeson (Object, decode)
import           Data.Aeson.Encode.Pretty (encodePretty)
import           Prelude ()
import           Prelude.Compat

import           Control.Monad
import           Data.Maybe
import           Data.String
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import           Network.HTTP.Types
import           Network.Wai.Test
import           Data.List
import           Data.Word
import           Data.Char hiding (ord)
import qualified Data.Char as Char

-- import           Test.Hspec.Wai.Util

-- type Body = LB.ByteString

ignoreFieldSpec :: Spec
ignoreFieldSpec =
  describe "ignoreField" $ do
    let strWithField = "{\"groupSettings\":{\"enablePayments\":true},\"pastHouseholdOrders\":[],\"households\":[{\"contactPhone\":null,\"contactEmail\":null,\"totalOrders\":0,\"totalPayments\":0,\"balance\":0,\"name\":\"Test Household 1\",\"contactName\":null,\"id\":1},{\"contactPhone\":null,\"contactEmail\":null,\"totalOrders\":0,\"totalPayments\":0,\"balance\":0,\"name\":\"Test Household 2\",\"contactName\":null,\"id\":2},{\"contactPhone\":null,\"contactEmail\":null,\"totalOrders\":0,\"totalPayments\":0,\"balance\":0,\"name\":\"Test Household 3\",\"contactName\":null,\"id\":3}],\"householdOrders\":[{\"orderIsAbandoned\":false,\"totalExcVat\":0,\"adjustment\":null,\"isAbandoned\":false,\"totalIncVat\":0,\"orderCreatedBy\":1,\"isComplete\":false,\"orderIsPlaced\":false,\"items\":[],\"householdId\":1,\"householdName\":\"Test Household 1\",\"orderCreatedByName\":\"Test Household 1\",\"orderId\":1,\"isOpen\":true}],\"pastCollectiveOrders\":[],\"householdPayments\":[],\"collectiveOrder\":{\"orderIsAbandoned\":false,\"totalExcVat\":0,\"adjustment\":null,\"totalIncVat\":0,\"orderCreatedBy\":1,\"isComplete\":false,\"orderIsPlaced\":false,\"items\":[],\"orderCreatedDate\":\"2020-07-24T15:13:59Z\",\"orderCreatedByName\":\"Test Household 1\",\"id\":1,\"allHouseholdsUpToDate\":true}}"
    let strWith2Fields = "{\"groupSettings\":{\"enablePayments\":true},\"pastHouseholdOrders\":[],\"households\":[{\"contactPhone\":null,\"contactEmail\":null,\"totalOrders\":0,\"totalPayments\":0,\"balance\":0,\"name\":\"Test Household 1\",\"contactName\":null,\"id\":1},{\"contactPhone\":null,\"contactEmail\":null,\"totalOrders\":0,\"totalPayments\":0,\"balance\":0,\"name\":\"Test Household 2\",\"contactName\":null,\"id\":2},{\"contactPhone\":null,\"contactEmail\":null,\"totalOrders\":0,\"totalPayments\":0,\"balance\":0,\"name\":\"Test Household 3\",\"contactName\":null,\"id\":3}],\"householdOrders\":[{\"orderIsAbandoned\":false,\"totalExcVat\":0,\"adjustment\":null,\"isAbandoned\":false,\"totalIncVat\":0,\"orderCreatedBy\":1,\"isComplete\":false,\"orderIsPlaced\":false,\"items\":[],\"householdId\":1,\"householdName\":\"Test Household 1\",\"orderCreatedByName\":\"Test Household 1\",\"orderId\":1,\"isOpen\":true}],\"pastCollectiveOrders\":[],\"householdPayments\":[],\"collectiveOrder\":{\"orderIsAbandoned\":false,\"totalExcVat\":0,\"adjustment\":null,\"totalIncVat\":0,\"orderCreatedBy\":1,\"isComplete\":false,\"orderIsPlaced\":false,\"items\":[],\"orderCreatedDate\":\"2020-07-24T15:13:59Z\",\"orderCreatedDate\":\"2020-07-24T15:13:59Z\",\"orderCreatedByName\":\"Test Household 1\",\"id\":1,\"allHouseholdsUpToDate\":true}}"
    let strWithoutField = "{\"groupSettings\":{\"enablePayments\":true},\"pastHouseholdOrders\":[],\"households\":[{\"contactPhone\":null,\"contactEmail\":null,\"totalOrders\":0,\"totalPayments\":0,\"balance\":0,\"name\":\"Test Household 1\",\"contactName\":null,\"id\":1},{\"contactPhone\":null,\"contactEmail\":null,\"totalOrders\":0,\"totalPayments\":0,\"balance\":0,\"name\":\"Test Household 2\",\"contactName\":null,\"id\":2},{\"contactPhone\":null,\"contactEmail\":null,\"totalOrders\":0,\"totalPayments\":0,\"balance\":0,\"name\":\"Test Household 3\",\"contactName\":null,\"id\":3}],\"householdOrders\":[{\"orderIsAbandoned\":false,\"totalExcVat\":0,\"adjustment\":null,\"isAbandoned\":false,\"totalIncVat\":0,\"orderCreatedBy\":1,\"isComplete\":false,\"orderIsPlaced\":false,\"items\":[],\"householdId\":1,\"householdName\":\"Test Household 1\",\"orderCreatedByName\":\"Test Household 1\",\"orderId\":1,\"isOpen\":true}],\"pastCollectiveOrders\":[],\"householdPayments\":[],\"collectiveOrder\":{\"orderIsAbandoned\":false,\"totalExcVat\":0,\"adjustment\":null,\"totalIncVat\":0,\"orderCreatedBy\":1,\"isComplete\":false,\"orderIsPlaced\":false,\"items\":[],\"orderCreatedByName\":\"Test Household 1\",\"id\":1,\"allHouseholdsUpToDate\":true}}"
    it "should remove json field from string" $ do
      ignoreField "orderCreatedDate" strWithField `shouldBe` strWithoutField
    it "should remove all matching fields from string" $ do
      ignoreField "orderCreatedDate" strWith2Fields `shouldBe` strWithoutField
    it "should leave string without matching field untouched" $ do
      ignoreField "orderCreatedDate" strWithoutField `shouldBe` strWithoutField

ignoringField :: B.ByteString -> MatchBody -> MatchBody
ignoringField fieldName (MatchBody fn) = MatchBody (\h b -> fn h (BL.fromStrict $ ignoreField fieldName $ BL.toStrict b))

ignoreField :: B.ByteString -> B.ByteString -> B.ByteString
ignoreField fieldName str = 
  let fieldStart = BC.concat [",\"", fieldName, "\":\""]
  in  case BC.breakSubstring fieldStart str of
    (before, "") -> before
    (before, after) -> BC.concat [before, ignoreField fieldName $ BC.drop 1 $ BC.dropWhile (/= '\"') $ BC.drop (BC.length fieldStart) after]

bodyEquals' :: (B.ByteString -> B.ByteString) -> BL.ByteString -> MatchBody
bodyEquals' bodyFn bodyToMatch = 
  let (MatchBody fn) = bodyEquals $ BL.fromStrict $ bodyFn $ BL.toStrict bodyToMatch
  in  MatchBody (\h b -> fn h (BL.fromStrict $ bodyFn $ BL.toStrict b))

getFieldValue :: B.ByteString -> B.ByteString -> B.ByteString
getFieldValue fieldName str = 
  let fieldStart = BC.concat [",\"", fieldName, "\":\""]
  in  case BC.breakSubstring fieldStart str of
    (_, "") -> ""
    (_, after) -> BC.takeWhile (/= '\"') $ BC.drop (BC.length fieldStart) after

replaceFieldValue :: B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString
replaceFieldValue fieldName value str = 
  let fieldStart = BC.concat [",\"", fieldName, "\":\""]
  in  case BC.breakSubstring fieldStart str of
    (_, "") -> ""
    (before, after) -> BC.concat [before, fieldStart, value, BC.dropWhile (/= '\"') $ BC.drop (BC.length fieldStart) after]

shouldMatch :: SResponse -> ResponseMatcher -> WaiExpectation st
shouldMatch r matcher = forM_ (match r matcher) (liftIO . expectationFailure)