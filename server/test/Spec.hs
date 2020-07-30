{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

import           Test.Hspec

import ApiRegressionSpec (apiRegressionSpec)
import DomainSpec (domainSpec)
import GenerateTestsSpec (generateTestsSpec)
import TestHelpers (ignoreFieldSpec)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  apiRegressionSpec
  ignoreFieldSpec
  domainSpec
  -- generateTestsSpec  