module Main where
  import Config
  import UpgradeDB

  main :: IO ()
  main = do
    config <- getConfig
    upgradeDB config