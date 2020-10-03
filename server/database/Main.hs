module Main where
  import Config
  import UpgradeDB

  main :: IO ()
  main = do
    config <- getConfig"main.config"
    upgradeDB config