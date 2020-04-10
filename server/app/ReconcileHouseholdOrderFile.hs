{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module ReconcileHouseholdOrderFile where
  import Control.Monad (forM_)
  import Data.ByteString (ByteString)
  import Data.Char (isSpace)
  import Data.Maybe (catMaybes)
  import Data.Text (Text)
  import Data.Text.Encoding
  import Data.Time.Clock (UTCTime)
  import Safe (atMay)
  import Text.HTML.TagSoup
  import Text.Read (readMaybe)

  reconcileHouseholdOrderFile :: ByteString -> UTCTime -> String -> IO ()
  reconcileHouseholdOrderFile connectionString date filePath = do
    putStrLn filePath
    rows <- readOrderRows filePath
    forM_ rows $ \(code, qty, price) -> do
      putStrLn $ code ++ ": " ++ (show qty) ++ " x £" ++ (show price)
  
  readOrderRows :: String -> IO [(String, Int, Int)]
  readOrderRows filePath = do
    html <- readFile filePath
    return $ rows $ table $ parseTags $ html
    where
    table    = concat . take 1 . drop 4 . partitions (~== ("<table>" :: String))
    rows     = catMaybes . map (row . columns) . drop 1 . partitions (~== ("<tr>" :: String))
    columns  = map innerText . partitions (~== ("<td>" :: String))
    row cols = case (code, qty, price) of
      (Just c, Just q, Just p) -> Just (c, q, p)
      _ -> Nothing
      where
      code  = case (cols `atMay` 1) of
        Nothing -> Nothing
        Just [] -> Nothing
        Just cs | (all isSpace) cs -> Nothing
                | otherwise        -> Just cs
      qty   = cols `atMay` 4 >>= readMaybe :: Maybe Int
      price = cols `atMay` 5 >>= readMaybe :: Maybe Int