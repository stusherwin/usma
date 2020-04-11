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

  import HouseholdOrder
  import Database

  reconcileHouseholdOrderFile :: ByteString -> Int -> Int -> Int -> String -> IO ()
  reconcileHouseholdOrderFile connectionString groupId orderId householdId filePath = do
    putStrLn filePath
    reconcileDetails <- readReconcileOrderDetails filePath
    reconcileHouseholdOrderItems connectionString groupId orderId householdId reconcileDetails
    putStrLn "Done."
  
  readReconcileOrderDetails :: String -> IO [ReconcileHouseholdOrderItemDetails]
  readReconcileOrderDetails filePath = do
    html <- readFile filePath
    let colData = rows $ table $ parseTags $ html
    return $ catMaybes $ map (details . parseColData) colData
    where
    table    = concat . take 1 . drop 4 . partitions (~== ("<table>" :: String))
    rows     = map (extractColData . columns) . drop 1 . partitions (~== ("<tr>" :: String))
    columns  = map innerText . partitions (~== ("<td>" :: String))
    extractColData cols = (cols `atMay` 1, cols `atMay` 5, cols `atMay` 4)
    parseColData (c, p, q) = (parseCode c, p >>= parsePrice, q >>= parseQty)
    parseCode Nothing   = Nothing
    parseCode (Just []) = Nothing
    parseCode (Just str) | (all isSpace) str = Nothing
                         | otherwise         = Just str
    parsePrice :: String -> Maybe Int
    parsePrice p = do
      pounds <- (readMaybe p) :: Maybe Double
      return $ round $ pounds * 100
    parseQty :: String -> Maybe Int
    parseQty   = readMaybe
    details (Just c, Just p, Just q) = Just $ ReconcileHouseholdOrderItemDetails c p q
    details _ = Nothing