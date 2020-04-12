{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module ReconcileHouseholdOrderFile where
  import Control.Monad (forM_)
  import Data.ByteString (ByteString)
  import Data.Char (isSpace, isNumber, isPunctuation)
  import Data.List (intercalate)
  import Data.Maybe (catMaybes, listToMaybe)
  import Data.Text (Text)
  import Data.Text.Encoding
  import Data.Time.Clock (UTCTime)
  import Safe (atMay)
  import Text.HTML.TagSoup
  import Text.Read (readMaybe)
  import Debug.Trace (trace)

  import UploadedOrderFile
  import HouseholdOrder
  import Database

  getHouseholdOrderFileDetails :: String -> String -> IO (Maybe UploadedOrderFile)
  getHouseholdOrderFileDetails filePath fileId = do
    html <- readFile filePath
    let tags = parseTags html
    let rows = map toRow $ parseOrderRows tags
    let description = parseOrderDescription tags
    let totals = parseOrderTotal tags
    case (rows, totals) of
      (r:_, Just (totalExcVat, totalIncVat)) -> return $ Just $ UploadedOrderFile fileId
                                                                                  description
                                                                                  totalExcVat
                                                                                  totalIncVat
                                                                                  rows
      _ -> return Nothing
    where
    toRow (c, d, s, q, p, t) = UploadedOrderFileRow c d s p q t
  
  reconcileHouseholdOrderFile :: ByteString -> Int -> Int -> Int -> String -> IO ()
  reconcileHouseholdOrderFile connectionString groupId orderId householdId filePath = do
    reconcileDetails <- readReconcileOrderDetails filePath
    reconcileHouseholdOrderItems connectionString groupId orderId householdId reconcileDetails
  
  readReconcileOrderDetails :: String -> IO [ReconcileHouseholdOrderItemDetails]
  readReconcileOrderDetails filePath = do
    html <- readFile filePath
    let tags = parseTags html
    return $ map toDetails $ parseOrderRows tags 
    where
    toDetails (c, _, _, q, p, _) = ReconcileHouseholdOrderItemDetails c p q

  parseOrderDescription :: [Tag String] -> String
  parseOrderDescription = extractOrderRef . intercalate " " . map innerText . tr . table
    where
    table = concat . take 1 . drop 3 . partitions (~== ("<table>" :: String))
    tr    = take 1 . drop 1 . partitions (~== ("<tr>" :: String))
    extractOrderRef = unwords . drop 10 . words
    
  parseOrderTotal :: [Tag String] -> Maybe (Int, Int)
  parseOrderTotal = justIfAll . parseTotals . extractTotals . tds . table
    where
    table = concat . take 1 . drop 5 . partitions (~== ("<table>" :: String))
    tds   = concat . drop 2 . partitions (~== ("<td>" :: String))
    extractTotals x = (\t -> (t `atMay` 0, t `atMay` 2)) . filter (not . all isSpace) . map fromTagText . filter isTagText $ x
    parseTotals x | trace ("parseTotals " ++ show x) False = undefined
    parseTotals (exc, inc) = (exc >>= parsePrice, inc >>= parsePrice)
    justIfAll (Just exc, Just inc) = Just (exc, inc)
    justIfAll _ = Nothing

  parseOrderRows :: [Tag String] -> [(String, String, String, Int, Int, Int)]
  parseOrderRows = catMaybes . map (justIfAll . parseColData . extractColData . tds) . trs . table
    where
    table = concat . take 1 . drop 4 . partitions (~== ("<table>" :: String))
    trs   = drop 1 . partitions (~== ("<tr>" :: String))
    tds   = map innerText . partitions (~== ("<td>" :: String))
    extractColData cols = (cols `atMay` 1, cols `atMay` 2, cols `atMay` 3, cols `atMay` 4, cols `atMay` 5, cols `atMay` 8)
    parseColData (c, d, s, q, p, t) = ( c >>= parseRequiredString
                                      , d >>= parseOptionalString
                                      , s >>= parseOptionalString
                                      , q >>= parseQty
                                      , p >>= parsePrice
                                      , t >>= parsePrice
                                      )
    justIfAll (Just c, Just d, Just s, Just q, Just p, Just t) = Just (c, d, s, q, p, t)
    justIfAll _ = Nothing

  parseOptionalString = Just

  parseRequiredString :: String -> Maybe String
  parseRequiredString [] = Nothing
  parseRequiredString str | (all isSpace) str = Nothing
                          | otherwise         = Just str

  parsePrice :: String -> Maybe Int
  parsePrice p = do
    let numString = filter (\c -> isNumber c || isPunctuation c) p
    pounds <- (readMaybe numString) :: Maybe Double
    return $ round $ pounds * 100

  parseQty :: String -> Maybe Int
  parseQty   = readMaybe