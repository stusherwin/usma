{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module V2_.ReconcileSumaOrderFile where
  import Data.Char (isSpace, isNumber, isPunctuation)
  import Data.List (intercalate)
  import Data.Maybe (catMaybes)
  import Safe (atMay)
  import Text.HTML.TagSoup (Tag, parseTags, partitions, innerText, fromTagText, isTagText, (~==))
  import Text.Read (readMaybe)

  import V1_.Types as Api (UploadedOrderFile(..), UploadedOrderFileRow(..))
  import V2_.Domain

  parseOrderFileDetails :: String -> String -> Maybe UploadedOrderFile
  parseOrderFileDetails fileId html =
    let tags = parseTags html
        rows = map toRow $ parseOrderRows tags
        description = parseOrderDescription tags
        totals = parseOrderTotal tags
        toRow (c, d, s, q, p, t) = UploadedOrderFileRow c d s p q t
    in case (rows, totals) of
      (_:_, Just (totalExcVat, totalIncVat)) -> Just $ UploadedOrderFile fileId
                                                                         description
                                                                         totalExcVat
                                                                         totalIncVat
                                                                         rows
      _ -> Nothing
  
  parseOrderFileUpdates :: String -> [OrderItemSpec]
  parseOrderFileUpdates html =
    let tags = parseTags html
        toDetails (c, _, _, q, p, _) = OrderItemSpec (ProductCode c) p q
    in map toDetails $ parseOrderRows tags 

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

  parseOptionalString :: a -> Maybe a
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