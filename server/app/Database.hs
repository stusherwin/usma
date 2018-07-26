{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Database (getAllOrders) where
  import Control.Monad (mzero, when)
  import Control.Monad.IO.Class (liftIO)
  import Database.PostgreSQL.Simple
  import Database.PostgreSQL.Simple.ToField
  import Database.PostgreSQL.Simple.FromField
  import Database.PostgreSQL.Simple.FromRow
  import Database.PostgreSQL.Simple.Time (Unbounded(..))
  import Data.ByteString (ByteString)
  import Data.Maybe (listToMaybe, fromJust)
  import Data.Map.Lazy (fromListWith, assocs)
  import qualified Data.Text as T
  import Data.Text.Encoding (encodeUtf8)
  import Types
  import qualified Data.IntMap.Strict as IM (IntMap(..), fromList, elems, lookup, insert, size)
  
  toDatabaseChar :: Char -> Action
  toDatabaseChar c = Escape $ encodeUtf8 $ T.pack [c]

  instance FromRow Order where
    fromRow = Order <$> field <*> field

  instance FromField Date where
    fromField f date = do
      sqlDate <- fromField f date
      case sqlDate of
        Just (Finite day) -> return $ toDate day
        _ -> mzero

  instance ToField Date where
    toField = toField . fromDate
             
  (<&>) :: Functor f => f a -> (a -> b) -> f b
  (<&>) = flip (<$>)

  (&) :: a -> (a -> b) -> b
  (&) = flip ($)
  
  getAllOrders :: ByteString -> IO [Order]
  getAllOrders connectionString = do
    conn <- connectPostgreSQL connectionString
    result <- query_ conn
      " select id, date\
      \ from \"order\""
    close conn
    return result