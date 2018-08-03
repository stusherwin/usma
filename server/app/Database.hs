{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Database (getAllOrders, getOrderSummary, createOrder) where
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
  import Data.Time.Calendar (Day, showGregorian)
  
  toDatabaseChar :: Char -> Action
  toDatabaseChar c = Escape $ encodeUtf8 $ T.pack [c]

  (<&>) :: Functor f => f a -> (a -> b) -> f b
  (<&>) = flip (<$>)
  -- infixl 4 <&>

  (&) :: a -> (a -> b) -> b
  (&) = flip ($)
  -- infixr 0 &
  
  getAllOrders :: ByteString -> IO [Order]
  getAllOrders connectionString = do
    conn <- connectPostgreSQL connectionString
    rOrders <- query_ conn
      " select date\
      \ from \"order\""
    close conn
    return $ (rOrders :: [Only Day]) <&> \(Only day) -> Order (showGregorian day) (showGregorian day)

  getOrderSummary :: ByteString -> Day -> IO (Maybe OrderSummary)
  getOrderSummary connectionString day = do
    conn <- connectPostgreSQL connectionString
    rOrders <- query conn
      " select date\
      \ from \"order\"\
      \ where date = ?"
      (Only day)
    close conn
    return $ listToMaybe $ (rOrders :: [Only Day]) <&> \(Only day) -> OrderSummary (showGregorian day) (showGregorian day) False [] 0

  createOrder :: ByteString -> Day -> IO ()
  createOrder connectionString date = do
    conn <- connectPostgreSQL connectionString
    execute conn
      "insert into \"order\" (date) values (?)"
      (Only date)
    close conn
    return ()