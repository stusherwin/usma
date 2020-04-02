{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}

module DatabaseV2 where

import Control.Monad (mzero, when, void, forM_)
import Control.Monad.IO.Class (liftIO)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.FromField (FromField(..))
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Time (Unbounded(..))
import Database.PostgreSQL.Simple.SqlQQ
import Data.ByteString (ByteString)
import Data.Maybe (listToMaybe, fromJust, fromMaybe)
import Data.Map.Lazy (fromListWith, assocs)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.IntMap.Strict as IM (IntMap(..), fromList, elems, lookup, insert, size)
import Data.Time.Calendar (Day, showGregorian)
import Data.Time.Clock (UTCTime)

import DomainV2
