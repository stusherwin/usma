{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module UploadedOrderFile where
  import Data.Aeson
  import GHC.Generics

  data UploadedOrderFile = UploadedOrderFile { fileId :: String 
                                             , orderDescription :: String
                                             , totalExcVat :: Int
                                             , totalIncVat :: Int
                                             , rows :: [UploadedOrderFileRow]
                                             } deriving (Eq, Show, Generic)
  instance ToJSON UploadedOrderFile
  instance FromJSON UploadedOrderFile

  data UploadedOrderFileRow = UploadedOrderFileRow { code :: String 
                                                   , productDescription :: String
                                                   , productSize :: String
                                                   , price :: Int
                                                   , quantity :: Int
                                                   , total :: Int
                                                   } deriving (Eq, Show, Generic)
  instance ToJSON UploadedOrderFileRow
  instance FromJSON UploadedOrderFileRow
