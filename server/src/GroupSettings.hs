{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module GroupSettings where
  import Data.Aeson
  import GHC.Generics
  
  data GroupSettings = GroupSettings { enablePayments :: Bool
                                     } deriving (Eq, Show, Generic)
  instance ToJSON GroupSettings