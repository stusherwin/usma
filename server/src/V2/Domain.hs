{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}

module V2.Domain ( module V2.Domain.Adjustments 
                 , module V2.Domain.Catalogue 
                 , module V2.Domain.Households 
                 , module V2.Domain.Orders 
                 , module V2.Domain.Payments 
                 , module V2.Domain.Prices 
                 , module V2.Domain.Types 
                 , module V2.Domain.Utils
                 ) where

import V2.Domain.Adjustments 
import V2.Domain.Catalogue 
import V2.Domain.Households 
import V2.Domain.Orders 
import V2.Domain.Payments 
import V2.Domain.Prices 
import V2.Domain.Types 
import V2.Domain.Utils