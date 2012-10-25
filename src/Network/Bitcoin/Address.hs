{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- | This module contains an encapsulated method of generating and extracting
--   valid bitcoin addresses.
module Network.Bitcoin.Address
    (
    -- * Types
      Address

    -- * Functions
    , mkAddress
    , addressToText
    )
where

import           Data.Aeson
import           Data.Text ( Text )
import qualified Data.Text as T
import           Data.Typeable

-- | Represents a Bitcoin receiving address.  Construct one with
-- 'mkAddress'.
newtype Address = Address Text
    deriving ( ToJSON
             , FromJSON
             , Typeable
             , Eq
             , Ord
             , Read
             , Show
             )

-- | Converts a given address to its textual representation.
addressToText :: Address -> Text
addressToText (Address t) = t

-- | Construct an 'Address' from 'Text'.
--   Returns 'Nothing' if the text is not a valid Bitcoin address.
--
-- Only validates approximate address format.
-- /Does not/ validate address checksum.
-- Until full validation is done, use 'isValidAddress' RPC call instead
mkAddress :: Text -> Maybe Address
mkAddress s =
    if isOK (T.unpack s)
        then Just $ Address s
        else Nothing
  where -- TODO validate address checksum (write base58 module first)
    isOK ('1':_) = (T.length s >= 25) && (T.length s <= 34)
    isOK ('m':_) = (T.length s >= 26) && (T.length s <= 34)
    isOK ('n':_) = (T.length s >= 26) && (T.length s <= 34)
    isOK _       = False
