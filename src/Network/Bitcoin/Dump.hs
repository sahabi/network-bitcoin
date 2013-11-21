{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
-- | An interface to bitcoind's available private key calls. The implementation
--   of these functions can be found at <https://github.com/bitcoin/bitcoin/blob/master/src/rpcdump.cpp>.
--
--   If any APIs are missing, patches are always welcome. If you look at the
--   source of this module, you'll see that the interface code is trivial.
module Network.Bitcoin.Dump ( PrivateKey
                            , importPrivateKey
                            , dumpPrivateKey
                            ) where

import Control.Applicative
import Network.Bitcoin.Internal

-- | A textual representation of a bitcoin private key.
type PrivateKey = Text

-- | Adds a private key (as returned by dumpprivkey) to your wallet.
importPrivateKey :: Auth
                 -> PrivateKey
                 -> Maybe Account
                 -- ^ An optional label for the key.
                 -> IO ()
importPrivateKey auth pk Nothing =
    unNil <$> callApi auth "importprivkey" [ tj pk ]
importPrivateKey auth pk (Just label) =
    unNil <$> callApi auth "importprivkey" [ tj pk, tj label ]

-- | Reveals the private key corresponding to the given address.
dumpPrivateKey :: Auth
               -> Address
               -> IO PrivateKey
dumpPrivateKey auth addr = callApi auth "dumpprivkey" [ tj addr ]
