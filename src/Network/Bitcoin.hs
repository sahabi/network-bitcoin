{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# OPTIONS_GHC -Wall #-}
-- | A Haskell binding to the bitcoind server.
module Network.Bitcoin
    (
    -- * Types
      Auth(..)
    , Address
    , mkAddress
    , Amount
    , Account
    , MinConf
    , AddressValidation
    , isValid
    , isMine
    , account
    , BitcoinException(..)
    , Satoshi(..)

    -- * Individual API methods
    -- ** Account Administration
    , getAccount
    , setAccount
    , getAccountAddress
    , getAddressesByAccount
    , getBalance
    , getNewAddress
    -- ** Amount Paid By Someone Retrieval
    , getReceivedByAccount
    , getReceivedByAddress
--    , getTransaction -- TODO
--    , getWork -- TODO
--    , listAccounts -- TODO
    -- ** Listing Payments Received
    , ReceivedPaymentByAcc(..)
    , listReceivedByAccount
    , listReceivedByAccount'
    , ReceivedPaymentByAddr(..)
    , listReceivedByAddress
    , listReceivedByAddress'
    -- ** Listing Transactions
    , TransactionDetails(..)
    , listTransactions
    -- ** Moving Bitcoins
    , moveBitcoins
    , moveBitcoins'
    -- ** Sending Bitcoins
    , sendBitcoins
    , sendBitcoins'
    -- ** Server Administration
    , BitcoinServerInfo(..)
    , getInfo
    , backupWallet
    , getBlockCount
    , getBlockNumber
    , getConnectionCount
    , getDifficulty
    , getGenerate
    , setGenerate
    , getHashesPerSec
    , stopBitcoind
    -- ** Validation
    , validateAddress
    , isValidAddress

    -- * Low-level API
    , callApi
    ) where

import Network.Bitcoin.Address

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Aeson as A
import Data.Attoparsec.Number
import Data.Fixed
import Data.Maybe (fromJust, fromMaybe)
import Data.Ratio ((%))
import Data.Typeable
import Data.Vector ( Vector )
import Network.Browser
import Network.HTTP hiding (password)
import Network.URI (parseURI)
import qualified Data.ByteString.Lazy as BL
import Data.Text ( Text )
import qualified Data.Text as T

-- | Defines Bitcoin's internal precision
satoshis :: Integer
satoshis = 10^(8::Integer)

-- | We just use this datatype to implement an instance of 'Fixed' for
--   satoshis (the unit of bitcoin).
data Satoshi = Satoshi

instance HasResolution Satoshi where
    resolution _ = satoshis

-- | Fixed precision Bitcoin amount (to avoid floating point errors).
newtype Amount = Amount (Fixed Satoshi)
    deriving ( Typeable
             , Enum
             , Eq
             , Fractional
             , Num
             , Ord
             , Read
             , Real
             , RealFrac
             , Show )

-- | The name of a Bitcoin wallet account.
type Account = Text

-- | Reprsents the minimum number of confirmations for a payment.
type MinConf = Integer

-- | 'Auth' describes authentication credentials for
-- making API requests to the Bitcoin daemon.
data Auth = Auth
    { rpcUrl      :: Text -- ^ URL, with port, where bitcoind listens
    , rpcUser     :: Text -- ^ same as bitcoind's 'rpcuser' config
    , rpcPassword :: Text -- ^ same as bitcoind's 'rpcpassword' config
    }
    deriving (Show, Read, Ord, Eq)

-- | RPC calls return an error object. It can either be empty; or have an
--   error message + error code.
data BitcoinRpcError = NoError -- ^ All good.
                     | BitcoinRpcError Int Text -- ^ Error code + error message.
    deriving ( Show, Read, Ord, Eq )

instance FromJSON BitcoinRpcError where
    parseJSON (Object v) = BitcoinRpcError <$> v .: "code"
                                           <*> v .: "message"
    parseJSON Null       = return NoError
    parseJSON _ = mzero

-- | A response from bitcoind will contain the result of the JSON-RPC call, and
--   an error. The error should be null if a valid response was received.
data BitcoinRpcResponse a = BitcoinRpcResponse { btcResult  :: a
                                               , btcError   :: BitcoinRpcError
                                               }
    deriving ( Show, Read, Ord, Eq )

instance FromJSON a => FromJSON (BitcoinRpcResponse a) where
    parseJSON (Object v) = BitcoinRpcResponse <$> v .: "result"
                                              <*> v .: "error"
    parseJSON _          = mzero

-- | A 'BitcoinException' is thrown when 'callApi encounters an
--   error.  The API error code is represented as an @Int@, the message as
--   a @String@.
--
--   It may also be thrown when the value returned by the bitcoin API wasn't
--   what we expected.
--
--   WARNING: Any of the functions in this module's public API may throw this
--            exception. You should plan on handling it.
data BitcoinException = BitcoinApiError Int String
                      -- ^ A 'BitcoinApiError' has an error code error
                      --   message, as returned by bitcoind's JSON-RPC
                      --   response.
                      | BitcoinResultTypeError BL.ByteString
                      -- ^ The raw JSON returned.
    deriving (Show,Typeable)

instance Exception BitcoinException

-- | encodes an RPC request into a ByteString containing JSON
jsonRpcReqBody :: Text -> [Value] -> BL.ByteString
jsonRpcReqBody cmd params = encode $ object [
                "jsonrpc" .= ("2.0"::Text),
                "method"  .= cmd,
                "params"  .= params,
                "id"      .= (1::Int)
              ]

-- | 'callApi is a low-level interface for making authenticated API
--   calls to a Bitcoin daemon. The first argument specifies
--   authentication details (URL, username, password) and is often
--   curried for convenience:
--
--   > callBtc = callApi $ Auth "http://127.0.0.1:8332" "user" "password"
--
--   The second argument is the command name.  The third argument provides
--   parameters for the API call.
--
--   > let result = callBtc "getbalance" [ toJSON "account-name", toJSON 6 ]
--
--   On error, throws a 'BitcoinException'.
callApi :: FromJSON v
        => Auth    -- ^ authentication credentials for bitcoind
        -> Text    -- ^ command name
        -> [Value] -- ^ command arguments
        -> IO v
callApi auth command params = do
    (_,httpRes) <- browse $ do
        setOutHandler $ const $ return ()
        addAuthority authority
        setAllowBasicAuth True
        request . httpRequest (T.unpack urlString) $ jsonRpcReqBody command params
    let response = rspBody httpRes
    case decode' response of
        Just r@(BitcoinRpcResponse {btcError=NoError})
            -> return $ btcResult r
        Just (BitcoinRpcResponse {btcError=BitcoinRpcError code msg})
            -> throw $ BitcoinApiError code (T.unpack msg)
        Nothing
            -> throw . BitcoinResultTypeError $ response
    where authority     = httpAuthority auth
          urlString     = rpcUrl auth

-- | Internal helper functions to make callApi more readable
httpAuthority :: Auth -> Authority
httpAuthority (Auth urlString username password) =
    AuthBasic {
        auRealm    = "jsonrpc",
        auUsername = T.unpack username,
        auPassword = T.unpack password,
        auSite     = uri
    }
    where uri = fromJust . parseURI $ T.unpack urlString

-- | Builds the JSON HTTP request.
httpRequest :: String -> BL.ByteString -> Request BL.ByteString
httpRequest urlString jsonBody =
    (postRequest urlString){
        rqBody = jsonBody,
        rqHeaders = [
            mkHeader HdrContentType "application/json",
            mkHeader HdrContentLength (show $ BL.length jsonBody)
        ]
    }

instance FromJSON Amount where
    parseJSON (Number (I n)) = return $ fromInteger n
    parseJSON (Number (D n)) = let numerator = round $ n*fromInteger satoshis
                               in return . fromRational $ numerator % satoshis
    parseJSON _              = mzero

-- THIS MIGHT BE TROUBLE. We're converting to double. Does the bitcoin API
-- accept satoshis? Those would be better to use.
instance ToJSON Amount where
    toJSON (Amount n) = toJSON (realToFrac n :: Double)

-- | A handy shortcut.
tj :: ToJSON a => a -> Value
tj = toJSON

-- | Safely copies *wallet.dat* to 'destination', which can be a
--   directory or a path with filename.
backupWallet :: Auth
             -> Text -- ^ destination
             -> IO ()
backupWallet auth dest = callApi auth "backupwallet" [ tj dest ]

-- | Returns the account associated with the given address.
--
--   If an invalid address is given, a 'BitcoinException' will be thrown.
getAccount :: Auth
           -> Address -- ^ bitcoin address
           -> IO Account
getAccount auth addr = callApi auth "getaccount" [ tj addr ]

-- | Sets the account associated with the given address. The account may be
--   'Nothing' to remove an address from the account.
setAccount :: Auth
           -> Address       -- ^ The address to associate with the account.
           -> Maybe Account -- ^ The account to associate with the address.
           -> IO ()
setAccount auth addr Nothing    = callApi auth "setaccount" [ tj addr ]
setAccount auth addr (Just acc) = callApi auth "setaccount" [ tj addr, tj acc ]

-- | Returns a new bitcoin address for the given account.
getAccountAddress :: Auth
                  -> Account
                  -> IO Address
getAccountAddress auth acc = callApi auth "getaccountaddress" [ tj acc ]

-- | Returns the list of addresses associated with the given 'account'.
getAddressesByAccount :: Auth
                    -> Account
                    -> IO (Vector Address)
getAddressesByAccount auth acc = callApi auth "getaddressesbyaccount" [ tj acc ]

-- | Returns the server's available balance, or, if an account is given, the
--   balance for the given account.
getBalance :: Auth
           -> Maybe Account
           -> IO Amount
getBalance auth Nothing    = callApi auth "getbalance" []
getBalance auth (Just acc) = callApi auth "getbalance" [ tj acc ]

-- | Returns the number of blocks in the longest block chain.
getBlockCount :: Auth -> IO Integer
getBlockCount auth = callApi auth "getblockcount" []

-- | Returns the block number of the latest block in the
--   longest block chain.
getBlockNumber :: Auth -> IO Integer
getBlockNumber auth = callApi auth "getblocknumber" []

-- | Returns the number of connections to other nodes.
getConnectionCount :: Auth -> IO Integer
getConnectionCount auth = callApi auth "getconnectioncount" []

-- | Returns the proof-of-work difficulty as a multiple of the minimum
--   difficulty.
getDifficulty :: Auth -> IO Double
getDifficulty auth = callApi auth "getdifficulty" []

-- | Returns boolean true if server is trying to generate bitcoins,
--   false otherwise.
getGenerate :: Auth -> IO Bool
getGenerate auth = callApi auth "getgenerate" []

-- | Generation is limited to the given number of processors, -1 is
--   unlimited.
setGenerate :: Auth
            -> Bool -- ^ Should we generate bitcoins?
            -> Int  -- ^ The processor limit.
            -> IO ()
setGenerate auth shouldStart n = callApi auth "setgenerate" [ tj shouldStart, tj n ]

-- | Returns a recent hashes per second performance measurement while
--   generating.
getHashesPerSec :: Auth -> IO Integer
getHashesPerSec auth = callApi auth "gethashespersec" []

-- | A structural representation of the response bitcoind gives on a 'getInfo'
--   call.
data BitcoinServerInfo = BitcoinServerInfo { bsiVersion :: Integer
                                           , bsiBalance :: Amount
                                           , bsiBlocks :: Integer
                                           , bsiConnections :: Integer
                                           , bsiProxy :: Text
                                           , bsiGenerate :: Bool
                                           , bsiGenProcLimit :: Integer
                                           , bsiDifficulty :: Double
                                           , bsiHashesPerSec :: Double
                                           , bsiTestnet :: Bool
                                           , bsiKeyPoolOldDest :: Integer
                                           , bsiPayTxFee :: Amount
                                           , bsiErrors :: Text
                                           }
    deriving (Eq, Ord, Show, Read)

instance FromJSON BitcoinServerInfo where
    parseJSON (Object o) = BitcoinServerInfo <$> o .: "version"
                                             <*> o .: "balance"
                                             <*> o .: "blocks"
                                             <*> o .: "connections"
                                             <*> o .: "proxy"
                                             <*> o .: "generate"
                                             <*> o .: "genproclimit"
                                             <*> o .: "difficulty"
                                             <*> o .: "hashespersec"
                                             <*> o .: "testnet"
                                             <*> o .: "keypoololddest"
                                             <*> o .: "paytxfee"
                                             <*> o .: "errors"
    parseJSON _ = mzero

-- | Retrieves a whole bunch of stats on bitcoind.
getInfo :: Auth -> IO BitcoinServerInfo
getInfo auth =  callApi auth "getinfo" []

-- | Returns a new bitcoin address for receiving payments. If an account
--   is specified (recommended), it is added to the address book so payments
--   received with the address will be credited to the account automatically.
getNewAddress :: Auth
              -> Maybe Account
              -> IO Address
getNewAddress auth (Just acc) = callApi auth "getnewaddress" [ tj acc ]
getNewAddress auth Nothing    = callApi auth "getnewaddress" []

-- | Returns the total amount received by addresses associated with 'account'
--   in transactions with at least 'MinConf' confirmations.
--
--   A good default value for 'MinConf' is 1.
getReceivedByAccount :: Auth
                     -> Account
                     -> MinConf
                     -> IO Amount
getReceivedByAccount auth acct conf = callApi auth "getreceivedbyaccount" [ tj acct, tj conf ]

-- | Returns the total amount received by the given address in transactions
--   with at least 'MinConf' confirmations.
--
--   A good default value for 'MinConf' is 1.
getReceivedByAddress :: Auth
                     -> Address
                     -> MinConf
                     -> IO Amount
getReceivedByAddress auth addr conf = callApi auth "getreceivedbyaddress" [ tj addr, tj conf ]

-- | Represents a single received payment for an account.
data ReceivedPaymentByAcc = ReceivedPaymentByAcc { receivedInto :: Account
                                                 -- ^ The account of the receiving address.
                                                 , byAcctAmountReceived :: Amount
                                                 -- ^ Total amount received by the address.
                                                 , byAcctReceivedConfirmations :: Integer
                                                 -- ^ number of confirmations of the most
                                                 --   recent transaction included.
                                                 }
    deriving ( Show, Read, Eq, Ord )

instance FromJSON ReceivedPaymentByAcc where
    parseJSON (Object o) = ReceivedPaymentByAcc <$> o .: "account"
                                                <*> o .: "amount"
                                                <*> o .: "confirmations"
    parseJSON _ = mzero

-- | Returns a vector of 'ReceivedPaymentByAcc's.
listReceivedByAccount' :: Auth
                       -> MinConf -- ^ The minimum number of confirmations
                                  --   needed for a transaction to be valid.
                       -> Bool -- ^ Should we include addresses which haven't
                               --   received any payments?
                       -> IO (Vector ReceivedPaymentByAcc)
listReceivedByAccount' auth mc includeEmpty = callApi auth "listreceivedbyaccount" [ tj mc, tj includeEmpty ]

-- | Returns a vector of 'ReceivedPaymentByAcc's.
--
--   Is a shortcut for: `\auth -> listReceivedByAccount' auth 1 False`
listReceivedByAccount :: Auth
                       -> IO (Vector ReceivedPaymentByAcc)
listReceivedByAccount auth = callApi auth "listreceivedbyaccount" []

-- | Represents a single received payment for an address.
data ReceivedPaymentByAddr = ReceivedPaymentByAddr { receiveAddress :: Address
                                                   -- ^ The address the payment was received from.
                                                   , byAddrReceivedInto :: Account
                                                   -- ^ The account of the receiving address.
                                                   , amountReceived :: Amount
                                                   -- ^ Total amount received by the address.
                                                   , byAddrReceivedConfirmations :: Integer
                                                   -- ^ number of confirmations of the most
                                                   --   recent transaction included.
                                                   }
    deriving ( Show, Read, Eq, Ord )

instance FromJSON ReceivedPaymentByAddr where
    parseJSON (Object o) = ReceivedPaymentByAddr <$> o .: "address"
                                                 <*> o .: "account"
                                                 <*> o .: "amount"
                                                 <*> o .: "confirmations"
    parseJSON _ = mzero


-- | Returns a vector of 'ReceivedPaymentByAddr's.
listReceivedByAddress' :: Auth
                       -> MinConf -- ^ The minimum number of confirmations
                                  --   needed for a transaction to be valid.
                       -> Bool -- ^ Should we include addresses which haven't
                               --   received any payments?
                       -> IO (Vector ReceivedPaymentByAddr)
listReceivedByAddress' auth mc includeEmpty = callApi auth "listreceivedbyaddress" [ tj mc, tj includeEmpty ]

-- | Returns a vector of 'ReceivedPaymentByAddr's.
--
--   Is a shortcut for: `\auth -> listReceivedByAddress' auth 1 False`
listReceivedByAddress :: Auth
                      -> IO (Vector ReceivedPaymentByAddr)
listReceivedByAddress auth = callApi auth "listreceivedbyaddress" []

-- | A transaction category is used to classify the transaction details we
--   receive from bitcoind.
data TransactionCategory = TxnGenerate
                         | TxnSend
                         | TxnReceive
                         | TxnMove
    deriving ( Show, Read, Eq, Ord )

instance FromJSON TransactionCategory where
    parseJSON (String x) | x == "generate" = return TxnGenerate
                         | x == "send"     = return TxnSend
                         | x == "receive"  = return TxnReceive
                         | x == "move"     = return TxnMove
    parseJSON _ = mzero

-- | Information on a specific transaction.
data TransactionDetails = TransactionDetails { txnCategory :: TransactionCategory
                                             , txnAmount :: Amount
                                             -- ^ Amount of transaction.
                                             , txnFee :: Maybe Amount
                                             -- ^ Fee (if any) paid. Only for send transactions.
                                             , txnConfirmations :: Maybe Integer
                                             -- ^ Number of confirmations. Only for generate/send/receive.
                                             , txnID :: Maybe Text
                                             -- ^ Transaction ID. Only for generate/send/receive.
                                             , txnOtherAccount :: Maybe Account
                                             -- ^ Account funds were moved to or from. Move only.
                                             , txnMessage :: Maybe Text
                                             -- ^ Message associated with transaction. Send only.
                                             , txnTo :: Maybe Address
                                             -- ^ Message-to associated with transaction. Send only.
                                             }
    deriving ( Show, Read, Eq, Ord )

instance FromJSON TransactionDetails where
    parseJSON (Object o) = TransactionDetails <$> o .:  "category"
                                              <*> o .:  "amount"
                                              <*> o .:?  "fee"
                                              <*> o .:? "confirmations"
                                              <*> o .:? "txid"
                                              <*> o .:? "otheraccount"
                                              <*> o .:? "message"
                                              <*> o .:? "to"
    parseJSON _ = mzero

-- | Returns a vector of 'TransactionDetails's.
listTransactions' :: Auth
                  -> Maybe Account -- ^ If nothing, retrieves transactions for
                                   --   all accounts. Otherwise, only retrieves
                                   --   transactions from the given account.
                  -> Integer -- ^ How far back (in transactions) should we look?
                  -> IO (Vector TransactionDetails)
listTransactions' auth (Just acc) count = callApi auth "listtransactions" [ tj acc, tj count ]
listTransactions' auth Nothing    count = callApi auth "listtransactions" [ tj ("*"::Text), tj count ]

-- | The simple API to 'listTransactions''
--
--   Returns the last 10 transactions through any account.
listTransactions :: Auth -> IO (Vector TransactionDetails)
listTransactions auth = listTransactions' auth Nothing 10

-- | Move funds between accounts.
moveBitcoins' :: Auth
              -> Account    -- ^ The account we're withdrawing from.
              -> Account    -- ^ The account we're depositing into.
              -> Amount     -- ^ The amount of bitcoins to transfer.
              -> MinConf    -- ^ The minimum number of confirmations to wait for.
              -> Maybe Text -- ^ A comment for the transaction.
              -> Maybe Text -- ^ A comment-to for the transaction.
              -> IO ()
moveBitcoins' auth from to amt conf comm comm2 = callApi auth "move" [ tj from
                                                                      , tj to
                                                                      , tj amt
                                                                      , tj conf
                                                                      , tj comm'
                                                                      , tj comm2'
                                                                      ]
    where
        comm'  = fromMaybe "" comm
        comm2' = fromMaybe "" comm2

-- | Move funds between accounts - the simple version.
moveBitcoins :: Auth
             -> Account -- ^ The account we're withdrawing from.
             -> Account -- ^ The account we're depositing into.
             -> Amount  -- ^ The amount of bitcoins to transfer.
             -> Maybe Text -- ^ A comment, if you wish.
             -> IO ()
moveBitcoins auth from to amt comm = moveBitcoins' auth from to amt 1 comm Nothing

-- | Send bitcoins from an account to a given bitcoin address.
--
--   If the funds are not available, a 'BitcoinException' will be thrown.
--   Otherwise, the transaction ID will be returned.
sendBitcoins' :: Auth
              -> Maybe Account -- ^ The account to transfer from. If 'Nothing',
                               --   uses the default account of \"\".
              -> Address -- ^ The address to transfer to.
              -> Amount -- ^ The amount of bitcoins to transfer.
              -> MinConf -- ^ The minimum number of confirmations to wait for.
              -> Maybe Text -- ^ A comment for the transaction.
              -> Maybe Text -- ^ A comment-to for the transaction.
              -> IO Text -- ^ Returns a transaction ID on success.
sendBitcoins' auth Nothing addr amt _ comm comm2 = callApi auth "sendtoaddress" [ tj addr, tj amt, tj comm', tj comm2' ]
    where comm'  = fromMaybe "" comm
          comm2' = fromMaybe "" comm2
sendBitcoins' auth (Just acc) addr amt conf comm comm2 = callApi auth "sendfrom" [ tj acc, tj addr, tj amt, tj conf, tj comm', tj comm2' ]
    where comm'  = fromMaybe "" comm
          comm2' = fromMaybe "" comm2

-- | The simpler version of 'sendBitcoins''.
-- 
--   Sends the given amount of bitcoins to the given address.
--   The funds will be taken from the default account. If you
--   wish to specify the account, use 'sendBitcoins'' instead.
--
--   If the funds are not available, a 'BitcoinException' will
--   be thrown. Otherwise, the transaction ID will be returned.
sendBitcoins :: Auth
             -> Address    -- ^ The address to send to.
             -> Amount     -- ^ The amount of bitcoins to send.
             -> Maybe Text -- ^ An optional comment.
             -> IO Text    -- ^ Returns a transaction ID on success.
sendBitcoins auth addr amt comm = sendBitcoins' auth Nothing addr amt 1  comm Nothing

-- | Stops the bitcoin server.
stopBitcoind :: Auth -> IO ()
stopBitcoind auth = callApi auth "stop" []

-- | Encapsulates address validation results from 'validateAddress'
data AddressValidation = AddressValidation
    { isValid :: Bool    -- ^ Is the address valid?
    , isMine  :: Bool    -- ^ Does the address belong to my wallet?
    , account :: Account -- ^ To which account does this address belong?
    } deriving ( Show, Read, Eq, Ord )

instance FromJSON AddressValidation where
    parseJSON (Object o) = AddressValidation <$> o .: "isvalid"
                                             <*> o .: "ismine"
                                             <*> o .: "address"
    parseJSON _ = mzero

-- | Return information about an address.
-- If the address is invalid or doesn't belong to us, the account name
-- is the empty string.
validateAddress :: Auth
                -> Address
                -> IO AddressValidation
validateAddress auth addr = callApi auth "validateaddress" [ tj addr ]

-- | Returns true if the RPC says the address is valid.
--   Use this function until 'mkAddress' verifies address checksums
isValidAddress :: Auth -> Address -> IO Bool
isValidAddress auth addr = isValid <$> validateAddress auth addr
