{-# LANGUAGE OverloadedStrings #-}

module Main where


import Test.QuickCheck
import Test.QuickCheck.Monadic
import Network.Bitcoin
import Data.Aeson
import Data.Vector ( empty, mapM )


main :: IO ()
main = mapM_ qcOnce [ canGetInfo
                    , canListUnspent
                    ]


qcOnce :: Property -> IO ()
qcOnce = quickCheckWith stdArgs { maxSuccess = 1
                                , maxSize = 1
                                , maxDiscardRatio = 1
                                }


auth :: Auth
auth = Auth "http://localhost:18332" "bitcoinrpc" "bitcoinrpcpassword"


canGetInfo :: Property
canGetInfo = monadicIO $ do
    info <- run $ getBitcoindInfo auth
    let checks = [ bitcoinVersion info > 80000
                 , onTestNetwork info
                 , bitcoindErrors info == ""
                 ]
    assert $ and checks


canListUnspent :: Property
canListUnspent = monadicIO $ do
    vUnspent <- run $ listUnspent auth Nothing Nothing Data.Vector.empty
    _ <- run $ print vUnspent
    Data.Vector.mapM (\coins -> assert True) vUnspent
