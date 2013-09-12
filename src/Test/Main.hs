{-# LANGUAGE OverloadedStrings #-}

module Main where


import Test.QuickCheck
import Test.QuickCheck.Monadic
import Network.Bitcoin


main :: IO ()
main = mapM_ qcOnce [ canGetInfo ]--defaultMain tests 


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


