{-# LANGUAGE OverloadedStrings #-}

module Spec.Data.Watson
    ( tests
    ) where

import qualified Data.HashMap.Strict as HM
import qualified Data.Watson         as W
import qualified Data.Watson.Value   as W
import           Test.Tasty
import           Test.Tasty.HUnit


tests :: TestTree
tests = testGroup "Data.Watson"
  [ testGroup "decode"
    [ testCase "examples" $ do
        Right (W.Int 123) @=? W.decode "BBuaBubaBubbbaBubbbbaBubbbbbaBubbbbbba"
        Right (W.String "hoge") @=? W.decode "?SShaaarrkShaaaaarrkShaaaaaarrk-SShkSharrkShaarrkShaaarrkShaaaaarrkShaaaaaarrk-SShkSharrkShaarrkShaaaaarrkShaaaaaarrk-SShkShaarrkShaaaaarrkShaaaaaarrk-"
        Right (W.Bool True) @=?W.decode "zo"
    ]
  , testGroup "decodeFile"
    [ testCase "examples" $ do
        actual <- W.decodeFile "test/fixture/object.watson"
        actual @?= Right (W.Object $ HM.fromList [("hello", W.String "world"), ("first", W.Bool True)])
    ]
  ]
