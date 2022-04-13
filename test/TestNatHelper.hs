{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module TestNatHelper where

import Cybus.NatHelper
import Data.List.NonEmpty (NonEmpty (..))
import Data.Pos
import Test.Tasty
import Test.Tasty.HUnit

doit :: IO ()
doit = defaultMain suite

suite :: TestTree
suite =
  testGroup
    "TestNatHelper"
    [ testCase "nestedNonEmptyToList nestedListToNonEmpty" $
        let m = [[[1 :: Int, 2, 3], [4, 5, 6]], [[7, 8, 9], [10, 11, 12]], [[13, 14, 15], [16, 17, 18]]]
         in (nestedNonEmptyToList @(NN 323) =<< nestedListToNonEmpty @(NN 323) m) @?= Right m
    , testCase "nestedNonEmptyToList" $
        let m = [[[1 :: Int, 2, 3], [4, 5, 6]], [[7, 8, 9], [10, 11, 12]], [[13, 14, 15], [16, 17, 18]]]
         in nestedListToNonEmpty @(NN 323) m @?= Right (((1 :| [2, 3]) :| [4 :| [5, 6]]) :| [(7 :| [8, 9]) :| [10 :| [11, 12]], (13 :| [14, 15]) :| [16 :| [17, 18]]])
    , testCase "validateNestedList" $
        validateNestedList [True, False, True]
          @?= Right (_3P :| [])
    , testCase "validateNestedList" $
        validateNestedList [[[True, False, True]]]
          @?= Right (_1P :| [_1P, _3P])
    , testCase "validateNestedList" $
        validateNestedList [[[True, False, True], [False, False, False]]]
          @?= Right (_1P :| [_2P, _3P])
    , testCase "validateNestedList" $
        validateNestedList [()]
          @?= Right (_1P :| [])
    , testCase "validateNestedList" $
        validateNestedList ([] :: [()])
          @?= Left "validateNestedListC: ixes=[]:no data!"
    , testCase "validateNestedList" $
        validateNestedList [[1 :: Int, 2], [1, 2], [1, 2, 3]] @?= Left "validateNestedListC: lengths=[2,2,3] ixes=[_3P]"
    ]
