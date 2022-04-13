{-# OPTIONS -Wno-orphans #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module TestMat where

import CheckerHelper
import Control.Arrow
import Control.Lens
import Control.Monad
import Cybus.Fin
import Cybus.FinMat
import Cybus.Mat
import Cybus.NatHelper
import Data.Char
import Data.Foldable
import Data.Int
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as N
import qualified Data.Monoid as MM
import Data.Pos
import Data.Semigroup.Foldable
import qualified Data.Vector as V
import GHC.TypeNats (Nat)
import Primus.Enum
import Primus.Error
import Primus.Fold
import Primus.Num1
import Primus.One
import Primus.Rep
import Test.QuickCheck
import Test.QuickCheck.Checkers
import "checkers" Test.QuickCheck.Classes
import Test.Tasty
import Test.Tasty.HUnit
import qualified Test.Tasty.QuickCheck as TQ
import Unsafe.Coerce
import qualified GHC.TypeNats as GN
import Data.Proxy

instance (NS ns, Arbitrary a) => Arbitrary (Mat ns a) where
  arbitrary = sequenceA $ mat @ns (repeat arbitrary)

instance Eq a => EqProp (Mat ns a) where (=-=) = eq

testLawsMat :: forall (ns :: [Nat]). (ShowMatC ns, NS ns) => [TestBatch]
testLawsMat =
  [functor z, applicative z, monoid z, monad z, semigroup (z, Fixed (10 :: Int)), foldable z1] --  , traversable z]
 where
  z = undefined :: Mat ns (MA, MB, MC)
  z1 = undefined :: Mat ns (MA, MB, MC, Int, MD)

testLawsMat' :: forall (ns :: [Nat]). (ShowMatC ns, NS ns) => [TestBatch]
testLawsMat' =
  [functor z, applicative z, monoid z, monad z, semigroup (z, Fixed (10 :: Int)), foldable z1] --  , traversable z]
 where
  z = undefined :: Mat ns (MM.Sum Integer, String, MM.Sum Int)
  z1 = undefined :: Mat ns (String, Integer, String, Int, Bool)

-- testLawsMat @'[2,3,4]
testLawsMatIO :: forall (ns :: [Nat]). (ShowMatC ns, NS ns) => IO ()
testLawsMatIO = traverse_ verboseBatch (testLawsMat @ns)

testLawsMatIO' :: forall (ns :: [Nat]). (ShowMatC ns, NS ns) => IO ()
testLawsMatIO' = traverse_ verboseBatch (testLawsMat' @ns)

doit :: IO ()
doit = defaultMain suite

m345 :: Mat '[3, 4, 5] Char
m345 = mat' ['A' .. '|']

m345' :: Mat '[3, 4, 5] Int
m345' = mat' [1 .. 60]

m35 :: Mat '[3, 5] Int
m35 = mat' [1 .. 15]

suite :: TestTree
suite =
  testGroup
    "TestMat"
    [ testCase "gen" $
        gen @'[2, 3] id
          @?= MatU (V.fromList [0 .. 5]) (_2P :| [_3P])
    , testCase "gen" $
        gen @'[9] id
          @?= MatU (V.fromList [0 .. 8]) (_9P :| [])
    , testCase "get index 0" $
        indexMat (FinMatU 0 (_3P :| [_4P, _5P])) m345
          @?= 'A'
    , testCase "get index 4" $
        indexMat (FinMatU 4 (_2P :| [_3P, _6P])) (gen' @'[2, 3, 6] id)
          @?= [1, 1, 5]
    , testCase "get index 4" $
        indexMat (finMatC @'[2, 1, 5]) (gen' @'[2, 3, 6] id)
          @?= [2, 1, 5]
    , testCase "get index 2" $
        m345 ^. ixMat' @'[1, 2, 1]
          @?= 'F'
    , testCase "update index 2" $
        matToNestedListC (m345' & ixMat' @'[1, 2, 1] %~ succ)
          @?= [[[1, 2, 3, 4, 5], [7, 7, 8, 9, 10], [11, 12, 13, 14, 15], [16, 17, 18, 19, 20]], [[21, 22, 23, 24, 25], [26, 27, 28, 29, 30], [31, 32, 33, 34, 35], [36, 37, 38, 39, 40]], [[41, 42, 43, 44, 45], [46, 47, 48, 49, 50], [51, 52, 53, 54, 55], [56, 57, 58, 59, 60]]]
    , testCase "reverseRows" $
        matToNestedListC (reverseRows m345)
          @?= [["EDCBA", "JIHGF", "ONMLK", "TSRQP"], ["YXWVU", "^]\\[Z", "cba`_", "hgfed"], ["mlkji", "rqpon", "wvuts", "|{zyx"]]
    , testCase "reverseT" $
        matToNestedListC (reverseT m345)
          @?= [["|{zyx", "wvuts", "rqpon", "mlkji"], ["hgfed", "cba`_", "^]\\[Z", "YXWVU"], ["TSRQP", "ONMLK", "JIHGF", "EDCBA"]]
    , testCase "reverseRows 2" $
        reverseRows (reverseRows m345)
          @?= m345
    , testCase "update index 2" $
        matToNestedListC (m345' & ixMat' @'[1, 2, 1] %~ succ)
          @?= [[[1, 2, 3, 4, 5], [7, 7, 8, 9, 10], [11, 12, 13, 14, 15], [16, 17, 18, 19, 20]], [[21, 22, 23, 24, 25], [26, 27, 28, 29, 30], [31, 32, 33, 34, 35], [36, 37, 38, 39, 40]], [[41, 42, 43, 44, 45], [46, 47, 48, 49, 50], [51, 52, 53, 54, 55], [56, 57, 58, 59, 60]]]
    , testCase "transpose" $
        matToNestedListC (transposeMat m35)
          @?= [[1, 6, 11], [2, 7, 12], [3, 8, 13], [4, 9, 14], [5, 10, 15]]
    , testCase "transpose2" $
        transposeMat (transposeMat m35)
          @?= m35
    , testCase "fmap" $
        matToNestedListC (fmap (show . succ) m35)
          @?= [["2", "3", "4", "5", "6"], ["7", "8", "9", "10", "11"], ["12", "13", "14", "15", "16"]]
    , testCase "totuple" $
        toTupleC (mat' @'[2, 3, 2] @Int [1 .. 12])
          @?= (((1, 2), (3, 4), (5, 6)), ((7, 8), (9, 10), (11, 12)))
    , testCase "fromtuple" $
        fromTupleC (((1, 2), (3, 4), (5, 6)), ((7, 8), (9, 10), (11, 12)))
          @?= mat' @'[2, 3, 2] @Int [1 .. 12]
    , testCase "change row" $
        (mat' @'[3, 4] @Int [1 .. 12] & ixSlice @'[2, 3] .~ 999)
          @?= mat' @'[3, 4] [1, 2, 3, 4, 5, 6, 999, 8, 9, 10, 11, 12]
    , testCase "change row" $
        (mat' @'[3, 4] @Int [1 .. 12] & ixSlice @'[1] *~ 999)
          @?= mat' @'[3, 4] [999, 1998, 2997, 3996, 5, 6, 7, 8, 9, 10, 11, 12]
    , testCase "change row" $
        m345 ^. ixSlice @'[2, 3]
          @?= mat' @'[5] ['_' .. 'c']
    , testCase "change row" $
        (m35 & ixSlice @'[2] . traverse *~ 100)
          @?= mat' @'[3, 5] [1, 2, 3, 4, 5, 600, 700, 800, 900, 1000, 11, 12, 13, 14, 15]
    , testCase "change row" $
        (mat' @'[2, 1, 2, 3, 4] @Int [1 .. 48] & ixSlice @'[2, 1, 1] . traverse *~ 100)
          @?= mat' @'[2, 1, 2, 3, 4] [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 2500, 2600, 2700, 2800, 2900, 3000, 3100, 3200, 3300, 3400, 3500, 3600, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48]
    , testCase "change row" $
        m345 ^. ixSlice @'[2]
          @?= mat' @'[4, 5] ['U' .. 'h']
    , testCase "not as useful: nests all stuff" $
        fmap sum (matToNestedVecC @'[2, 3] (mat' @_ @Int [1 .. 6]))
          @?= 6 .| 15
    , testCase "mapLeaf: change the lowest rows into lists" $
        mapLeaf (const sum) (mat' @'[4, 3] @Int [1 .. 12])
          @?= mat' @'[4] [6, 15, 24, 33]
    , testCase "mapLeafSimple" $
        mapLeafSimple (fmap . (,) . fmPos) (gen' @(NN 43) id)
          @?= mat' @'[4, 3] [(0, [1, 1]), (0, [1, 2]), (0, [1, 3]), (3, [2, 1]), (3, [2, 2]), (3, [2, 3]), (6, [3, 1]), (6, [3, 2]), (6, [3, 3]), (9, [4, 1]), (9, [4, 2]), (9, [4, 3])]
    , testCase "toLeaves" $
        toLeaves (gen' @(NN 23) id)
          @?= mat' @'[2] [mat' @'[3] [[1, 1], [1, 2], [1, 3]], mat' [[2, 1], [2, 2], [2, 3]]]
    , testCase "toLeaves" $
        mat' @'[2] [mat' @'[3] [[1, 1], [1, 2], [1, 3]], mat' [[2, 1], [2, 2], [2, 3]]]
          @?= toLeaves (gen' @(NN 23) id)
    , testCase "fromLeavesInternalC toLeaves" $
        fromLeavesInternalC (toLeaves (gen' @(NN 3214) id))
          @?= gen' @(NN 3214) id
    , testCase "foldMapLeaf" $
        foldMapLeaf (\i m -> [(fmPos i, sum m, toList m)]) (mm @(NN 234))
          @?= [(0, 10, [1, 2, 3, 4]), (4, 26, [5, 6, 7, 8]), (8, 42, [9, 10, 11, 12]), (12, 58, [13, 14, 15, 16]), (16, 74, [17, 18, 19, 20]), (20, 90, [21, 22, 23, 24])]
    , testCase "foldMapLeafR" $
        foldMapLeafR (\i m -> [(fmPos i, sum m, toList m)]) (mm @(NN 234))
          @?= [(20, 90, [21, 22, 23, 24]), (16, 74, [17, 18, 19, 20]), (12, 58, [13, 14, 15, 16]), (8, 42, [9, 10, 11, 12]), (4, 26, [5, 6, 7, 8]), (0, 10, [1, 2, 3, 4])]
    , testCase "addition" $
        mat' @'[2, 3] @Int [1 .. 6] + mat' [100 .. 105]
          @?= mat' [101, 103, 105, 107, 109, 111]
    , testCase "multiplication" $
        mat' @'[2, 3] @Int [1 .. 6] * mat' [100 .. 105]
          @?= mat' [100, 202, 306, 412, 520, 630] -- note: have to use mat' for inference to work
    , testCase "transpose" $
        transposeMat (mat' @'[2, 3] @Int [1 .. 6])
          @?= mat' [1, 4, 2, 5, 3, 6]
    , testCase "transpose iso" $
        transposeMat (transposeMat m345)
          @?= m345
    , testCase "diagonal" $
        diagonal (mat' @'[3, 3, 4] @Int [1 .. 36])
          @?= mat' [1, 2, 3, 4, 17, 18, 19, 20, 33, 34, 35, 36]
    , testCase "diagonal" $
        diagonal (gen @'[4, 4] succ)
          @?= mat' [1, 6, 11, 16]
    , testCase "diagonal" $
        diagonal (diagonal (diagonal (gen' @(NN 3333) id)))
          @?= mat' @'[3] [[1, 1, 1, 1], [2, 2, 2, 2], [3, 3, 3, 3]]
    , testCase "fromNSP" $
        fromNSP @'[4, 2, 3, 5, 1]
          @?= _4P :| [_2P, _3P, _5P, _1P]
    , testCase "finMatMatrix" $
        finMatMatrix @'[2, 3, 1]
          @?= mat' (toList (N.map (fr . (nonEmptyToFinMat <=< toPositives)) ([1, 1, 1] :| [[1, 2, 1], [1, 3, 1], [2, 1, 1], [2, 2, 1], [2, 3, 1]])))
    , testCase "insert row" $
        insertRow @2 (mat' @'[3, 4] [100 .. 111]) (mat' @'[2, 3, 4] @Int [1 .. 24])
          @?= mat' [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24]
    , testCase "insert column" $
        insertCol @2 (mat' @'[2, 4] [100 .. 107]) (mat' @'[2, 3, 4] @Int [1 .. 24])
          @?= mat' [1, 2, 3, 4, 100, 101, 102, 103, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 104, 105, 106, 107, 17, 18, 19, 20, 21, 22, 23, 24]
    , testCase "delete row" $
        deleteRow @2 (mat' @'[2, 3, 4] @Int [1 .. 24])
          @?= mat' [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]
    , testCase "insert/delete row" $
        deleteRow @2 (insertRow @2 (mat' @'[4, 5] [100 .. 119]) m345')
          @?= m345'
    , testCase "to nested lists" $
        matToNestedListC (mat' @'[2, 3, 4] @Int [1 .. 24])
          @?= [[[1, 2, 3, 4], [5, 6, 7, 8], [9, 10, 11, 12]], [[13, 14, 15, 16], [17, 18, 19, 20], [21, 22, 23, 24]]]
    , testCase "concat vertically" $
        matToNestedListC (appendV (mat' @'[2, 3, 2] [1 .. 12]) (mat' @'[5, 3, 2] @Int [100 .. 129]))
          @?= [[[1, 2], [3, 4], [5, 6]], [[7, 8], [9, 10], [11, 12]], [[100, 101], [102, 103], [104, 105]], [[106, 107], [108, 109], [110, 111]], [[112, 113], [114, 115], [116, 117]], [[118, 119], [120, 121], [122, 123]], [[124, 125], [126, 127], [128, 129]]]
    , testCase "concat vertically" $
        matToNestedListC (appendV (mat' @'[2, 3] [1 .. 6]) (mat' @'[7, 3] @Int [100 .. 120]))
          @?= [[1, 2, 3], [4, 5, 6], [100, 101, 102], [103, 104, 105], [106, 107, 108], [109, 110, 111], [112, 113, 114], [115, 116, 117], [118, 119, 120]]
    , testCase "concat horizontally" $
        matToNestedListC (appendH (mat' @'[5, 2, 2] [1 .. 20]) (mat' @'[5, 3, 2] @Int [100 .. 129]))
          @?= [[[1, 2], [3, 4], [100, 101], [102, 103], [104, 105]], [[5, 6], [7, 8], [106, 107], [108, 109], [110, 111]], [[9, 10], [11, 12], [112, 113], [114, 115], [116, 117]], [[13, 14], [15, 16], [118, 119], [120, 121], [122, 123]], [[17, 18], [19, 20], [124, 125], [126, 127], [128, 129]]]
    , testCase "concat horizontally" $
        matToNestedListC (appendH (mat' @'[3, 2] @Int [1 .. 6]) (mat' @'[3, 7] @Int [100 .. 120]))
          @?= [[1, 2, 100, 101, 102, 103, 104, 105, 106], [3, 4, 107, 108, 109, 110, 111, 112, 113], [5, 6, 114, 115, 116, 117, 118, 119, 120]]
    , testCase "consMat" $
        (gen @'[3, 4] succ ^. consMat)
          @?= (1 .: 2 .: 3 .| 4, 5 .: 6 .: 7 .| 8 .|| (9 .: 10 .: 11 .| 12))
    , testCase "snocMat" $
        (gen @'[3, 4] succ ^. snocMat)
          @?= (1 .: 2 .: 3 .| 4 .|| (5 .: 6 .: 7 .| 8), 9 .: 10 .: 11 .| 12)
    , testCase "consMat" $
        (gen @'[3, 4] succ & consMat . _1 +~ 1000)
          @?= ((1001 .: 1002 .: 1003 .| 1004) .:: (5 .: 6 .: 7 .| 8) .|| (9 .: 10 .: 11 .| 12))
    , testCase "snocMat" $
        (gen @'[3, 4] succ & snocMat . _2 +~ 1000)
          @?= ((1 .: 2 .: 3 .| 4) .:: (5 .: 6 .: 7 .| 8) .|| (1009 .: 1010 .: 1011 .| 1012))
    , testCase "consMat" $
        (gen @'[5] succ ^. consMat)
          @?= (1, 2 .: 3 .: 4 .| 5)
    , testCase "snocMat" $
        (gen @'[5] succ ^. snocMat)
          @?= (1 .: 2 .: 3 .| 4, 5)
    , testCase "consMat" $
        (gen @'[5] succ & consMat . _1 +~ 1000)
          @?= (1001 .: 2 .: 3 .: 4 .| 5)
    , testCase "consMat" $
        (gen @'[5] succ & consMat . _2 +~ 1000)
          @?= (1 .: 1002 .: 1003 .: 1004 .| 1005)
    , testCase "snocMat" $
        (gen @'[5] succ & snocMat . _2 +~ 1000)
          @?= (1 .: 2 .: 3 .: 4 .| 1005)
    , testCase "snocMat" $
        (gen @'[5] succ & snocMat . _1 +~ 1000)
          @?= (1001 .: 1002 .: 1003 .: 1004 .| 5)
    , testCase "consMat" $
        (gen @'[1] succ ^. consMat)
          @?= (1, Eof1)
    , testCase "snocMat" $
        (gen @'[1] succ ^. snocMat)
          @?= (Eof1, 1)
    , testCase "consMat" $
        (gen @'[1, 4] succ ^. consMat)
          @?= (1 .: 2 .: 3 .| 4, EofN)
    , testCase "snocMat" $
        (gen @'[1, 4] succ ^. snocMat)
          @?= (EofN, 1 .: 2 .: 3 .| 4)
    , testCase "consMat" $
        (gen @'[1, 4] succ & consMat . _1 +~ 999)
          @?= se2 (1000 .: 1001 .: 1002 .| 1003)
    , testCase "snocMat" $
        (gen @'[1, 4] succ & snocMat . _2 +~ 999)
          @?= se2 (1000 .: 1001 .: 1002 .| 1003)
    , testCase "swapMat" $
        swapMat @'[2, 3, 1] @'[2, 1, 1] (gen @'[2, 3, 4] id)
          @?= mat' [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 20, 13, 14, 15, 16, 17, 18, 19, 12, 21, 22, 23]
    , testCase "matToNestedVecC" $
        nestedVecToMatC (matToNestedVecC m345)
          @?= m345 -- works without @'[3,4,5] cos @?= tells us the type
    , testCase "delete item from 1d mat'" $
        deleteRow @4 (mat' @'[10] @Int [1 .. 10])
          @?= mat' [1, 2, 3, 5, 6, 7, 8, 9, 10]
    , testCase "redim" $
        redim (mat' @'[2, 3, 5] @Int [1 .. 30])
          @?= mat' @'[6, 5] @Int [1 .. 30]
    , testCase "redim" $
        redim (mat' @'[5, 9, 4] @Int [1 .. 180])
          @?= mat' @'[3, 6, 10] @Int [1 .. 180]
    , testCase "redim" $
        redim (mat' @'[18] @Int [1 .. 18])
          @?= mat' @'[3, 2, 3] @Int [1 .. 18]
    , testCase "redim" $
        redim (mat' @'[3, 2, 3] @Int [1 .. 18])
          @?= mat' @'[18] @Int [1 .. 18]
    , testCase "diagonal" $
        diagonal (gen @'[4, 4] succ)
          @?= mat' @'[4] [1, 6, 11, 16]
    , testCase "diagonal" $
        diagonal (gen @'[3, 3, 4, 2] succ)
          @?= mat' @'[3, 4, 2] [1, 2, 3, 4, 5, 6, 7, 8, 33, 34, 35, 36, 37, 38, 39, 40, 65, 66, 67, 68, 69, 70, 71, 72]
    , testCase "diagonal" $
        diagonal (mm @(NN 99))
          @?= mat' @'[9] [1, 11, 21, 31, 41, 51, 61, 71, 81]
    , testCase "multMat" $
        multMat (mat' @'[2, 5] @Int [1 .. 10]) (mat' @'[5, 6] @Int [1 .. 30])
          @?= mat' @'[2, 6] [255, 270, 285, 300, 315, 330, 580, 620, 660, 700, 740, 780]
    , testCase "universe1 enum" $
        toNonEmpty (finMatMatrix @'[2, 3, 7])
          @?= universe1 @(FinMat '[2, 3, 7])
    , testCase "finmat enum" $
        toList (finMatMatrix @'[2, 3, 7])
          @?= toList fmi237'
    , testCase "D3" $
        mat' @(D3 2 3 4) @Int [1 .. 24]
          @?= mat' @'[2, 3, 4] [1 .. 24]
    , testCase "ixMat" $
        (mat' @'[2, 3, 4] @Int [1 .. 24] & ixMat (finMatC @'[2, 3, 1]) +~ 100)
          @?= mat' @'[2, 3, 4] [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 121, 22, 23, 24]
    , testCase "ixMat" $
        (mat' @'[2, 3, 4] @Int [1 .. 24] & ixMat (finMatC @'[2, 3, 4]) +~ 100)
          @?= mat' @'[2, 3, 4] [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 124]
    , testCase "read" $
        (read @(Mat (D1 4) Int) $ show $ mat' @'[4] @Int [1 .. 4])
          @?= (1 .: 2 .: 3 .| 4)
    , testCase "read" $
        let m = gen' @'[1] id
         in read (show m) @?= m
    , testCase "read" $
        let m = gen' @'[1, 1, 1, 1] id
         in read (show m) @?= m
    , testCase "read" $
        let m = gen' @'[2, 3, 4, 5] id
         in read (show m) @?= m
    , testCase "read" $
        let m = gen' @'[9, 2, 1] id
         in read (show m) @?= m
    , testCase "read" $
        let m = gen' @'[1, 2, 3] id
         in read (show m) @?= m
    , testCase "read" $
        let m = ('x', True, ['a' .. 'z'], gen' @'[1, 2, 3] id, False)
         in read (show m) @?= m
    , testCase "read" $
        let m = mat' @'[4, 5] @Int [1 .. 20]
         in read @(Mat (D2 4 5) Int) (show m) @?= m
    , testCase "read" $
        let m = mat' @'[1, 2, 3, 4] @Int [1 .. 24]
         in read (show m) @?= m
    , testCase "read" $
        let m = toND @1 (mm @(NN 2352))
         in read (show m) @?= m
    , testCase "read" $
        let m = toND @2 (mm @(NN 2352))
         in read (show m) @?= m
    , testCase "read" $
        let m = mat' @'[26] ['a' .. 'z']
         in read (show m) @?= m
    , testCase "sortByRows" $
        sortByRows (flip compare) (mat' @'[4, 2] [10, 9, 1, 2, 100, 200, 300, 400])
          @?= mat' @_ @Int [10, 9, 2, 1, 200, 100, 400, 300]
    , testCase "sortByT" $
        sortByT (flip compare) (mat' @'[4] @Int [10, 9, 1, 2])
          @?= (10 .: 9 .: 2 .| 1)
    , testCase "sortByT" $
        sortByT compare (mat' @'[4] @Int [10, 9, 1, 2])
          @?= (1 .: 2 .: 9 .| 10)
    , testCase "sortByRows" $
        sortByRows compare (mat' @'[4, 2] @Int [10, 9, 1, 2, 100, 200, 300, 400])
          @?= mat' [9, 10, 1, 2, 100, 200, 300, 400]
    , testCase "totuple" $
        toTupleC (vec' "abc")
          @?= ('a', 'b', 'c')
    , testCase "totuple" $
        toTupleC (vec' "a")
          @?= One 'a'
    , testCase "fromtuple" $
        fromTupleC (One 'a')
          @?= se1 'a'
    , testCase "fromtuple" $
        fromTupleC @_ @Int (1, 2, 3)
          @?= 1 .: 2 .| 3
    , testCase "consMat" $
        (mat' @'[1] "x" ^. consMat)
          @?= ('x', Eof1)
    , testCase "consMat" $
        (mat' @'[1, 1] "x" ^. consMat)
          @?= (se1 'x', EofN)
    , testCase "consMat" $
        (mat' @'[1, 1, 1] "x" ^. consMat)
          @?= (se2 (se1 'x'), EofN)
    , testCase "consMat" $
        (mat' @'[4] "xyz{" ^. consMat)
          @?= ('x', vec' "yz{")
    , testCase "consMat" $
        (mat' @'[1, 4] "xyz{" ^. consMat)
          @?= (mat' "xyz{", EofN)
    , testCase "consMat" $
        (mat' @'[4, 1] "xyz{" ^. consMat)
          @?= (se1 'x', mat' @'[3, 1] "yz{")
    , testCase "consMat" $
        (mat' @'[5, 3] ['A' .. 'O'] ^. consMat)
          @?= (mat' @'[3] "ABC", mat' @'[4, 3] ['D' .. 'O'])
    , testCase "snocMat" $
        (mat' @'[1] "x" ^. snocMat)
          @?= (Eof1, 'x')
    , testCase "snocMat" $
        (mat' @'[1, 1] "x" ^. snocMat)
          @?= (EofN, se1 'x')
    , testCase "snocMat" $
        (mat' @'[1, 1, 1] "x" ^. snocMat)
          @?= (EofN, se2 (se1 'x'))
    , testCase "snocMat" $
        (mat' @'[4] "xyz{" ^. snocMat)
          @?= (vec' "xyz", '{')
    , testCase "snocMat" $
        (mat' @'[1, 4] "xyz{" ^. snocMat)
          @?= (EofN, vec' "xyz{")
    , testCase "snocMat" $
        (mat' @'[4, 1] "xyz{" ^. snocMat)
          @?= (mat' @'[3, 1] "xyz", se1 '{')
    , testCase "snocMat" $
        (mat' @'[5, 3] ['A' .. 'O'] ^. snocMat)
          @?= (mat' @'[4, 3] ['A' .. 'L'], mat' @'[3] "MNO")
    , testCase "field lens" $
        (mat' @'[3, 3, 4] @Int [1 .. 36] ^. _r3 . _r1)
          @?= vec' @4 [25, 26, 27, 28]
    , testCase "field lens" $
        (mat' @'[3, 3, 4] @Int [1 .. 36] ^. _r3 . _r1)
          @?= vec' @4 [25, 26, 27, 28]
    , testCase "field lens update" $
        (mat' @'[2, 1, 4] ['A' .. 'H'] & _r2 . _r1 . _r3 %~ toLower)
          @?= mat' "ABCDEFgH"
    , testCase "field lens" $
        (mat' @'[7] @Int [1 .. 7] ^. _r3)
          @?= 3
    , testCase "field lens" $
        (mat' @'[7, 4] @Int [1 .. 28] ^. _r3 . _r2)
          @?= 10
    , testCase "subsetRows" $
        subsetRows @2 @2 (gen @'[2, 5] succ)
          @?= mat' @'[1, 5] [6, 7, 8, 9, 10]
    , testCase "subsetRows" $
        subsetRows @1 @2 (gen @'[2, 5] succ)
          @?= mat' @'[2, 5] [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    , testCase "subsetRows" $
        subsetRows @2 @4 (gen @'[4, 5] succ)
          @?= mat' @'[3, 5] [6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]
    , testCase "subsetRows" $
        subsetRows @2 @4 (gen @'[5, 7] succ)
          @?= mat' @'[3, 7] [8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28]
    , testCase "subsetRows" $
        subsetRows @2 @2 (gen @'[4] succ)
          @?= mat' @'[1] [2]
    , testCase "subsetRows" $
        subsetRows @2 @3 (gen @'[4] succ)
          @?= mat' @'[2] [2, 3]
    , testCase "subsetCols" $
        subsetCols @2 @4 (gen @'[7, 6] succ)
          @?= mat' @'[7, 3] [2, 3, 4, 8, 9, 10, 14, 15, 16, 20, 21, 22, 26, 27, 28, 32, 33, 34, 38, 39, 40]
    , testCase "subsetCols" $
        subsetCols @1 @1 (gen @'[3, 5] succ)
          @?= mat' @'[3, 1] [1, 6, 11]
    , testCase "subsetCols" $
        subsetCols @1 @2 (gen @'[3, 5] succ)
          @?= mat' @'[3, 2] [1, 2, 6, 7, 11, 12]
    , testCase "sliceC 35" $
        sliceC @'[3, 5] @'[4, 6, 2] (gen' id)
          @?= mat' @'[2] [[3, 5, 1], [3, 5, 2]]
    , testCase "sliceC 3" $
        sliceC @'[3] @'[4, 6, 2] (gen' id)
          @?= mat' @'[6, 2] [[3, 1, 1], [3, 1, 2], [3, 2, 1], [3, 2, 2], [3, 3, 1], [3, 3, 2], [3, 4, 1], [3, 4, 2], [3, 5, 1], [3, 5, 2], [3, 6, 1], [3, 6, 2]]
    , testCase "sliceC' 35" $ -- (3-1) * 6 + (5-1) == 16 cos all indexes start at 1
        sliceC' @'[4, 6] @'[4, 6, 2] (FinMatU 16 (_4P :| [_6P])) (gen' id)
          @?= mat' @'[2] [[3, 5, 1], [3, 5, 2]]
    , testCase "sliceC' 3" $
        sliceC' @'[4] @'[4, 6, 2] (FinMatU 2 (_4P :| [])) (gen' id)
          @?= mat' @'[6, 2] [[3, 1, 1], [3, 1, 2], [3, 2, 1], [3, 2, 2], [3, 3, 1], [3, 3, 2], [3, 4, 1], [3, 4, 2], [3, 5, 1], [3, 5, 2], [3, 6, 1], [3, 6, 2]]
    , testCase "sliceC' 35" $
        map (\i -> sliceC' @'[5, 3] @'[5, 3, 2] (FinMatU i (_5P :| [_3P])) (gen succ)) [0 .. 14]
          @?= [1 .| 2, 3 .| 4, 5 .| 6, 7 .| 8, 9 .| 10, 11 .| 12, 13 .| 14, 15 .| 16, 17 .| 18, 19 .| 20, 21 .| 22, 23 .| 24, 25 .| 26, 27 .| 28, 29 .| 30]
    , testCase "sliceC' 2" $
        sliceC' @'[1, 7, 3, 2, 6] @'[1, 7, 3, 2, 6] (FinMatU 2 (_1P :| [_7P, _3P, _2P, _6P])) (gen' id)
          @?= [1, 1, 1, 1, 3]
    , testCase "sliceC' 43" $
        sliceC' @'[1, 7, 3, 2, 6] @'[1, 7, 3, 2, 6] (FinMatU 43 (_1P :| [_7P, _3P, _2P, _6P])) (gen' id)
          @?= [1, 2, 1, 2, 2]
    , testCase "sliceC 2" $
        sliceC @'[1, 1, 1, 1, 3] @'[1, 7, 3, 2, 6] (gen' id)
          @?= [1, 1, 1, 1, 3]
    , testCase "sliceC 43" $
        sliceC @'[1, 2, 1, 2, 2] @'[1, 7, 3, 2, 6] (gen' id)
          @?= [1, 2, 1, 2, 2]
    , testCase "sliceUpdateC' 0" $
        sliceUpdateC' @'[4, 3] @'[4, 3, 2] (FinMatU 0 (_4P :| [_3P])) (gen succ) (mat [999 ..])
          @?= mat' @'[4, 3, 2] [999, 1000, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24]
    , testCase "sliceUpdateC' 1" $
        sliceUpdateC' @'[4, 3] @'[4, 3, 2] (FinMatU 1 (_4P :| [_3P])) (gen succ) (mat [999 ..])
          @?= mat' @'[4, 3, 2] [1, 2, 999, 1000, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24]
    , testCase "sliceUpdateC' 2" $
        sliceUpdateC' @'[4, 3] @'[4, 3, 2] (FinMatU 2 (_4P :| [_3P])) (gen succ) (mat [999 ..])
          @?= mat' @'[4, 3, 2] [1, 2, 3, 4, 999, 1000, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24]
    , testCase "sliceUpdateC' 5" $
        sliceUpdateC' @'[4, 3] @'[4, 3, 2] (FinMatU 5 (_4P :| [_3P])) (gen succ) (mat [999 ..])
          @?= mat' @'[4, 3, 2] [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 999, 1000, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24]
    , testCase "sliceUpdateC' 11" $
        sliceUpdateC' @'[4, 3] @'[4, 3, 2] (FinMatU 11 (_4P :| [_3P])) (gen succ) (mat [999 ..])
          @?= mat' @'[4, 3, 2] [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 999, 1000]
    , testCase "sliceUpdateC 0" $
        sliceUpdateC @'[1, 1] @'[4, 3, 2] (gen succ) (mat [999 ..])
          @?= mat' @'[4, 3, 2] [999, 1000, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24]
    , testCase "sliceUpdateC 1" $
        sliceUpdateC @'[1, 2] @'[4, 3, 2] (gen succ) (mat [999 ..])
          @?= mat' @'[4, 3, 2] [1, 2, 999, 1000, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24]
    , testCase "sliceUpdateC 2" $
        sliceUpdateC @'[1, 3] @'[4, 3, 2] (gen succ) (mat [999 ..])
          @?= mat' @'[4, 3, 2] [1, 2, 3, 4, 999, 1000, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24]
    , testCase "sliceUpdateC 5" $
        sliceUpdateC @'[2, 3] @'[4, 3, 2] (gen succ) (mat [999 ..])
          @?= mat' @'[4, 3, 2] [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 999, 1000, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24]
    , testCase "sliceUpdateC 11" $
        sliceUpdateC @'[4, 3] @'[4, 3, 2] (gen succ) (mat [999 ..])
          @?= mat' @'[4, 3, 2] [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 999, 1000]
    , testCase "sliceUpdateC' 0" $
        sliceUpdateC' @'[4, 3] @'[4, 3] (FinMatU 0 (_4P :| [_3P])) (gen succ) 999
          @?= mat' @'[4, 3] [999, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]
    , testCase "sliceUpdateC' 7" $
        sliceUpdateC' @'[4, 3] @'[4, 3] (FinMatU 7 (_4P :| [_3P])) (gen succ) 999
          @?= mat' @'[4, 3] [1, 2, 3, 4, 5, 6, 7, 999, 9, 10, 11, 12]
    , testCase "sliceUpdateC 0" $
        sliceUpdateC @'[1, 1] @'[4, 3] (gen succ) 999
          @?= mat' @'[4, 3] [999, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]
    , testCase "sliceUpdateC' 7" $
        sliceUpdateC @'[3, 2] @'[4, 3] (gen succ) 999
          @?= mat' @'[4, 3] [1, 2, 3, 4, 5, 6, 7, 999, 9, 10, 11, 12]
    , testCase "sliceUpdateC' 0" $
        sliceUpdateC' @'[7] @'[7] (FinMatU 0 (_7P :| [])) (gen succ) 999
          @?= mat' @'[7] [999, 2, 3, 4, 5, 6, 7]
    , testCase "sliceUpdateC' 4" $
        sliceUpdateC' @'[7] @'[7] (FinMatU 4 (_7P :| [])) (gen succ) 999
          @?= mat' @'[7] [1, 2, 3, 4, 999, 6, 7]
    , testCase "sliceUpdateC 0" $
        sliceUpdateC @'[1] @'[7] (gen succ) 999
          @?= mat' @'[7] [999, 2, 3, 4, 5, 6, 7]
    , testCase "sliceUpdateC 4" $
        sliceUpdateC @'[5] @'[7] (gen succ) 999
          @?= mat' @'[7] [1, 2, 3, 4, 999, 6, 7]
    , testCase "toND" $
        toND @1 (gen' @'[5, 3] id)
          @?= mat' @'[5]
            ( map
                mat'
                [ [[1, 1], [1, 2], [1, 3]]
                , [[2, 1], [2, 2], [2, 3]]
                , [[3, 1], [3, 2], [3, 3]]
                , [[4, 1], [4, 2], [4, 3]]
                , [[5, 1], [5, 2], [5, 3]]
                ]
            )
    , testCase "concatMat toND" $
        let m = gen' @'[5, 3, 2] id
         in concatMat (toND @1 m) @?= m
    , testCase "concatMat toND" $
        let m = gen' @'[5, 3, 2] id
         in concatMat (toND @2 m) @?= m
    , testCase "nonEmptyMatsToMat" $
        nonEmptyMatsToMat @10 (gen @'[2, 5] id :| [])
          @?= Left "LT: not enough elements: expected 10 found 1"
    , testCase "nonEmptyMatsToMat" $
        nonEmptyMatsToMat @1 (gen @'[2, 5] id :| [])
          @?= Right (mat' @'[1, 2, 5] [0, 1, 2, 3, 4, 5, 6, 7, 8, 9])
    , testCase "nonEmptyMatsToMat" $
        nonEmptyMatsToMat @2 (gen @'[2, 5] succ :| [gen @'[2, 5] (+ 100)])
          @?= Right (mat' @'[2, 2, 5] [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109])
    , testCase "nonEmptyMatsToMat" $
        nonEmptyMatsToMat @1 (gen @'[2, 5] succ :| [gen @'[2, 5] (+ 100)])
          @?= Left "GT: too many elements: expected 1"
    , testCase "nonEmptyMatsToMat" $
        nonEmptyMatsToMat @3 (gen @'[2, 5] succ :| [gen @'[2, 5] (+ 100)])
          @?= Left "LT: not enough elements: expected 3 found 2"
    , testCase "cartesian" $
        cartesian (,) (gen @'[4] succ) (gen @'[7] (+ 101))
          @?= mat' @'[4, 7] [(1, 101), (1, 102), (1, 103), (1, 104), (1, 105), (1, 106), (1, 107), (2, 101), (2, 102), (2, 103), (2, 104), (2, 105), (2, 106), (2, 107), (3, 101), (3, 102), (3, 103), (3, 104), (3, 105), (3, 106), (3, 107), (4, 101), (4, 102), (4, 103), (4, 104), (4, 105), (4, 106), (4, 107)]
    , testCase "bulkMat" $
        (mat' @'[4, 4] ['a' .. 'p'] ^. bulkMat (finMatC @'[1, 2] .: finMatC @'[1, 4] .| (finMatC @'[4, 3])))
          @?= ('b' .: 'd' .| 'o')
    , testCase "bulkMat" $
        (mat' @'[4, 4] ['a' .. 'p'] & bulkMat (finMatC @'[1, 2] .: finMatC @'[1, 4] .: finMatC @'[3, 1] .| (finMatC @'[4, 3])) %~ fmap toUpper)
          @?= (('a' .: 'B' .: 'c' .| 'D') .:: ('e' .: 'f' .: 'g' .| 'h') .:: ('I' .: 'j' .: 'k' .| 'l') .|| ('m' .: 'n' .: 'O' .| 'p'))
    , testCase "findMatElems" $
        findMatElems ((== 0) . flip mod 5) (gen @'[2, 3, 5] succ)
          @?= [ (finMatC @'[1, 1, 5], 5)
              , (finMatC @'[1, 2, 5], 10)
              , (finMatC @'[1, 3, 5], 15)
              , (finMatC @'[2, 1, 5], 20)
              , (finMatC @'[2, 2, 5], 25)
              , (finMatC @'[2, 3, 5], 30)
              ]
    , testCase "permutationsMat" $
        permutationsMat @4 "abcd"
          @?= mat' @'[24, 4] "abcdbacdcbadbcadcabdacbddcbacdbacbdadbcabdcabcdadabcadbcabdcdbacbdacbadcdacbadcbacdbdcabcdabcadb"
    , testCase "swapRow" $
        swapRow @2 @5 (gen @'[6, 3, 2] succ)
          @?= mat' @'[6, 3, 2] [1, 2, 3, 4, 5, 6, 25, 26, 27, 28, 29, 30, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 7, 8, 9, 10, 11, 12, 31, 32, 33, 34, 35, 36]
    , testCase "swapRow'" $
        swapRow' (FinU _3P _7P) (FinU _3P _7P) (gen @'[7] succ)
          @?= mat' @'[7] [1, 2, 3, 4, 5, 6, 7]
    , testCase "swapRow'" $
        swapRow' (FinU _1P _7P) (FinU _7P _7P) (gen @'[7] succ)
          @?= mat' @'[7] [7, 2, 3, 4, 5, 6, 1]
    , testCase "swapRow'" $
        swapRow' (FinU _3P _3P) (FinU _2P _3P) (gen @'[3, 2, 2, 1] succ)
          @?= mat' @'[3, 2, 2, 1] [1, 2, 3, 4, 9, 10, 11, 12, 5, 6, 7, 8]
    , testCase "ixSlice" $
        (gen' @'[2, 4, 5] id ^. ixSlice @'[2])
          @?= mat' @'[4, 5] [[2, 1, 1], [2, 1, 2], [2, 1, 3], [2, 1, 4], [2, 1, 5], [2, 2, 1], [2, 2, 2], [2, 2, 3], [2, 2, 4], [2, 2, 5], [2, 3, 1], [2, 3, 2], [2, 3, 3], [2, 3, 4], [2, 3, 5], [2, 4, 1], [2, 4, 2], [2, 4, 3], [2, 4, 4], [2, 4, 5]]
    , testCase "ixSlice" $
        (gen' @'[2, 4, 5] id ^. ixSlice @'[2, 1])
          @?= mat' @'[5] [[2, 1, 1], [2, 1, 2], [2, 1, 3], [2, 1, 4], [2, 1, 5]]
    , testCase "ixSlice" $
        (gen' @'[2, 4, 5] id ^. ixSlice @'[2, 1, 5])
          @?= [2, 1, 5]
    , testCase "gen'" $
        (gen @'[4] succ ^. _row @4)
          @?= 4
    , testCase "gen'" $
        (gen @'[4, 3] succ ^. _row @4)
          @?= mat' @'[3] [10, 11, 12]
    , testCase "rowsToMat" $
        rowsToMat (vec' @2 [_F1, _F1]) (mm @(NN 57))
          @?= mat' @'[2, 7] [1, 2, 3, 4, 5, 6, 7, 1, 2, 3, 4, 5, 6, 7]
    , testCase "rowsToMat" $
        rowsToMat (vec' @2 [_F1, _F3]) (mm @(NN 57))
          @?= mat' @'[2, 7] [1, 2, 3, 4, 5, 6, 7, 15, 16, 17, 18, 19, 20, 21]
    , testCase "rowsToMat" $
        rowsToMat (vec' @4 [_F1, _F3, _F5, _F3]) (mm @(NN 57))
          @?= mat' @'[4, 7] [1, 2, 3, 4, 5, 6, 7, 15, 16, 17, 18, 19, 20, 21, 29, 30, 31, 32, 33, 34, 35, 15, 16, 17, 18, 19, 20, 21]
    , testCase "_row'" $
        (mm @(NN 235) ^. _row' _F1)
          @?= mat @'[3, 5] [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]
    , testCase "_row'" $ (mm @(NN 7) ^. _row' _F5) @?= 5
    , testCase "_row'" $ (mm @(NN 17) ^. _row' _F1 . _row' _F7) @?= 7
    , testCase "sliceToFinMat" $
        sliceToFinMat @(NN 123) @(NN 456)
          @?= FinMatU 8 (_4P :| [_5P, _6P])
    , testCase "sliceToFinMat" $
        sliceToFinMat @(NN 45) @(NN 456)
          @?= FinMatU 19 (_4P :| [_5P])
    , testCase "sliceToFinMat" $
        sliceToFinMat @(NN 12) @(NN 456)
          @?= FinMatU 1 (_4P :| [_5P])
    , testCase "sliceToFinMat" $
        sliceToFinMat @(NN 111) @(NN 456)
          @?= FinMatU 0 (_4P :| [_5P, _6P])
    , testCase "sliceToFinMat" $
        sliceToFinMat @(NN 11) @(NN 456)
          @?= FinMatU 0 (_4P :| [_5P])
    , testCase "sliceToFinMat" $
        sliceToFinMat @(NN 1) @(NN 4567)
          @?= FinMatU 0 (_4P :| [])
    , testCase "sliceToFinMat" $
        sliceToFinMat @(NN 3) @(NN 4567)
          @?= FinMatU 2 (_4P :| [])
    , testCase "consMat" $
        (mm @(NN 5) & consMat %~ (show *** fmap show))
          @?= mat' @'[5] ["1", "2", "3", "4", "5"]
    , testCase "consMat" $
        let z = se1 'x' ^. consMat
         in z ^. from (consMat @'[1]) @?= se1 'x'
    , testCase "consMat" $
        (('x', Eof1) ^. from (consMat @'[1]))
          @?= se1 'x'
    , testCase "nestedListToMatC" $
        nestedListToMatC @'[2, 3, 5] @Int [[[1, 2, 3, 4, 5], [6, 7, 8, 9, 10], [11, 12, 13, 14, 15]], [[16, 17, 18, 19, 20], [21, 22, 23, 24, 25], [26, 27, 28, 29, 30]]]
          @?= Right (mat' @'[2, 3, 5] [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30])
    , testCase "nestedListToMatC" $
        nestedListToMatC @'[3, 3, 5] @Int [[[1, 2, 3, 4, 5], [6, 7, 8, 9, 10], [11, 12, 13, 14, 15]], [[16, 17, 18, 19, 20], [21, 22, 23, 24, 25], [26, 27, 28, 29, 30]]]
          @?= Left "LT: not enough elements: expected 3 found 2"
    , testCase "nestedListToMatC" $
        nestedListToMatC @'[2, 3, 6] @Int [[[1, 2, 3, 4, 5], [6, 7, 8, 9, 10], [11, 12, 13, 14, 15]], [[16, 17, 18, 19, 20], [21, 22, 23, 24, 25], [26, 27, 28, 29, 30]]]
          @?= Left "not enough elements: expected 6 found 5"
    , testCase "indexRow" $
        indexRow (fr $ fin @7 1) (gen' @(NN 73) id)
          @?= vec' [[1, 1], [1, 2], [1, 3]]
    , testCase "indexRow" $
        indexRow (fr $ fin @7 3) (gen' @(NN 73) id)
          @?= vec' [[3, 1], [3, 2], [3, 3]]
    , testCase "indexRow" $
        indexRow (fr $ fin @7 7) (gen' @(NN 73) id)
          @?= vec' [[7, 1], [7, 2], [7, 3]]
    , testCase "readVec" $
        readVec @5 @Int (show (vec' @5 [1::Int ..5])) @?= [(vec' [1 .. 5], "")]
    , testCase "readMat2" $
        let m = mat' @'[3, 7] ['a' .. 'u']
         in readMat2 @3 @7 @Char (show m ++ "xyz") @?= [(m, "xyz")]
    , testCase "readVec" $
        let m = mat' @'[7] ['a' .. 'g']
         in readVec @7 @Char (show m ++ " xyz") @?= [(m, " xyz")]
    , testCase "readMat" $
        let m = mat' @'[3,7,2] [1::Int .. 42]
         in readMat @'[3,7,2] @Int (show m ++ "xyz") @?= [(m, "xyz")] -- dont need type application but here we have inference
    , testCase "readMat12" $
        let m = mat' @'[3,7,2] [1::Int .. 42]
         in readMat @'[3,7,2] @Int (show m ++ "xyz") @?= [(m, "xyz")] -- dont need type application but here we have inference
    , testCase "readMat3456" $
        let m = toMat2 (mm @(NN 3456))
         in readMat2 @3 @4 @(Mat2 5 6 Int) (show m ++ "xyz") @?= [(m, "xyz")] -- dont need type application but here we have inference
    , testCase "readMat23456" $
        let m = toMat2 (mm @(NN 23456))
         in readMat2 @2 @3 @(Mat '[4, 5, 6] Int) (show m ++ "xyz") @?= [(m, "xyz")] -- dont need type application but here we have inference
    , testCase "showMat" $
        showMat defShowOpts (mm @(NN 5)) @?= "Vec@5 [1,2,3,4,5]"
    , testCase "showMat" $
        showMat defShowOpts (mm @(NN 52)) @?= "Mat2@(5,2)\n  [\n     [1,2],\n     [3,4],\n     [5,6],\n     [7,8],\n     [9,10]\n  ]\n"
    , testCase "showMat" $
        showMat defShowOpts (mm @(NN 222)) @?= "Mat@[2,2,2]\n  [\n     [\n        [1,2],\n        [3,4]\n     ],[\n        [5,6],\n        [7,8]\n     ]\n  ]\n"
    , testCase "(.:)" $
        se1 @Int 99
          @?= vec' @1 [99]
    , testCase "(.:)" $
        (12 .: 44 .| 99)
          @?= vec' @3 @Int [12, 44, 99]
    , testCase "(.:)" $
        (5 .| 10 .:: 15 .| 20 .|| (25 .| 30))
          @?= mat2' @3 @2 @Int [5, 10, 15, 20, 25, 30]
    , testCase "(.:)" $
        se2 (5 .| 10 .:: 15 .| 20 .|| (25 .| 30))
          @?= mat' @'[1, 3, 2] @Int [5, 10, 15, 20, 25, 30]
    , testCase "nestedListToMatValidated" $
        let x = [[[[1 :: Int, 2, 3, 4], [5, 6, 7, 8], [9, 10, 11, 12]], [[13, 14, 15, 16], [17, 18, 19, 20], [21, 22, 23, 24, 25, 26, 27]]]]
         in nestedListToMatValidated @(NN 1234) x @?= Left "validateNestedListC: lengths=[4,4,4,4,4,7] ixes=[_1P,_2P,_3P]"
    , testCase "nestedListToMatValidated" $
        let x = [[[[1 :: Int, 2, 3, 4], [5, 6, 7, 8], [9, 10, 11, 12]], [[13, 14, 15, 16], [17, 18, 19, 20], []]]]
         in nestedListToMatValidated @(NN 1234) x @?= Left "validateNestedListC: lengths=[4,4,4,4,4,0] ixes=[_1P,_2P,_3P]"
    , testCase "nestedListToMatValidated" $
        let x = [[[[1 :: Int, 2, 3, 4], [5, 6, 7, 8], [9, 10, 11, 12]], [[13, 14, 15, 16], [17, 18, 19, 20], [21, 22, 23, 24]]]]
         in nestedListToMatValidated @(NN 1234) x @?= Right (mat' @'[1, 2, 3, 4] [1 .. 24])
    , testCase "matToNestedNonEmptyC" $
        matToNestedNonEmptyC (mm @(NN 234))
          @?= ((1 :| [2 :: Int, 3, 4]) :| [5 :| [6, 7, 8], 9 :| [10, 11, 12]]) :| [(13 :| [14, 15, 16]) :| [17 :| [18, 19, 20], 21 :| [22, 23, 24]]]
    , testCase "nestedNonEmptyToMatValidated" $
        let x = ((1 :| [2 :: Int, 3, 4]) :| [5 :| [6, 7, 8], 9 :| [10, 11, 12]]) :| [(13 :| [14, 15, 16]) :| [17 :| [18, 19, 20], 21 :| [22, 23, 24]]]
         in nestedNonEmptyToMatValidated @(NN 234) x @?= Right (mat' @'[2, 3, 4] [1 .. 24])
    , testCase "nestedNonEmptyToMatValidated" $
        let x = ((1 :| [2 :: Int, 3, 4]) :| [5 :| [6, 7, 8], 9 :| [10, 11, 12]]) :| [(13 :| []) :| [17 :| [18, 19, 20], 21 :| [22, 23, 24]]]
         in nestedNonEmptyToMatValidated @(NN 234) x @?= Left "validateNestedNonEmptyC: lengths=[4,4,4,1,4,4] ixes=[2,3]"
    , testCase "nestedNonEmptyToMatValidated" $
        let x = ((1 :| [2 :: Int, 3, 4]) :| [5 :| [6, 7, 8], 9 :| [10, 11, 12]]) :| [(13 :| [1 .. 20]) :| [17 :| [18, 19, 20], 21 :| [22, 23, 24]]]
         in nestedNonEmptyToMatValidated @(NN 234) x @?= Left "validateNestedNonEmptyC: lengths=[4,4,4,21,4,4] ixes=[2,3]"
    , testCase "tailsT" $
        tailsT (mm @(NN 52))
          @?= mat' @(NN 52)
            [ 1 :| [2, 3, 4, 5, 6, 7, 8, 9, 10]
            , 2 :| [3, 4, 5, 6, 7, 8, 9, 10]
            , 3 :| [4, 5, 6, 7, 8, 9, 10]
            , 4 :| [5, 6, 7, 8, 9, 10]
            , 5 :| [6, 7, 8, 9, 10]
            , 6 :| [7, 8, 9, 10]
            , 7 :| [8, 9, 10]
            , 8 :| [9, 10]
            , 9 :| [10]
            , 10 :| []
            ]
    , testCase "initsT" $
        initsT (mm @(NN 52))
          @?= mat' @(NN 52)
            [ 1 :| []
            , 1 :| [2]
            , 1 :| [2, 3]
            , 1 :| [2, 3, 4]
            , 1 :| [2, 3, 4, 5]
            , 1 :| [2, 3, 4, 5, 6]
            , 1 :| [2, 3, 4, 5, 6, 7]
            , 1 :| [2, 3, 4, 5, 6, 7, 8]
            , 1 :| [2, 3, 4, 5, 6, 7, 8, 9]
            , 1 :| [2, 3, 4, 5, 6, 7, 8, 9, 10]
            ]
    , testCase "ipostscanr" $
        ipostscanr (\i a zs -> (fmPos i, a) : zs) [] (mat @(NN 32) ['a' ..])
          @?= mat2' @3 @2 [[(0, 'a'), (1, 'b'), (2, 'c'), (3, 'd'), (4, 'e'), (5, 'f')], [(1, 'b'), (2, 'c'), (3, 'd'), (4, 'e'), (5, 'f')], [(2, 'c'), (3, 'd'), (4, 'e'), (5, 'f')], [(3, 'd'), (4, 'e'), (5, 'f')], [(4, 'e'), (5, 'f')], [(5, 'f')]]
    , testCase "ipostscanr" $
        ipostscanr (\i a zs -> (fmPos i, a) : zs) [(999, 'Z')] (mat @(NN 32) ['a' ..])
          @?= mat2' @3 @2 [[(0, 'a'), (1, 'b'), (2, 'c'), (3, 'd'), (4, 'e'), (5, 'f'), (999, 'Z')], [(1, 'b'), (2, 'c'), (3, 'd'), (4, 'e'), (5, 'f'), (999, 'Z')], [(2, 'c'), (3, 'd'), (4, 'e'), (5, 'f'), (999, 'Z')], [(3, 'd'), (4, 'e'), (5, 'f'), (999, 'Z')], [(4, 'e'), (5, 'f'), (999, 'Z')], [(5, 'f'), (999, 'Z')]]
    , testCase "ipostscanl" $
        ipostscanl (\i zs a -> (fmPos i, a) : zs) [] (mat @(NN 32) ['a' ..])
          @?= mat2' @3 @2 [[(0, 'a')], [(1, 'b'), (0, 'a')], [(2, 'c'), (1, 'b'), (0, 'a')], [(3, 'd'), (2, 'c'), (1, 'b'), (0, 'a')], [(4, 'e'), (3, 'd'), (2, 'c'), (1, 'b'), (0, 'a')], [(5, 'f'), (4, 'e'), (3, 'd'), (2, 'c'), (1, 'b'), (0, 'a')]]
    , testCase "ipostscanl" $
        ipostscanl (\i zs a -> (fmPos i, a) : zs) [(999, 'Z')] (mat @(NN 32) ['a' ..])
          @?= mat2' @3 @2 [[(0, 'a'), (999, 'Z')], [(1, 'b'), (0, 'a'), (999, 'Z')], [(2, 'c'), (1, 'b'), (0, 'a'), (999, 'Z')], [(3, 'd'), (2, 'c'), (1, 'b'), (0, 'a'), (999, 'Z')], [(4, 'e'), (3, 'd'), (2, 'c'), (1, 'b'), (0, 'a'), (999, 'Z')], [(5, 'f'), (4, 'e'), (3, 'd'), (2, 'c'), (1, 'b'), (0, 'a'), (999, 'Z')]]
    , testCase "postscanlMat" $
        postscanlMat (flip (:)) [] (vec @6 ['a' ..])
          @?= vec' @6 ["a", "ba", "cba", "dcba", "edcba", "fedcba"]
    , testCase "postscanrMat" $
        postscanrMat (:) [] (vec @6 ['a' ..])
          @?= vec' @6 ["abcdef", "bcdef", "cdef", "def", "ef", "f"]
    , testCase "postscanlMat" $
        postscanlMat (flip (:)) [] (mat @(NN 222) ['a' ..])
          @?= mat' @'[2, 2, 2] ["a", "ba", "cba", "dcba", "edcba", "fedcba", "gfedcba", "hgfedcba"]
    , testCase "scanlVec" $
        scanlVec (flip (:)) [] (vec @6 ['a' ..])
          @?= vec' @7 ["", "a", "ba", "cba", "dcba", "edcba", "fedcba"]
    , testCase "scanrVec" $
        scanrVec (:) ['Z'] (vec @6 ['a' ..])
          @?= vec' @7 ["abcdefZ", "bcdefZ", "cdefZ", "defZ", "efZ", "fZ", "Z"]
    , testCase "unfoldlRep" $
        unfoldlRep @(Vec 5) (\i s -> (drop 1 s, (fmPos i, head s))) ['a' .. 'h']
          @?= ("fgh", vec' @5 [(0, 'a'), (1, 'b'), (2, 'c'), (3, 'd'), (4, 'e')])
    , testCase "unfoldrRep" $
        unfoldrRep @(Vec 5) (\i s -> (drop 1 s, (fmPos i, head s))) ['a' .. 'h']
          @?= ("fgh", vec' @5 [(0, 'e'), (1, 'd'), (2, 'c'), (3, 'b'), (4, 'a')])
    , testCase "fillTraversable" $
        fillTraversable @(MatN 234) @Int (pure ()) [1.. 40]
          @?= Right ([25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40], mat' @'[2, 3, 4] [1 .. 24])
    , testCase "toInteger1" $
        toInteger1 (pure @(Mat2 4 2) EQ)
          @?= 3280
    , testCase "toInteger1" $
        toInteger1 (pure @(Mat2 4 2) LT)
          @?= 0
    , testCase "toInteger1" $
        toInteger1 (pure @(Mat2 4 2) GT)
          @?= 6560
    , testCase "toInteger1" $
        fmap toInteger1 (withOp (+ 123) (pure @(Mat2 4 2) EQ))
          @?= Right 3403
    , testCase "toInteger1" $
        fmap toInteger1 (withOp (+ 1) (pure @(Mat2 4 2) GT))
          @?= Left "cap=(0,6560):padL: negative fill: would need to truncate the data"
    , testCase "toInteger1" $
        fmap toInteger1 (withOp (subtract 1) (pure @(Mat2 4 2) LT))
          @?= Left "calcNextEnum:not defined for negative numbers"
    , testCase "toInteger1" $
        fmap toInteger1 (withOp2 ((+) . (+ 1)) (pure @(Mat2 4 2) EQ) minBound)
          @?= Right 3281
    , testCase "mempty" $
        (mempty :: Vec 10 Ordering)
          @?= vec' @10 [EQ, EQ, EQ, EQ, EQ, EQ, EQ, EQ, EQ, EQ]
    , testCase "minBound" $
        (minBound :: Vec 10 Ordering)
          @?= vec' @10 [LT, LT, LT, LT, LT, LT, LT, LT, LT, LT]
    , testCase "maxBound" $
        (maxBound :: Vec 10 Ordering)
          @?= vec' @10 [GT, GT, GT, GT, GT, GT, GT, GT, GT, GT]
    , testCase "fromInteger1" $
        fromInteger1 (minBound @(Mat '[2, 5] Ordering)) 0
          @?= Right (mat' @'[2, 5] [LT, LT, LT, LT, LT, LT, LT, LT, LT, LT])
    , testCase "fromInteger1" $
        fromInteger1 (minBound @(Mat '[2, 5] Ordering)) (-5)
          @?= Left "calcNextEnum:not defined for negative numbers"
    , testCase "fromInteger1" $
        fromInteger1 (minBound @(Mat '[2, 5] Int8)) 0
          @?= Right (mat' @'[2, 5] [0, 0, 0, 0, 0, 0, 0, 0, 0, 0])
    , testCase "fromInteger1" $
        fromInteger1 (minBound @(Mat '[2, 5] Int8)) 128
          @?= Right (mat' @'[2, 5] [0, 0, 0, 0, 0, 0, 0, 0, 1, 0])
    , testCase "fromInteger1" $
        fromInteger1 (minBound @(Mat '[2, 5] Int8)) (-129)
          @?= Right (mat' @'[2, 5] [0, 0, 0, 0, 0, 0, 0, 0, -1, 0])
    , testCase "fromInteger1" $
        fromInteger1 (minBound @(Mat '[2, 5] Ordering)) 23
          @?= Right (mat' @'[2, 5] [LT, LT, LT, LT, LT, LT, LT, GT, EQ, GT])
    , testCase "fromInteger1" $
        fromInteger1 (minBound @(Mat '[2, 5] Ordering)) 59049
          @?= Left "cap=(0,59048):padL: negative fill: would need to truncate the data"
    , testCase "fromInteger1" $
        fromInteger1 (minBound @(Mat '[2, 5] Ordering)) 59048
          @?= Right (mat' @'[2, 5] [GT, GT, GT, GT, GT, GT, GT, GT, GT, GT])
    , testCase "fromInteger1" $
        fromInteger1 (minBound @(Mat '[2, 5] Int8)) 99999999999999
          @?= Right (mat' @'[2, 5] [0, 0, 0, 22, 94, 49, 3, 104, 127, 127])
    , testCase "fromInteger1" $
        fromInteger1 (minBound @(Mat '[2, 5] Ordering)) (-1)
          @?= Left "calcNextEnum:not defined for negative numbers"
    , testCase "toInteger1" $
        toInteger1 (mat' @'[2, 5] [LT, LT, LT, LT, LT, LT, LT, LT, LT, LT])
          @?= 0
    , testCase "fromInteger1" $
        fromInteger1 (minBound @(Mat '[2, 5] Int8)) 99999999999999999999
          @?= Right (mat2' @2 @5 [10, 107, 99, 87, 69, 86, 24, 63, 127, 127])
    , testCase "fromInteger1" $
        fromInteger1 (minBound @(Mat '[2, 5] Int8)) 99999999999999999999999
          @?= Left "cap=(-1276136419117121619200,1180591620717411303423):padL: negative fill: would need to truncate the data"
    , testCase "fromInteger1" $
        fromInteger1 (minBound @(Mat '[2, 5] Int8)) 1180591620717411303423
          @?= Right (mat2' @2 @5 [127, 127, 127, 127, 127, 127, 127, 127, 127, 127])
    , testCase "fromInteger1" $
        fromInteger1 (minBound @(Mat '[2, 5] Int8)) 1180591620717411303424
          @?= Left "cap=(-1276136419117121619200,1180591620717411303423):padL: negative fill: would need to truncate the data"
    , testCase "fromInteger1" $
        fromInteger1 (minBound @(Mat '[2, 5] Int8)) (-1276136419117121619200)
          @?= Right (mat2' @2 @5 [-128, -128, -128, -128, -128, -128, -128, -128, -128, -128])
    , testCase "fromInteger1" $
        fromInteger1 (minBound @(Mat '[2, 5] Int8)) (-1276136419117121619201)
          @?= Left "cap=(-1276136419117121619200,1180591620717411303423):padL: negative fill: would need to truncate the data"
    , testCase "lenses mixed" $
        mm @(NN 1234) ^. _r1 . _r1 . _c2 . snocMat . _1 . consMat
          @?= (2, vec @1 [6])
    , testCase "lenses mixed" $
        mm @(NN 734) ^. _c3 . snocMat . _1 . _c4 . _r5
          @?= 60
    , testCase "withN" $
        let s = withN 5 $ \(_ :: x n) ->
              withN 3 $ \(_ :: x m) ->
                withN 2 $ \(_ :: x p) ->
                  let w1 :: Mat2 n m Int -- if you use let statements then must have signatures else fails with constraints not satisfied
                      w1 = mat2 @n @m [1 ..]
                      w2 :: Mat2 m p Int
                      w2 = mat2 @m @p [10 ..]
                      z = multMat w1 w2
                   in show (fromNSP @'[n, m, p], z)
         in read @(NonEmpty Pos, Mat2 5 2 Int) s
              @?= (_5P :| [_3P, _2P], mat2 @5 @2 [76, 82, 184, 199, 292, 316, 400, 433, 508, 550])
    , testCase "withN" $
        let s = withN 5 $ \(_ :: x n) ->
              withN 3 $ \(_ :: x m) ->
                show (mat2 @n @m @Int [1 ..] `multMat` mat2 @m @n @Int [10 ..])
         in read @(Mat2 5 5 Int) s @?= mat2' @5 @5 [100, 106, 112, 118, 124, 235, 250, 265, 280, 295, 370, 394, 418, 442, 466, 505, 538, 571, 604, 637, 640, 682, 724, 766, 808]
    , testCase "withN" $
        let s = withN 5 $ \(_ :: x n) -> show (vec @n @Int [1 ..])
         in read @(Vec 5 Int) s @?= vec' @5 [1 .. 5]
    , testCase "withN" $
        let s = withN 5 $ \(_ :: x n) ->
              withN 7 $ \(_ :: x m) -> show (mat2 @n @m @Int [1 ..], mat2 @m @n ['a' ..])
         in read @(Mat2 5 7 Int, Mat2 7 5 Char) s @?= (mat2' @5 @7 [1 .. 35], mat2 @7 @5 ['a' ..])
    , testCase "withN2" $
        let z = withN2 4 5 $ \(_ :: p n) (_ :: p m) -> show (mat2 @n @m ['a' ..])
         in read @(Mat2 4 5 Char) z @?= mat2' @4 @5 ['a' .. 't']
    , testCase "withN3" $
        withN3 2 3 4 (\(_ :: z n) (_ :: z m) (_ :: z q) -> fromNSP @'[n, m, q])
          @?= _2P :| [_3P, _4P]
    , testCase "withN3" $
        let z = withN 4 $ \(_ :: p n) -> show (mat2 @2 @n @Int [1..] ^. _row @1)
         in read @(Vec 4 Int) z @?= vec' @4 [1 .. 4]
    , testCase "withN3" $
        let z = withN 4 $ \(_ :: p n) -> show (mat2 @2 @n @Int [1 ..] ^. _row @2)
         in read @(Vec 4 Int) z @?= vec' @4 [5 .. 8]

    , testCase "withNMin3" $
         let z = withNMin3 5 (\(_ :: p n) -> withNMin3 4 (\(_ :: p m) -> show (mat2 @n @m [1::Int ..] ^. _c2)))
         in read @(Vec 5 Int) z @?= vec' @5 [2,6,10,14,18]

    , testCase "rotateLeft rotateRight" $
        let z = mm @(NN 57)
        in rotateLeft (rotateRight z) @?= z
    , testCase "rotateLeft" $
        rotateLeft (mm @(NN 35))
          @?= mat2' @5 @3 [5,10,15,4,9,14,3,8,13,2,7,12,1,6,11]
      ,testCase "rotateRight" $
        rotateRight (mm @(NN 35))
          @?= mat2' @5 @3 [11,6,1,12,7,2,13,8,3,14,9,4,15,10,5]
    , testCase "rotateRight transpose rotateRight" $
        let z = mm @(NN 57)
        in (transposeMat . rotateLeft . transposeMat) z  @?= rotateRight z

    , testCase "determinant" $
        determinant (mat2' @3 @3 @Int [2, -3, 1, 2, 0, -1, 1, 4, 5])
          @?= 49
    , testCase "determinant" $
        determinant (mat2' @3 @3 @Int [1, 3, 2, -3, -1, -3, 2, 3, 1])
          @?= (-15)
    , testCase "determinant" $
        determinant (mat2' @4 @4 @Int [3, 2, 0, 1, 4, 0, 1, 2, 3, 0, 2, 1, 9, 2, 3, 1])
          @?= 24
    , testCase "determinant" $
        determinant (mat2' @4 @4 @Int [1, 0, 2, -1, 3, 0, 0, 5, 2, 1, 4, -3, 1, 0, 5, 0])
          @?= 30
    , testCase "determinant" $
        determinant (mat2' @4 @4 @Int [1, 0, 4, -6, 2, 5, 0, 3, -1, 2, 3, 5, 2, 1, -2, 3])
          @?= 318
    , testCase "determinant" $
        determinant (mat2' @2 @2 @Int [1, 2, 3, 4])
          @?= (-2)

    , testCase "determinant" $
        determinant (mat2' @1 @1 @Int [-5])
          @?= (-5)
    , testCase "deleteColumnL" $
        deleteColumnL _2P 1 [1,2::Int] @?= [1]
    , testCase "deleteColumnL" $
        deleteColumnL _2P 0 [1,2::Int] @?= [2]
    , testCase "deleteColumnL" $
        deleteColumnL _3P 1 [1::Int .. 12] @?= [1,3,4,6,7,9,10,12]
    , testCase "deleteColumnL" $
        deleteColumnL _2P 1 [1..10::Int] @?=  [1,3,5,7,9]
    , testCase "deleteColumnL" $
        deleteColumnL _1P 0 [1::Int] @?= []
    ]

suiteCheckers :: TestTree
suiteCheckers =
  testGroup
    "TestMat Checkers"
    [ adj' False 10 500 10 $ TQ.testProperties "mat [2,3,4]" (checkersToProps (testLawsMat @'[2, 3, 4]))
    , adj' False 10 500 10 $ TQ.testProperties "mat [5]" (checkersToProps (testLawsMat' @'[5]))
    , adj' False 10 500 10 $ TQ.testProperties "mat [1]" (checkersToProps (testLawsMat' @'[1]))
    , adj' False 10 500 10 $ TQ.testProperties "mat [1,5]" (checkersToProps (testLawsMat' @'[1, 5]))
    ]

fmi237' :: NonEmpty (FinMat '[2, 3, 7])
fmi237' = frp $ traverse (nonEmptyToFinMat <=< toPositives) fmi237

fmi237 :: NonEmpty (NonEmpty Int)
fmi237 = fmap N.fromList ([1, 1, 1] :| [[1, 1, 2], [1, 1, 3], [1, 1, 4], [1, 1, 5], [1, 1, 6], [1, 1, 7], [1, 2, 1], [1, 2, 2], [1, 2, 3], [1, 2, 4], [1, 2, 5], [1, 2, 6], [1, 2, 7], [1, 3, 1], [1, 3, 2], [1, 3, 3], [1, 3, 4], [1, 3, 5], [1, 3, 6], [1, 3, 7], [2, 1, 1], [2, 1, 2], [2, 1, 3], [2, 1, 4], [2, 1, 5], [2, 1, 6], [2, 1, 7], [2, 2, 1], [2, 2, 2], [2, 2, 3], [2, 2, 4], [2, 2, 5], [2, 2, 6], [2, 2, 7], [2, 3, 1], [2, 3, 2], [2, 3, 3], [2, 3, 4], [2, 3, 5], [2, 3, 6], [2, 3, 7]])

-- ghc 9.2 needs explicit kinds for "i" and "n"
overrideDictPositive :: forall (i :: Nat) (n :: Nat) p . p n -> (i GN.<=? n) :~: 'True
overrideDictPositive _ = unsafeCoerce Refl

-- | lift a positive number to the typelevel
withN :: Int -> (forall n. FinC 1 n => Proxy n -> x) -> x
withN i f
  | i <= 1 = normalError $ "withN: index must be at least 1:found " ++ show i
  | otherwise =
      case GN.someNatVal (toEnum i) of
        GN.SomeNat (pn :: Proxy n) ->
          case overrideDictPositive @1 pn of
            Refl -> f (Proxy @n)

-- | lift a positive number to the typelevel
withNMin2 :: Int -> (forall n . (FinC 2 n, FinC 1 n) => Proxy n -> x) -> x
withNMin2 i f
  | i < 2 = normalError $ "withNMin2: index must be at least 2:found " ++ show i
  | otherwise =
      case GN.someNatVal (toEnum i) of
        GN.SomeNat (pn :: Proxy n) ->
          case overrideDictPositive @1 pn of
            Refl ->
              case overrideDictPositive @2 pn of
                 Refl -> f (Proxy @n)

withNMin3 :: Int -> (forall n . (FinC 3 n, FinC 2 n, FinC 1 n) => Proxy n -> x) -> x
withNMin3 i f
  | i < 3 = normalError $ "withNMin3: index must be at least 3:found " ++ show i
  | otherwise =
      case GN.someNatVal (toEnum i) of
        GN.SomeNat (pn :: Proxy n) ->
          case overrideDictPositive @1 pn of
            Refl ->
              case overrideDictPositive @2 pn of
                Refl ->
                  case overrideDictPositive @3 pn of
                    Refl -> f (Proxy @n)

-- | lift two positive numbers to the typelevel
withN2 :: Int -> Int -> (forall n m. (FinC 1 n, FinC 1 m) => Proxy n -> Proxy m -> x) -> x
withN2 i j f = withN i $ \p1 -> withN j $ \p2 -> f p1 p2

-- | lift three positive numbers to the typelevel
withN3 :: Int -> Int -> Int -> (forall n m p. (FinC 1 n, FinC 1 m, FinC 1 p) => Proxy n -> Proxy m -> Proxy p -> x) -> x
withN3 i j k f = withN i $ \p1 -> withN j $ \p2 -> withN k $ \p3 -> f p1 p2 p3

-- | lift four positive numbers to the typelevel
withN4 :: Int -> Int -> Int -> Int -> (forall n m p q. (FinC 1 n, FinC 1 m, FinC 1 p, FinC 1 q) => Proxy n -> Proxy m -> Proxy p -> Proxy q -> x) -> x
withN4 i j k l f = withN i $ \p1 -> withN j $ \p2 -> withN k $ \p3 -> withN l $ \p4 -> f p1 p2 p3 p4

