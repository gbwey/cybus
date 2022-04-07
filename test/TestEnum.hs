{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module TestEnum where

import Control.Arrow
import Cybus.Fin
import Cybus.FinMat
import Cybus.Mat
import Cybus.NatHelper
import Data.Foldable
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as N
import Data.Pos
import Primus.AsMaybe
import Primus.Enum
import Primus.Rep
import Test.Tasty
import Test.Tasty.HUnit

suite :: TestTree
suite =
  testGroup
    "TestEnum"
    [ testCase "toEnum" $
        toEnum @(Mat2 3 4 ()) 0
          @?= mat' @'[3, 4] (replicate 12 ())
    , testCase "toEnum" $
        toEnum @(Mat '[2, 3] ()) 0
          @?= mat' @'[2, 3] [(), (), (), (), (), ()]
    , testCase "toEnum" $
        left (const ()) (toEnumRep @(Mat '[2, 3]) @() 2)
          @?= Left ()
    , testCase "toenum" $
        toEnum @(Fin 10) 0
          @?= FinU _1P _10P
    , testCase "toenum" $
        toEnum @(Fin 10) 1
          @?= FinU _2P _10P
    , testCase "toenum" $
        toEnum @(Fin 10) 2
          @?= FinU _3P _10P
    , testCase "toenum" $
        toEnum @(Fin 2) 1
          @?= FinU _2P _2P
    , testCase "min" $
        minBound @(Fin 5)
          @?= FinU _1P _5P
    , testCase "max" $
        maxBound @(Fin 5)
          @?= FinU _5P _5P
    , testCase "toEnumList" $
        toEnumList @(FinMat '[3, 4]) (-2)
          @?= Left "calcNextEnum:not defined for negative numbers"
    , testCase "toEnumList" $
        toEnumList @(FinMat '[3, 4]) (-1)
          @?= Left "calcNextEnum:not defined for negative numbers"
    , testCase "toEnumList" $
        toEnumList @(FinMat '[3, 4]) 0
          @?= Right []
    , testCase "toEnumList" $
        toEnumList @(FinMat '[3, 4]) 1
          @?= Right [FinMatU 1 (_3P :| [_4P])]
    , testCase "toEnumList" $
        toEnumList @(FinMat '[3, 4]) 4
          @?= Right [FinMatU 4 (_3P :| [_4P])]
    , testCase "toEnumList" $
        toEnumList @(FinMat '[3, 4]) 5
          @?= Right [FinMatU 5 (_3P :| [_4P])]
    , testCase "toEnumList" $
        toEnumList @(FinMat '[1, 1, 1, 1]) (-2)
          @?= Left "calcNextEnum:not defined for negative numbers"
    , testCase "toEnumList" $
        toEnumList @(FinMat '[1, 1, 1, 1]) 0
          @?= Right []
    , testCase "toEnumList" $
        toEnumList @(FinMat '[1, 1, 1, 1]) 2
          @?= Left "calcNextEnum:not defined for positive numbers"
    , testCase "toEnumList" $
        toEnumList @(Fin 1) (-1)
          @?= Left "calcNextEnum:not defined for negative numbers"
    , testCase "toEnumList" $
        toEnumList @(Fin 1) 0
          @?= Right []
    , testCase "toEnumList" $
        toEnumList @(Fin 1) 1
          @?= Left "calcNextEnum:not defined for positive numbers"
    , testCase "toEnumList" $
        toEnumList @(Fin 2) (-10)
          @?= Left "calcNextEnum:not defined for negative numbers"
    , testCase "toEnumList" $
        toEnumList @(Fin 2) 0
          @?= Right []
    , testCase "toEnumList" $
        toEnumList @(Fin 2) 1
          @?= Right [FinU _2P _2P :: Fin 2]
    , testCase "toEnumList" $
        toEnumList @(Fin 2) 12
          @?= Right [FinU _2P _2P, FinU _2P _2P, FinU _1P _2P, FinU _1P _2P :: Fin 2]
    , testCase "toEnumList" $
        toEnumList @(Fin 2) 100
          @?= Right [FinU _2P _2P, FinU _2P _2P, FinU _1P _2P, FinU _1P _2P, FinU _2P _2P, FinU _1P _2P, FinU _1P _2P :: Fin 2]
    , testCase "universe1 fin" $
        universe1 @(Fin 5)
          @?= FinU @5 _1P _5P :| [FinU @5 _2P _5P, FinU @5 _3P _5P, FinU @5 _4P _5P, FinU @5 _5P _5P]
    , testCase "universe1 nextfin" $
        iterateT1 succSafe (FinU _1P _5P)
          @?= FinU @5 _1P _5P :| [FinU @5 _2P _5P, FinU @5 _3P _5P, FinU @5 _4P _5P, FinU @5 _5P _5P]
    , testCase "universe1 prevfin" $
        let f5 = FinU @5 _1P _5P :| [FinU @5 _2P _5P, FinU @5 _3P _5P, FinU @5 _4P _5P, FinU @5 _5P _5P]
         in iterateT1 predSafe (N.last f5)
              @?= N.reverse f5
    , testCase "universe1 prevfin" $
        iterateT1 predSafe (FinU @5 _1P _5P)
          @?= FinU _1P _5P :| []
    , testCase "toEnumRep" $
        toEnumRep @(Mat '[4]) @Ordering 10
          @?= Right (mat' @'[4] [LT, EQ, LT, EQ])
    , testCase "toEnumRep" $
        toEnumRep @(Mat '[4]) @Ordering 0
          @?= Right (mat' @'[4] [LT, LT, LT, LT])
    , testCase "toEnumList" $
        toEnumList @(Vec 3 Ordering) 0
          @?= Right []
    , testCase "toEnumList" $
        toEnumList @(Vec 3 Ordering) 1
          @?= Right [mat' @'[3] [LT, LT, EQ]]
    , testCase "toEnumList" $
        toEnumList @(Vec 3 Ordering) 200
          @?= Right [mat' @'[3] [LT, GT, EQ], mat' @'[3] [EQ, LT, GT]]
    , testCase "toEnumList1" $
        toEnumList1 @(Vec 3 Ordering) 0
          @?= Right (mat' @'[3] [LT, LT, LT] :| [])
    , testCase "toEnumList1" $
        toEnumList1 @(Vec 3 Ordering) 1
          @?= Right (mat' @'[3] [LT, LT, EQ] :| [])
    , testCase "toEnumList1" $
        toEnumList1 @(Vec 3 Ordering) 26
          @?= Right (mat' @'[3] [GT, GT, GT] :| [])
    , testCase "toEnumList1" $
        toEnumList1 @(Vec 3 Ordering) 27
          @?= Right (mat' @'[3] [LT, LT, EQ] :| [mat' @'[3] [LT, LT, LT]])
    , testCase "toEnumList1" $
        toEnumList1 @(Vec 3 Ordering) 200
          @?= Right (mat' @'[3] [LT, GT, EQ] :| [mat' @'[3] [EQ, LT, GT]])
    , testCase "succTraversable" $
        universe1 @(Vec 3 Ordering)
          @?= iterateT1 succSafe minBound
    , testCase "toEnumList" $
        toEnumList @(Vec 3 ()) 1
          @?= Left "calcNextEnum:not defined for positive numbers"
    , testCase "toEnumList" $
        toEnumList @(Vec 3 ()) 0
          @?= Right []
    , testCase "toEnumList1" $
        toEnumList1 @(Vec 3 ()) 0
          @?= Right ((() .: () .| ()) :| [])
    , testCase "toEnumList" $
        toEnumList @(Vec 3 Bool) 20
          @?= Right [False .: True .| False, True .: False .| False]
    , testCase "toEnumList1" $
        toEnumList1 @(Vec 3 Bool) 20
          @?= Right ((False .: True .| False) :| [True .: False .| False])
    , testCase "toEnumTraversable" $
        toEnumTraversable @Ordering (pure @(Mat '[6]) ()) 10
          @?= Right (LT .: LT .: LT .: EQ .: LT .| EQ)
    , testCase "toEnumRep" $
        toEnumRep @(Mat '[6]) @Ordering 10
          @?= Right (LT .: LT .: LT .: EQ .: LT .| EQ)
    , testCase "universe1" $
        universe1 @(FinMat (NN 123))
          @?= let ns = _1P :| [_2P, _3P]
               in FinMatU 0 ns :| [FinMatU 1 ns, FinMatU 2 ns, FinMatU 3 ns, FinMatU 4 ns, FinMatU 5 ns]
    , testCase "universeTraversable" $
        universeTraversable (pure @(Vec 2) GT)
          @?= Right ((LT .| LT) :| [LT .| EQ, LT .| GT, EQ .| LT, EQ .| EQ, EQ .| GT, GT .| LT, GT .| EQ, GT .| GT])
    , testCase "universeTraversable" $
        universeTraversable (replicate 2 (minBound @(Fin 5)))
          @?= let ff p n = FinU @5 p n
               in Right ([ff _1P _5P, ff _1P _5P] :| [[ff _1P _5P, ff _2P _5P], [ff _1P _5P, ff _3P _5P], [ff _1P _5P, ff _4P _5P], [ff _1P _5P, ff _5P _5P], [ff _2P _5P, ff _1P _5P], [ff _2P _5P, ff _2P _5P], [ff _2P _5P, ff _3P _5P], [ff _2P _5P, ff _4P _5P], [ff _2P _5P, ff _5P _5P], [ff _3P _5P, ff _1P _5P], [ff _3P _5P, ff _2P _5P], [ff _3P _5P, ff _3P _5P], [ff _3P _5P, ff _4P _5P], [ff _3P _5P, ff _5P _5P], [ff _4P _5P, ff _1P _5P], [ff _4P _5P, ff _2P _5P], [ff _4P _5P, ff _3P _5P], [ff _4P _5P, ff _4P _5P], [ff _4P _5P, ff _5P _5P], [ff _5P _5P, ff _1P _5P], [ff _5P _5P, ff _2P _5P], [ff _5P _5P, ff _3P _5P], [ff _5P _5P, ff _4P _5P], [ff _5P _5P, ff _5P _5P]])
    , testCase "universeTraversable" $
        universeTraversable (vec @2 (repeat (finMatC @'[1, 1] @'[2, 3])))
          @?= let ff i = FinMatU i (_2P :| [_3P])
               in Right ((ff 0 .| ff 0) :| [ff 0 .| ff 1, ff 0 .| ff 2, ff 0 .| ff 3, ff 0 .| ff 4, ff 0 .| ff 5, ff 1 .| ff 0, ff 1 .| ff 1, ff 1 .| ff 2, ff 1 .| ff 3, ff 1 .| ff 4, ff 1 .| ff 5, ff 2 .| ff 0, ff 2 .| ff 1, ff 2 .| ff 2, ff 2 .| ff 3, ff 2 .| ff 4, ff 2 .| ff 5, ff 3 .| ff 0, ff 3 .| ff 1, ff 3 .| ff 2, ff 3 .| ff 3, ff 3 .| ff 4, ff 3 .| ff 5, ff 4 .| ff 0, ff 4 .| ff 1, ff 4 .| ff 2, ff 4 .| ff 3, ff 4 .| ff 4, ff 4 .| ff 5, ff 5 .| ff 0, ff 5 .| ff 1, ff 5 .| ff 2, ff 5 .| ff 3, ff 5 .| ff 4, ff 5 .| ff 5])
    , testCase "capacity" $
        capacity @(FinMat '[2, 3]) (replicate 2 ())
          @?= Right (0, 35)
    , testCase "capacity" $
        capacity @(FinMat '[1, 3, 5, 6]) (replicate 7 ())
          @?= Right (0, 47829689999999)
    , testCase "iterateT1 succTraversable FinMat" $
        fmap (toList . fmap fmPos) (iterateT1 succTraversable (vec' @2 [finMatC @'[3, 3] @'[4, 3], finMatC @'[2, 1] @'[4, 3]]))
          @?= [8, 3] :| [[8, 4], [8, 5], [8, 6], [8, 7], [8, 8], [8, 9], [8, 10], [8, 11], [9, 0], [9, 1], [9, 2], [9, 3], [9, 4], [9, 5], [9, 6], [9, 7], [9, 8], [9, 9], [9, 10], [9, 11], [10, 0], [10, 1], [10, 2], [10, 3], [10, 4], [10, 5], [10, 6], [10, 7], [10, 8], [10, 9], [10, 10], [10, 11], [11, 0], [11, 1], [11, 2], [11, 3], [11, 4], [11, 5], [11, 6], [11, 7], [11, 8], [11, 9], [11, 10], [11, 11]]
    , testCase "iterateT1 succTraversable FinMat" $
        fmap (toList . fmap fmPos) (iterateT1 succTraversable [finMatC @'[3, 3] @'[3, 3], finMatC @'[2, 1], finMatC @'[3, 1]])
          @?= [8, 3, 6] :| [[8, 3, 7], [8, 3, 8], [8, 4, 0], [8, 4, 1], [8, 4, 2], [8, 4, 3], [8, 4, 4], [8, 4, 5], [8, 4, 6], [8, 4, 7], [8, 4, 8], [8, 5, 0], [8, 5, 1], [8, 5, 2], [8, 5, 3], [8, 5, 4], [8, 5, 5], [8, 5, 6], [8, 5, 7], [8, 5, 8], [8, 6, 0], [8, 6, 1], [8, 6, 2], [8, 6, 3], [8, 6, 4], [8, 6, 5], [8, 6, 6], [8, 6, 7], [8, 6, 8], [8, 7, 0], [8, 7, 1], [8, 7, 2], [8, 7, 3], [8, 7, 4], [8, 7, 5], [8, 7, 6], [8, 7, 7], [8, 7, 8], [8, 8, 0], [8, 8, 1], [8, 8, 2], [8, 8, 3], [8, 8, 4], [8, 8, 5], [8, 8, 6], [8, 8, 7], [8, 8, 8]]
    , testCase "iterateT1 succTraversable Fin" $
        fmap (toList . fmap fnPos) (iterateT1 succTraversable [finC @4 @5, finC @3, finC @2])
          @?= [_4P, _3P, _2P] :| [[_4P, _3P, _3P], [_4P, _3P, _4P], [_4P, _3P, _5P], [_4P, _4P, _1P], [_4P, _4P, _2P], [_4P, _4P, _3P], [_4P, _4P, _4P], [_4P, _4P, _5P], [_4P, _5P, _1P], [_4P, _5P, _2P], [_4P, _5P, _3P], [_4P, _5P, _4P], [_4P, _5P, _5P], [_5P, _1P, _1P], [_5P, _1P, _2P], [_5P, _1P, _3P], [_5P, _1P, _4P], [_5P, _1P, _5P], [_5P, _2P, _1P], [_5P, _2P, _2P], [_5P, _2P, _3P], [_5P, _2P, _4P], [_5P, _2P, _5P], [_5P, _3P, _1P], [_5P, _3P, _2P], [_5P, _3P, _3P], [_5P, _3P, _4P], [_5P, _3P, _5P], [_5P, _4P, _1P], [_5P, _4P, _2P], [_5P, _4P, _3P], [_5P, _4P, _4P], [_5P, _4P, _5P], [_5P, _5P, _1P], [_5P, _5P, _2P], [_5P, _5P, _3P], [_5P, _5P, _4P], [_5P, _5P, _5P]]
    , testCase "iterateT1 predTraversable Fin" $
        fmap (toList . fmap fnPos) (iterateT1 predTraversable [finC @4 @5, finC @3, finC @2])
          @?= [_4P, _3P, _2P] :| [[_4P, _3P, _1P], [_4P, _2P, _5P], [_4P, _2P, _4P], [_4P, _2P, _3P], [_4P, _2P, _2P], [_4P, _2P, _1P], [_4P, _1P, _5P], [_4P, _1P, _4P], [_4P, _1P, _3P], [_4P, _1P, _2P], [_4P, _1P, _1P], [_3P, _5P, _5P], [_3P, _5P, _4P], [_3P, _5P, _3P], [_3P, _5P, _2P], [_3P, _5P, _1P], [_3P, _4P, _5P], [_3P, _4P, _4P], [_3P, _4P, _3P], [_3P, _4P, _2P], [_3P, _4P, _1P], [_3P, _3P, _5P], [_3P, _3P, _4P], [_3P, _3P, _3P], [_3P, _3P, _2P], [_3P, _3P, _1P], [_3P, _2P, _5P], [_3P, _2P, _4P], [_3P, _2P, _3P], [_3P, _2P, _2P], [_3P, _2P, _1P], [_3P, _1P, _5P], [_3P, _1P, _4P], [_3P, _1P, _3P], [_3P, _1P, _2P], [_3P, _1P, _1P], [_2P, _5P, _5P], [_2P, _5P, _4P], [_2P, _5P, _3P], [_2P, _5P, _2P], [_2P, _5P, _1P], [_2P, _4P, _5P], [_2P, _4P, _4P], [_2P, _4P, _3P], [_2P, _4P, _2P], [_2P, _4P, _1P], [_2P, _3P, _5P], [_2P, _3P, _4P], [_2P, _3P, _3P], [_2P, _3P, _2P], [_2P, _3P, _1P], [_2P, _2P, _5P], [_2P, _2P, _4P], [_2P, _2P, _3P], [_2P, _2P, _2P], [_2P, _2P, _1P], [_2P, _1P, _5P], [_2P, _1P, _4P], [_2P, _1P, _3P], [_2P, _1P, _2P], [_2P, _1P, _1P], [_1P, _5P, _5P], [_1P, _5P, _4P], [_1P, _5P, _3P], [_1P, _5P, _2P], [_1P, _5P, _1P], [_1P, _4P, _5P], [_1P, _4P, _4P], [_1P, _4P, _3P], [_1P, _4P, _2P], [_1P, _4P, _1P], [_1P, _3P, _5P], [_1P, _3P, _4P], [_1P, _3P, _3P], [_1P, _3P, _2P], [_1P, _3P, _1P], [_1P, _2P, _5P], [_1P, _2P, _4P], [_1P, _2P, _3P], [_1P, _2P, _2P], [_1P, _2P, _1P], [_1P, _1P, _5P], [_1P, _1P, _4P], [_1P, _1P, _3P], [_1P, _1P, _2P], [_1P, _1P, _1P]]
    ]
