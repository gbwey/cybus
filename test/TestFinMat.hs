{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module TestFinMat where

import Control.Lens
import Control.Monad
import Cybus.Fin
import Cybus.FinMat
import Cybus.NatHelper
import Data.Either
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as N
import Data.Pos
import Primus.AsMaybe
import Primus.Enum
import Primus.Error
import Primus.NonEmpty
import Primus.Num1
import Test.Tasty
import Test.Tasty.HUnit

doit :: IO ()
doit = defaultMain suite

suite :: TestTree
suite =
  testGroup
    "TestFinMat"
    [ testCase "succSafe universe" $
        universe1 @(FinMat '[2, 3, 4])
          @?= iterateT1 succSafe minBound
    , testCase "predSafe universe" $
        universe1 @(FinMat '[2, 3, 4])
          @?= N.reverse (iterateT1 predSafe maxBound)
    , testCase "next finMat" $
        succSafe (maxBound :: FinMat '[3, 4, 5, 3])
          @?= Nothing
    , testCase "prev finMat" $
        predSafe (minBound :: FinMat '[3, 4, 5, 3])
          @?= Nothing
    , testCase "universe enums" $
        universe1 @(FinMat '[2, 3, 4])
          @?= iterateT1 succSafe minBound
    , testCase "prev FinMat universe" $
        universe1 @(FinMat '[2, 3, 4])
          @?= N.reverse (iterateT1 predSafe maxBound)
    , testCase "minBound" $
        (minBound :: FinMat '[3, 4, 5, 1])
          @?= FinMatU 0 (_3P :| [_4P, _5P, _1P])
    , testCase "maxBound" $
        (maxBound :: FinMat '[3, 4, 5, 1])
          @?= FinMatU 59 (_3P :| [_4P, _5P, _1P])
    , testCase "maxBound" $
        fromPositives (finMatToNonEmpty (maxBound :: FinMat '[3, 4, 5, 1]))
          @?= [3, 4, 5, 1]
    , testCase "prev finMat" $
        fmap (fromPositives . finMatToNonEmpty) (predSafe (maxBound :: FinMat '[3, 4, 5, 1]))
          @?= Just [3, 4, 4, 1]
    , testCase "prev finMat" $
        fmap (fromPositives . finMatToNonEmpty) (predSafe (maxBound :: FinMat '[3, 4, 5, 3]))
          @?= Just [3, 4, 5, 2]
    , testCase "next finMat" $
        succSafe (maxBound :: FinMat '[3, 4, 5, 3])
          @?= Nothing
    , testCase "prev finMat" $
        predSafe (minBound :: FinMat '[3, 4, 5, 3])
          @?= Nothing
    , testCase "next5 finMat" $
        fmap (fromPositives . finMatToNonEmpty) (take1 _5P $ enumFrom1 (fr $ nonEmptyToFinMat (_2P :| [_3P, _4P]) :: FinMat '[3, 4, 5]))
          @?= [2, 3, 4] :| [[2, 3, 5], [2, 4, 1], [2, 4, 2], [2, 4, 3]]
    , testCase "prev5 finMat" $
        fmap (fromPositives . finMatToNonEmpty) (take1 _5P $ enumFrom1R (fr $ nonEmptyToFinMat (_2P :| [_3P, _4P]) :: FinMat '[3, 4, 5]))
          @?= [2, 3, 4] :| [[2, 3, 3], [2, 3, 2], [2, 3, 1], [2, 2, 5]]
    , testCase "universe1 enum" $
        universe1 @(FinMat '[2, 3, 7])
          @?= fmi237'
    , testCase "universe1 enum" $
        universe1 @(FinMat '[1, 3, 5, 7, 2, 1])
          @?= fmiNS'
    , testCase "toEnum" $
        N.map toEnum (0 :| [1 .. 41])
          @?= fmi237'
    , testCase "mkFinMatC fail" $
        mkFinMatC @'[2, 3, 7] 42 (_2P :| [_3P, _7P])
          @?= Left "mkFinMat:is too large: maximum is 41 but found 42"
    , testCase "mkFinMatC fail" $
        mkFinMatC @'[2, 3, 7] (-1) (_2P :| [_3P, _7P])
          @?= Left "mkFinMat:cant be less than 0: i=-1"
    , testCase "mkFinMatC" $
        mkFinMatC @'[2, 3, 7] 41 (_2P :| [_3P, _7P])
          @?= Right maxBound
    , testCase "mkFinMatC" $
        mkFinMatC @'[2, 3, 7] 41 (_2P :| [_3P, _7P])
          @?= Right (FinMatU @'[2, 3, 7] 41 (_2P :| [_3P, _7P]))
    , testCase "mkFinMatC" $
        mkFinMatC @'[2, 3, 7] 0 (_2P :| [_3P, _7P])
          @?= Right minBound
    , testCase "mkFinMatC" $
        mkFinMatC @'[2, 3, 7] 0 (_2P :| [_3P, _7P])
          @?= Right (FinMatU @'[2, 3, 7] 0 (_2P :| [_3P, _7P]))
    , testCase "mkFinMatC" $
        mkFinMatC @'[2, 3, 7] 17 (_2P :| [_3P, _7P])
          @?= Right (FinMatU @'[2, 3, 7] 17 (_2P :| [_3P, _7P]))
    , testCase "nonEmptyToFinMat" $
        nonEmptyToFinMat @'[2, 3, 7] (_1P :| [_3P, _4P])
          @?= Right (FinMatU 17 (_2P :| [_3P, _7P]))
    , testCase "nonEmptyToFinMat" $
        nonEmptyToFinMat' @'[2, 3, 7] (_1P :| [_3P, _4P]) (_2P :| [_3P, _7P])
          @?= Right (FinMatU 17 (_2P :| [_3P, _7P]))
    , testCase "pos" $
        finMatC @'[3, 1] @'[3, 4]
          @?= FinMatU 8 (_3P :| [_4P])
    , testCase "pos" $
        finMatC @'[1, 1, 1, 1] @'[1, 2, 3, 4]
          @?= FinMatU 0 (_1P :| [_2P, _3P, _4P])
    , testCase "pos" $
        finMatC @'[3, 3, 3] @'[4, 4, 4]
          @?= FinMatU 42 (_4P :| [_4P, _4P])
    , testCase "finMatC" $
        finMatToNonEmpty (finMatC @'[1, 3, 4] @'[2, 3, 7])
          @?= _1P :| [_3P, _4P]
    , testCase "finMatC" $
        finMatC @'[1, 3, 4] @'[2, 3, 7]
          @?= FinMatU 17 (_2P :| [_3P, _7P])
    , testCase "finMatC" $
        (finMatC @'[1, 3, 4] @'[2, 3, 7] ^. _i1)
          @?= finC @1 @2
    , testCase "finMatC" $
        (finMatC @'[1, 3, 4] @'[2, 3, 7] ^. _i2)
          @?= finC @3 @3
    , testCase "finMatC" $
        (finMatC @'[1, 3, 4] @'[2, 3, 7] ^. _i3)
          @?= finC @4 @7
    , testCase "fromEnum" $
        N.map fromEnum fmi237'
          @?= 0 :| [1 .. 41]
    , testCase "toEnum one" $
        toEnum 1
          @?= FinMatU @'[2, 3, 7] 1 (_2P :| [_3P, _7P])
    , testCase "fromEnum one" $
        fromEnum @(FinMat '[2, 3, 4]) (FinMatU 4 (_2P :| [_3P, _4P]))
          @?= 4
    , testCase "toEnum one" $
        (toEnum 1 :: FinMat '[2, 3, 7])
          @?= FinMatU 1 (_2P :| [_3P, _7P])
    , testCase "fromEnum one" $
        fromEnum (FinMatU 7 (_2P :| [_3P, _7P]) :: FinMat '[2, 3, 7])
          @?= 7
    , testCase "minbound" $
        minBound @(FinMat '[2, 3, 4])
          @?= FinMatU 0 (_2P :| [_3P, _4P])
    , testCase "enum" $
        finMatToNonEmpty (fr $ nonEmptyToFinMat @'[2, 3, 4, 5] (_1P :| [_3P, _4P, _5P]))
          @?= _1P :| [_3P, _4P, _5P]
    , testCase "succ" $
        finMatToNonEmpty (succ (fr $ nonEmptyToFinMat @'[2, 3, 4, 5] (_1P :| [_3P, _4P, _5P])))
          @?= _2P :| [_1P, _1P, _1P]
    , testCase "pred" $
        finMatToNonEmpty (pred (fr $ nonEmptyToFinMat @'[2, 3, 4, 5] (_1P :| [_3P, _4P, _5P])))
          @?= _1P :| [_3P, _4P, _4P]
    , testCase "mkFinMatC" $
        let (xs, ys) = partitionEithers $ map (\i -> mkFinMatC @'[2, 4, 2, 4] i (_2P :| [_4P, _2P, _4P])) [-10 .. 100]
         in (length xs, length ys, length (groupByAdjacent1 (<) (N.fromList ys)))
              @?= (47, 64, 1)
    , testCase "maxBound" $
        (maxBound :: FinMat '[2, 3, 6])
          @?= FinMatU 35 (_2P :| [_3P, _6P])
    , testCase "minBound" $
        (minBound :: FinMat '[2, 3, 6])
          @?= FinMatU 0 (_2P :| [_3P, _6P])
    , testCase "iterateT1 next" $
        iterateT1 succSafe minBound
          @?= fmi237'
    , testCase "iterateT1 prev" $
        iterateT1 predSafe maxBound
          @?= N.reverse fmi237'
    , testCase "iterateT1 next" $
        iterateT1 succSafe minBound
          @?= fmiNS' @'[1, 3, 5, 7, 3, 2]
    , testCase "fmiNS" $
        fmiNS'
          @?= fmi237'
    , testCase "enumFrom" $
        [minBound :: FinMat '[2, 3] ..]
          @?= map (`FinMatU` (_2P :| [_3P])) [0 .. 5]
    , testCase "_i2 view" $
        (mkFinMatC @'[2, 3, 4] 10 (_2P :| [_3P, _4P]) ^. _Right . _i2)
          @?= (FinU _3P _3P :: Fin 3)
    , testCase "_i3 view" $
        (mkFinMatC @'[2, 3, 4] 10 (_2P :| [_3P, _4P]) ^. _Right . _i3)
          @?= (FinU _3P _4P :: Fin 4)
    , testCase "_i2 update" $
        (mkFinMatC @'[2, 3, 4] 0 (_2P :| [_3P, _4P]) & _Right . _i2 %~ succ)
          @?= Right (FinMatU 4 (_2P :| [_3P, _4P]))
    , testCase "read" $
        (read @(FinMat '[2, 3, 4]) $ show (finMatC @'[2, 3, 4] @'[2, 3, 4]))
          @?= finMatC @'[2, 3, 4] @'[2, 3, 4]
    , testCase "read" $
        (read @(FinMat '[2, 3, 4]) $ show (finMatC @'[1, 3, 2] @'[2, 3, 4]))
          @?= finMatC @'[1, 3, 2] @'[2, 3, 4]
    , testCase "enum roundtrip" $
        let xs = universe1 @(FinMat '[2, 4, 3])
            ys = fromEnum <$> xs
         in do
              fmap (toEnum @(FinMat '[2, 4, 3])) ys @?= xs
              ys @?= 0 :| [1 .. 23]
              N.head xs @?= minBound
              N.last xs @?= maxBound
    , testCase "showFinMat" $
        map showFinMat [FinMatU @'[2, 3, 5] 0 (_2P :| [_3P, _5P]), toEnum 5 ..]
          @?= ["FinMat@0{2,3,5}", "FinMat@5{2,3,5}", "FinMat@10{2,3,5}", "FinMat@15{2,3,5}", "FinMat@20{2,3,5}", "FinMat@25{2,3,5}"]
    , testCase "nonEmptyToFinMat'" $
        nonEmptyToFinMat' (_1P :| [_4P, _3P]) (_1P :| [_3P, _4P])
          @?= Left "nonEmptyToFinMat:These es=outofbounds (_4P,_3P) as=(_1P,_1P) :| [(_3P,_4P)]"
    , testCase "nonEmptyToFinMat'" $
        nonEmptyToFinMat' (_1P :| [_2P, _3P, _6P]) (_1P :| [_3P, _4P])
          @?= Left "nonEmptyToFinMat:too many indices: expected 3 is=_1P :| [_2P,_3P,_6P] ns=_1P :| [_3P,_4P]"
    , testCase "nonEmptyToFinMat'" $
        nonEmptyToFinMat' (_1P :| [_2P]) (_1P :| [_3P, _4P])
          @?= Left "nonEmptyToFinMat:not enough indices: expected 3 is=_1P :| [_2P] ns=_1P :| [_3P,_4P]"
    , testCase "nonEmptyToFinMat'" $
        nonEmptyToFinMat' (_3P :| [_1P, _4P]) (_3P :| [_8P, _7P])
          @?= Right (FinMatU @'[3, 8, 7] 115 (_3P :| [_8P, _7P]))
    , testCase "finMatToNonEmpty" $
        finMatToNonEmpty (FinMatU @'[3, 8, 7] 115 (_3P :| [_8P, _7P])) @?= _3P :| [_1P, _4P]
    , testCase "finMatToNonEmpty" $
        finMatToNonEmpty (FinMatU @'[3, 8, 7] 167 (_3P :| [_8P, _7P])) @?= _3P :| [_8P, _7P]
    , testCase "finMatToNonEmpty" $
        finMatToNonEmpty (FinMatU @'[3, 8, 7] 0 (_3P :| [_8P, _7P])) @?= _1P :| [_1P, _1P]
    , testCase "finMatToNonEmpty" $
        finMatToNonEmpty (FinMatU @'[1] 0 (_1P :| [])) @?= _1P :| []
    , testCase "finMatToNonEmpty" $
        finMatToNonEmpty (FinMatU @'[7] 0 (_7P :| [])) @?= _1P :| []
    , testCase "finMatToNonEmpty" $
        finMatToNonEmpty (FinMatU @'[7] 6 (_7P :| [])) @?= _7P :| []
    , testCase "finMatC" $
        (finMatC @(NN 1234) @(NN 1234) - minBound)
          @?= FinMatU @'[1, 2, 3, 4] 23 (_1P :| [_2P, _3P, _4P])
    , testCase "finMatC" $
        (pure (finMatC @(NN 1234) @(NN 1234)) .- pure minBound)
          @?= Right (FinMatU @'[1, 2, 3, 4] 23 (_1P :| [_2P, _3P, _4P]))
    , testCase "finMatC" $
        pure (finMatC @(NN 1234) @(NN 1234)) .+ pure maxBound
          @?= Left "(.+):mkFinMat:is too large: maximum is 23 but found 46"
    , testCase "Num1" $
        (pure (FinMatU @(NN 345) 14 (_3P :| [_4P, _5P])) .+ pure minBound)
          @?= Right (FinMatU @(NN 345) 14 (_3P :| [_4P, _5P]))
    , testCase "Num1" $
        pure (FinMatU @(NN 345) 14 (_3P :| [_4P, _5P])) .+ pure maxBound
          @?= Left "(.+):mkFinMat:is too large: maximum is 59 but found 73"
    , testCase "Num1" $
        (pure (FinMatU @(NN 345) 14 (_3P :| [_4P, _5P])) .+ pure 5)
          @?= Right (FinMatU @(NN 345) 19 (_3P :| [_4P, _5P]))
    , testCase "Num1" $
        (pure 5 .* pure 7)
          @?= Right (FinMatU @(NN 236) 35 (_2P :| [_3P, _6P]))
    , testCase "Num1" $
        (pure 7 .- pure 4)
          @?= Right (FinMatU @(NN 236) 3 (_2P :| [_3P, _6P]))
    , testCase "Num1" $
        finMatC @(NN 111) @(NN 123)
          @?= FinMatU @(NN 123) 0 (_1P :| [_2P, _3P])
    , testCase "Num1" $
        (pure (finMatC @(NN 111) @(NN 123)) .- pure minBound)
          @?= Right (FinMatU @(NN 123) 0 (_1P :| [_2P, _3P]))
    , testCase "Num1" $
        (pure (finMatC @(NN 111) @(NN 123)) .- pure (finMatC @(NN 111)))
          @?= Right (FinMatU @(NN 123) 0 (_1P :| [_2P, _3P]))
    , testCase "Num1" $
        pure (finMatC @(NN 111) @(NN 123)) .- pure (finMatC @(NN 112))
          @?= Left "(.-):mkFinMat:cant be less than 0: i=-1"
    , testCase "Num1" $
        (pure (finMatC @(NN 111) @(NN 123)) .+ pure (finMatC @(NN 112)))
          @?= Right (FinMatU @(NN 123) 1 (_1P :| [_2P, _3P]))
    , testCase "Num1" $
        (pure (finMatC @(NN 111) @(NN 123)) .+ pure (finMatC @(NN 122)))
          @?= Right (FinMatU @(NN 123) 4 (_1P :| [_2P, _3P]))
    , testCase "Num1" $
        (pure (finMatC @(NN 312) @(NN 573)) .+ pure (finMatC @(NN 363)))
          @?= Right (FinMatU @(NN 573) 102 (_5P :| [_7P, _3P]))
    , testCase "Num1" $
        mkFinMatC @(NN 573) 102 (_1P :| [_2P, _3P])
          @?= Left "mkFinMatC: invalid indices: typelevel [5,7,3] /= [1,2,3]"
    , testCase "Num1" $
        pure (finMatC @(NN 312) @(NN 573)) .+ pure (finMatC @(NN 373))
          @?= Left "(.+):mkFinMat:is too large: maximum is 104 but found 105"
    , testCase "signum1" $
        signum1 (Right (FinMatU @(NN 345) 0 (_3P :| [_4P, _5P])))
          @?= Right (FinMatU @(NN 345) 0 (_3P :| [_4P, _5P]))
    , testCase "signum1" $
        signum1 (Right (FinMatU @(NN 345) 1 (_3P :| [_4P, _5P])))
          @?= Right (FinMatU @(NN 345) 1 (_3P :| [_4P, _5P]))
    , testCase "signum1" $
        signum1 (Right (FinMatU @(NN 345) 10 (_3P :| [_4P, _5P])))
          @?= Right (FinMatU @(NN 345) 1 (_3P :| [_4P, _5P]))
    , testCase "Num1" $
        (pure (finMatC @(NN 217) @(NN 537)) .+ pure minBound .* pure maxBound)
          @?= Right (FinMatU @(NN 537) 27 (_5P :| [_3P, _7P]))
    , testCase "Num1" $
        pure (finMatC @(NN 21) @(NN 53)) .+ pure maxBound
          @?= Left "(.+):mkFinMat:is too large: maximum is 14 but found 17"
    , testCase "withOp" $
        withOp succ (finMatC @(NN 234) @(NN 234))
          @?= Left "mkFinMat:is too large: maximum is 23 but found 24"
    , testCase "withOp" $
        withOp pred (finMatC @(NN 234) @(NN 234))
          @?= Right (FinMatU 22 (_2P :| [_3P, _4P]))
    , testCase "finMatC" $
        (finMatC @'[1, 3, 4] @'[2, 3, 7] & _i3 %~ succ . succ)
          @?= FinMatU @'[2, 3, 7] 19 (_2P :| [_3P, _7P])
    , testCase "finMatC" $
        (finMatC @'[1, 3, 4] @'[2, 3, 7] & _i1 %~ succ)
          @?= FinMatU @'[2, 3, 7] 38 (_2P :| [_3P, _7P])
    , testCase "finMatC" $
        (finMatC @'[1, 3, 4] @'[2, 3, 7] & _i3 .~ _F2)
          @?= FinMatU @'[2, 3, 7] 15 (_2P :| [_3P, _7P])
    , testCase "finMatC" $
        (finMatC @'[1, 3, 4] @'[2, 3, 7] & _i3 .~ _F3)
          @?= FinMatU @'[2, 3, 7] 16 (_2P :| [_3P, _7P])
    , testCase "finMatC" $
        (finMatC @'[1, 1] @'[11, 7] & _i1 %~ succ)
          @?= FinMatU @'[11, 7] 7 (_11P :| [_7P])
    , testCase "finMatC" $
        (finMatC @'[1, 1] @'[11, 7] & _i1 %~ id)
          @?= FinMatU @'[11, 7] 0 (_11P :| [_7P])
    , testCase "finMatC" $
        (finMatC @'[1, 1] @'[11, 7] & _i1 %~ succ . succ)
          @?= FinMatU @'[11, 7] 14 (_11P :| [_7P])
    , testCase "finMatC" $
        (finMatC @(NN 543) @(NN 789) ^. _i1)
          @?= FinU _5P _7P
    , testCase "finMatC" $
        (finMatC @(NN 543) @(NN 789) ^. _i2)
          @?= FinU _4P _8P
    , testCase "finMatC" $
        (finMatC @(NN 543) @(NN 789) ^. _i3)
          @?= FinU _3P _9P
    , testCase "toFinMatFromPos" $
        toFinMatFromPos @0 @(NN 345)
          @?= FinMatU @'[3, 4, 5] 0 (_3P :| [_4P, _5P])
    , testCase "toFinMatFromPos" $
        toFinMatFromPos @59 @(NN 345)
          @?= FinMatU @'[3, 4, 5] 59 (_3P :| [_4P, _5P])
    , testCase "toFinMatFromPos" $
        toFinMatFromPos @34 @(NN 345)
          @?= FinMatU @'[3, 4, 5] 34 (_3P :| [_4P, _5P])
    , testCase "toFinMatFromPos" $
        toFinMatFromPos @0 @'[1]
          @?= FinMatU @'[1] 0 (_1P :| [])
    , testCase "toFinMatFromPos" $
        toFinMatFromPos @0 @'[2]
          @?= FinMatU @'[2] 0 (_2P :| [])
    , testCase "toFinMatFromPos" $
        toFinMatFromPos @1 @'[2]
          @?= FinMatU @'[2] 1 (_2P :| [])
    , testCase "relPos" $
        relPos ((_1P, _3P) :| []) @?= (_3P, 0)
    , testCase "relPos" $
        relPos ((_2P, _3P) :| []) @?= (_3P, 1)
    , testCase "relPos" $
        relPos ((_1P, _1P) :| [(_1P, _1P)]) @?= (_1P, 0)
    , testCase "relPos" $
        relPos ((_1P, _1P) :| [(_1P, _5P), (_5P, _5P)]) @?= (_P @25, 4)
    , testCase "relPos" $
        relPos ((_4P, _7P) :| [(_3P, _5P), (_2P, _5P)]) @?= (_P @175, 86)
    , testCase "readFinMat" $
        readFinMat @'[7, 3, 3] "FinMat@5{7,3,3}xyz" @?= [(finMatC @'[1, 2, 3] @'[7, 3, 3], "xyz")]
    , testCase "readFinMat" $
        let m = finMatC @'[1, 2, 3] @'[7, 3, 3]
         in readFinMat @'[7, 3, 3] (show m ++ "  ") @?= [(m, "  ")]
    , testCase "readFinMat" $
        readFinMat @'[7, 3, 3] "FinMat@6{1,2,3}xyz" @?= []
    , testCase "readFinMat" $
        readFinMat @'[1, 2, 3] "   FinMat@4{     1,             2,   3}xy"
          @?= [(FinMatU @'[1, 2, 3] 4 (_1P :| [_2P, _3P]), "xy")]
    , testCase "showFinMat'" $
        showFinMat' (finMatC @'[2, 3, 5] @'[4, 4, 6])
          @?= "FinMat@40{2,3,5|4,4,6}"
    , testCase "showFinMat'" $
        showFinMat' (finMatC @'[1] @'[1])
          @?= "FinMat@0{1|1}"
    , testCase "showFinMat'" $
        showFinMat' (finMatC @(NN 123) @(NN 234))
          @?= "FinMat@6{1,2,3|2,3,4}"
    , testCase "showFinMat'" $
        showFinMat' (finMatC @(NN 111) @(NN 234))
          @?= "FinMat@0{1,1,1|2,3,4}"
    , testCase "showFinMat'" $
        showFinMat' (finMatC @(NN 114) @(NN 234))
          @?= "FinMat@3{1,1,4|2,3,4}"
    , testCase "showFinMat'" $
        showFinMat' (finMatC @(NN 9) @(NN 9))
          @?= "FinMat@8{9|9}"
    , testCase "showFinMat" $
        showFinMat (finMatC @'[1] @'[1])
          @?= "FinMat@0{1}"
    , testCase "showFinMat" $
        showFinMat (finMatC @'[1] @'[10])
          @?= "FinMat@0{10}"
    , testCase "showFinMat" $
        showFinMat (finMatC @'[10] @'[10])
          @?= "FinMat@9{10}"
    , testCase "showFinMat" $
        showFinMat (finMatC @'[4] @'[10])
          @?= "FinMat@3{10}"
    , testCase "fromInteger1" $
        fromInteger1 (minBound @(FinMat '[2, 3, 4])) 0
          @?= Right (FinMatU @'[2, 3, 4] 0 (_2P :| [_3P, _4P]))
    , testCase "fromInteger1" $
        fromInteger1 (minBound @(FinMat '[2, 3, 4])) (-5)
          @?= Left "mkFinMat:cant be less than 0: i=-5"
    , testCase "fromInteger1" $
        fromInteger1 (minBound @(FinMat '[2, 3, 4])) 23
          @?= Right (FinMatU @'[2, 3, 4] 23 (_2P :| [_3P, _4P]))
    , testCase "fromInteger1" $
        fromInteger1 (minBound @(FinMat '[2, 3, 4])) 24
          @?= Left "mkFinMat:is too large: maximum is 23 but found 24"
    , testCase "fromInteger1" $
        fromInteger1 (minBound @(FinMat '[2, 3, 4])) (-1)
          @?= Left "mkFinMat:cant be less than 0: i=-1"
    , testCase "toInteger1" $
        toInteger1 (FinMatU @'[2, 3, 4] 0 (_2P :| [_3P, _4P]))
          @?= 0
    , testCase "toInteger1" $
        toInteger1 (FinMatU @'[2, 3, 4] 23 (_2P :| [_3P, _4P]))
          @?= 23
    , testCase "toInteger1" $
        toInteger1 (FinMatU @'[2, 3, 4] 12 (_2P :| [_3P, _4P]))
          @?= 12
    , testCase "index lenses" $
        finMatC @'[2, 5, 3, 7] @'[2, 12, 13, 8] ^. _i1
          @?= FinU @2 _2P _2P
    , testCase "index lenses" $
        finMatC @'[2, 5, 3, 7] @'[2, 12, 13, 8] ^. _i2
          @?= FinU @12 _5P _12P
    , testCase "index lenses" $
        finMatC @'[2, 5, 3, 7] @'[2, 12, 13, 8] ^. _i4
          @?= FinU @8 _7P _8P
    , testCase "finMat finMatC" $
        finMat @'[2, 12, 13, 8] (6 + 2 * 8 + 4 * 13 * 8 + 1 * 12 * 13 * 8)
          @?= Right (finMatC @'[2, 5, 3, 7] @'[2, 12, 13, 8])
    , testCase "finMat finMatC" $
        finMat @'[21] 0
          @?= Right (finMatC @'[1] @'[21])
    , testCase "_finMatCons" $
      (finMatC @'[2,1] @'[7,1] ^. _finMatCons) @?= (finC @2 @7, finMatC @'[1] @'[1])
    , testCase "_finMatCons" $
      (finMatC @'[2,1] @'[7,4] ^. _finMatCons) @?= (finC @2 @7, finMatC @'[1] @'[4])
    , testCase "_finMatCons" $
      (finMatC @'[2,2] @'[7,2] ^. _finMatCons) @?= (finC @2 @7, finMatC @'[2] @'[2])

    , testCase "_finMatCons" $
      (finMatC @'[2,4] @'[7,4] ^. _finMatCons) @?= (finC @2 @7, finMatC @'[4] @'[4])
    ]

fmi237' :: NonEmpty (FinMat '[2, 3, 7])
fmi237' = frp $ traverse (nonEmptyToFinMat <=< toPositives) fmi237

fmi237 :: NonEmpty (NonEmpty Int)
fmi237 = fmap N.fromList $ [1, 1, 1] :| [[1, 1, 2], [1, 1, 3], [1, 1, 4], [1, 1, 5], [1, 1, 6], [1, 1, 7], [1, 2, 1], [1, 2, 2], [1, 2, 3], [1, 2, 4], [1, 2, 5], [1, 2, 6], [1, 2, 7], [1, 3, 1], [1, 3, 2], [1, 3, 3], [1, 3, 4], [1, 3, 5], [1, 3, 6], [1, 3, 7], [2, 1, 1], [2, 1, 2], [2, 1, 3], [2, 1, 4], [2, 1, 5], [2, 1, 6], [2, 1, 7], [2, 2, 1], [2, 2, 2], [2, 2, 3], [2, 2, 4], [2, 2, 5], [2, 2, 6], [2, 2, 7], [2, 3, 1], [2, 3, 2], [2, 3, 3], [2, 3, 4], [2, 3, 5], [2, 3, 6], [2, 3, 7]]

fmiNS' :: forall ns. NS ns => NonEmpty (FinMat ns)
fmiNS' = frp $ traverse (nonEmptyToFinMat @ns <=< toPositives) (fmiNS (fmap unP (fromNSP @ns)))

fmiNS :: NonEmpty Int -> NonEmpty (NonEmpty Int)
fmiNS = traverse (N.fromList . enumFromTo 1)

