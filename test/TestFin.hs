{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module TestFin where

import Cybus.Fin
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as N
import Data.Pos
import Primus.Enum
import Primus.Num1
import Test.Tasty
import Test.Tasty.HUnit

doit :: IO ()
doit = defaultMain suite

suite :: TestTree
suite =
  testGroup
    "TestFin"
    [ testCase "read" $
        reads @(Fin 3) "Fin(2,5)"
          @?= []
    , testCase "read" $
        reads @(Fin 4) (show $ FinU @3 _1P _3P)
          @?= []
    , testCase "readF" $
        reads @(Fin 3) (show $ FinU @3 _1P _3P)
          @?= [(FinU _1P _3P, "")]
    , testCase "read" $
        reads @(Fin 5) "Fin(2,5)"
          @?= [(FinU _2P _5P, "")]
    , testCase "showFin" $
        showFin (FinU _1P _3P :: Fin 3)
          @?= "Fin(1,3)"
    , testCase "showFin" $
        showFin (FinU _4P _9P :: Fin 9)
          @?= "Fin(4,9)"
    , testCase "finC" $
        finC @2 @5
          @?= FinU _2P _5P
    , testCase "finC" $
        finC @3 @3
          @?= FinU _3P _3P
    , testCase "enumFrom" $
        [FinU @5 _1P _5P ..]
          @?= map (`FinU` _5P) [_1P .. _5P]
    , testCase "enumFromThen" $
        [FinU @5 _1P _5P, FinU _3P _5P ..]
          @?= [FinU @5 _1P _5P, FinU _3P _5P, FinU _5P _5P]
    , testCase "universe1" $
        universe1 @(Fin 4)
          @?= (FinU _1P _4P :| [FinU _2P _4P, FinU _3P _4P, FinU _4P _4P])
    , testCase "enum roundtrip" $
        let xs = universe1 @(Fin 11)
            ys = fromEnum <$> xs
         in do
              fmap (toEnum @(Fin 11)) ys @?= xs
              ys @?= (0 :| [1 .. 10])
              N.head xs @?= minBound
              N.last xs @?= maxBound
    , testCase "Num1" $
        withOp (subtract 4) (minBound :: Fin 4)
          @?= Left "integerToEnumSafe:underflow where -4 not in range [0..3]"
    , testCase "Num1" $
        withOp (* 0) (minBound :: Fin 4)
          @?= Right (FinU _1P _4P)
    , testCase "Num" $
        ((minBound :: Fin 4) * (1 :: Fin 4)) -- keep hlint happy as "x * 1 == x"
          @?= FinU _1P _4P
    , testCase "Num" $
        ((minBound :: Fin 4) + 1)
          @?= FinU _2P _4P
    , testCase "Num" $
        (1 :: Fin 4)
          @?= FinU _2P _4P
    , testCase "Num" $
        ((minBound :: Fin 10) + 7)
          @?= FinU _8P _10P
    , testCase "Num" $
        (toEnum 7 :: Fin 10)
          @?= FinU _8P _10P
    , testCase "Num" $
        withOp (+ 0) (minBound :: Fin 4)
          @?= Right (FinU _1P _4P)
    , testCase "Num" $
        withOp (+ 1) (minBound :: Fin 4)
          @?= Right (FinU _2P _4P)
    , testCase "Num" $
        ((minBound :: Fin 4) + 2)
          @?= FinU _3P _4P
    , testCase "Num" $
        withOp2 (-) (maxBound :: Fin 4) maxBound
          @?= Right (FinU _1P _4P)
    , testCase "Num" $
        withOp2 (-) (minBound :: Fin 4) maxBound
          @?= Left "integerToEnumSafe:underflow where -3 not in range [0..3]"
    , testCase "Num" $
        (Right (minBound :: Fin 4) .- Right maxBound)
          @?= Left "(.-):integerToEnumSafe:underflow where -3 not in range [0..3]"
    , testCase "Num" $
        (finC @3 @15 * finC @5)
          @?= FinU _9P _15P
    , testCase "Num" $
        (Right (finC @4 @16) .* Right (finC @6))
          @?= Right (FinU @16 _16P _16P)
    , testCase "Num1" $
        (pure (finC @4 @7) .+ pure minBound .+ pure maxBound)
          @?= Left "(.+):integerToEnumSafe:overflow where 9 not in range [0..6]"
    , testCase "Num1" $
        (pure (finC @4 @5) .- fromInteger1 minBound 1)
          @?= Right (FinU @5 _3P _5P)
    , testCase "Num1" $
        fromInteger1 (minBound @(Fin 5)) 99
          @?= Left "integerToEnumSafe:overflow where 99 not in range [0..4]"
    , testCase "Num1" $
        fromInteger1 (minBound @(Fin 5)) 3
          @?= Right (FinU @5 _4P _5P)
    , testCase "Num1" $
        toInteger1 (finC @11 @17)
          @?= 10
    , testCase "Num1" $
        toInteger1 (finC @17 @17)
          @?= 16
    , testCase "Num1" $
        toInteger1 (finC @1 @17)
          @?= 0
    , testCase "Num1" $
        fromInteger1 (minBound @(Fin 5)) 0
          @?= Right (FinU @5 _1P _5P)
    , testCase "Num1" $
        (pure (finC @4 @5) .- fromInteger1 minBound 6)
          @?= Left "integerToEnumSafe:overflow where 6 not in range [0..4]"
    , testCase "Num1" $
        (pure (finC @4 @5) .- fromInteger1 minBound 4)
          @?= Left "(.-):integerToEnumSafe:underflow where -1 not in range [0..4]"
    , testCase "Num1" $
        (pure (finC @4 @5) .- fromInteger1 minBound 7)
          @?= Left "integerToEnumSafe:overflow where 7 not in range [0..4]"
    , testCase "Num1" $
        (pure (finC @4 @10) .- fromInteger1 minBound 9)
          @?= Left "(.-):integerToEnumSafe:underflow where -6 not in range [0..9]"
    , testCase "Num1" $
        (pure (finC @4 @5) .+ pure minBound .* pure maxBound)
          @?= Right (FinU @5 _4P _5P)
    , testCase "Num1" $
        -- 2 * 14 - 7 * 2 - 1 = 13 == _14P
        withOp3 (\a b c -> b * c - a * 2 - 1) 7 (finC @3 @15) maxBound
          @?= Right (FinU @15 _14P _15P)
    , testCase "Num1" $
        withOp3 (\a b c -> a + b * c - 7) (finC @1 @15) minBound maxBound
          @?= Left "integerToEnumSafe:underflow where -7 not in range [0..14]"
    , testCase "_Fin" $
        finC @4 @5
          @?= FinU @5 _4P _5P
    , testCase "_Fin" $
        mkFinC @5 _4P _5P
          @?= Right (FinU @5 _4P _5P)
    , testCase "_Fin" $
        mkFinC @5 _10P _5P
          @?= Left "mkFin:_10P is too large: maximum is _5P"
    , testCase "mkFinC" $
        mkFinC @9 _4P _10P
          @?= Left "mkFinC: _10P /= _9P at typelevel"
    , testCase "mkFinC" $
        mkFinC @9 _12P _10P
          @?= Left "mkFinC: _10P /= _9P at typelevel"
    , testCase "signum1" $
        signum1 (Right (FinU @5 _4P _5P))
          @?= Right (FinU @5 _1P _5P)
    , testCase "signum1" $
        signum1 (Right (FinU @5 _1P _5P))
          @?= Right (FinU @5 _1P _5P)
    , testCase "signum1" $
        signum1 (Right (FinU @1 _1P _1P))
          @?= Right (FinU @1 _1P _1P)
    , testCase "abs1" $
        abs1 (Right (FinU @1 _1P _1P))
          @?= Right (FinU @1 _1P _1P)
    , testCase "abs1" $
        abs1 (Right (FinU @20 _7P _20P))
          @?= Right (FinU @20 _7P _20P)
    , testCase "_F4" $
        _F4 @10 @?= FinU @10 _4P _10P
    , testCase "withOp2" $
        withOp2 (+) (_F4 @10) _F9
          @?= Left "integerToEnumSafe:overflow where 11 not in range [0..9]"
    , testCase "withOp2" $
        withOp2 (+) (_F4 @10) _F6
          @?= Right (FinU @10 _9P _10P)
    , testCase "withOp2" $
        withOp2 (+) (_F4 @10) _F7
          @?= Right (FinU @10 _10P _10P)
    , testCase "withOp2" $
        withOp2 (-) (_F4 @10) _F7
          @?= Left "integerToEnumSafe:underflow where -3 not in range [0..9]"
    , testCase "withOp2" $
        withOp2 (-) (_F7 @10) _F7
          @?= Right (FinU @10 _1P _10P)
    , testCase "withOp2" $
        withOp2 (-) (_F8 @10) _F7
          @?= Right (FinU @10 _2P _10P)
    , testCase "pred1" $
        pred1 (Right (FinU @5 _1P _5P))
          @?= Left "pred1:integerToEnumSafe:underflow where -1 not in range [0..4]"
    , testCase "succ1" $
        succ1 (Right (FinU @5 _5P _5P))
          @?= Left "succ1:integerToEnumSafe:overflow where 5 not in range [0..4]"
    , testCase "pred1" $
        pred1 (Right (FinU @5 _2P _5P))
          @?= Right (FinU @5 _1P _5P)
    , testCase "succ1" $
        succ1 (Right (FinU @5 _2P _5P))
          @?= Right (FinU @5 _3P _5P)
    , testCase "abs1" $
        abs1 (Right (FinU @5 _2P _5P))
          @?= Right (FinU @5 _2P _5P)
    , testCase "negate1" $
        negate1 (Right (FinU @5 _5P _5P))
          @?= Left "negate1:integerToEnumSafe:underflow where -4 not in range [0..4]"
    , testCase "fin" $
        fin @10 0 @?= Left "eitherPos: i<=0: found 0"
    , testCase "fin" $
        fin @10 (-5) @?= Left "eitherPos: i<=0: found -5"
    , testCase "fin" $
        fin @10 11 @?= Left "mkFin:_11P is too large: maximum is _10P"
    , testCase "fin" $
        fin @10 10 @?= Right (FinU _10P _10P)
    , testCase "fin" $
        fin @10 5 @?= Right (FinU _5P _10P)
    , testCase "finC" $
        _F7 @10 @?= FinU @10 _7P _10P
    , testCase "finC" $
        _F1 @7 @?= FinU @7 _1P _7P
    , testCase "finC" $
        _F1 @1 @?= FinU @1 _1P _1P
    , testCase "finC" $
        _F3 @3 @?= FinU @3 _3P _3P
    , testCase "readFin" $
        readFin @13 "Fin(10,13)xyz" @?= [(finC @10 @13, "xyz")]
    , testCase "readFin" $
        let m = finC @10 @13
         in readFin @13 (show m ++ "  ") @?= [(m, "  ")]
    , testCase "readFin" $
        readFin @13 "Fin(14,13)xyz" @?= []
    , testCase "readFin" $
        readFin @15 "Fin(12,13)xyz" @?= []
    , testCase "readFin" $
        readFin @13 "Fin(0,13)xyz" @?= []
    ]
