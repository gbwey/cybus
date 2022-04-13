{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Main where
--import Data.List.NonEmpty (NonEmpty(..))
--import qualified Data.List.NonEmpty as N
import qualified GHC.TypeNats as GN
import Cybus

main :: IO ()
main = putStr $ show $ mm @'[2,3,4]

tst1 :: Mat '[4,5,3] Int
tst1 = gen id

tst2 :: Mat '[4] (Mat '[5,3] Int)
tst2 = toVec (gen @'[4,5,3] id)

tst3 :: Mat (n ': n1 ': ns) a -> Mat '[n] (Mat (n1 ': ns) a)
tst3 = toVec

tst4 :: Mat2 4 7 Int
tst4 = mat2 @4 @3 [1..] `multMat` mat2 @3 @7 [1..]

tst5 :: Mat (2 ': ns) a -> Mat (1 ': ns) a
tst5 = deleteRow @2

tst6 :: FinC 1 (1 GN.+ n) => Mat (1 GN.+ n ': ns) a -> Mat (n ': ns) a
tst6 = deleteRow @1

{-
>tst3 (mm @'[2,3,4])
Vec@2 [Mat2@(3,4)
  [
     [1,2,3,4],
     [5,6,7,8],
     [9,10,11,12]
  ]
,Mat2@(3,4)
  [
     [13,14,15,16],
     [17,18,19,20],
     [21,22,23,24]
  ]
]
it :: Mat '[2] (Mat '[3, 4] Int)

>tst6 (mm @'[9,3])
Mat2@(8,3)
  [
     [4,5,6],
     [7,8,9],
     [10,11,12],
     [13,14,15],
     [16,17,18],
     [19,20,21],
     [22,23,24],
     [25,26,27]
  ]

it :: Mat '[8, 3] Int
-}