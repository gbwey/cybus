{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

module Main where
--import Data.List.NonEmpty (NonEmpty(..))
--import qualified Data.List.NonEmpty as N
import Cybus

main :: IO ()
main = putStr $ show $ mm @(NN 234)

tst1 :: Mat '[4,5,3] Int
tst1 = gen id

tst2 :: Mat '[4] (Mat '[5,3] Int)
tst2 = toVec (gen @'[4,5,3] id)

tst3 :: Mat (n ': n1 ': ns) a -> Mat '[n] (Mat (n1 ': ns) a)
tst3 = toVec

tst4 :: Mat2 4 7 Int
tst4 = mat2 @4 @3 [1..] `multMat` mat2 @3 @7 [1..]
{-
>tst3 (mm @234)
Mat@[2]
[Mat@[3,4]
  [
     [1,2,3,4],
     [5,6,7,8],
     [9,10,11,12]
  ]
,Mat@[3,4]
  [
     [13,14,15,16],
     [17,18,19,20],
     [21,22,23,24]
  ]
]

it :: Mat '[2] (Mat '[3,4] Int)
-}