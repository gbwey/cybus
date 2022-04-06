module Main where

import System.Environment
import Test.Tasty
import qualified TestEnum
import qualified TestFin
import qualified TestFinMat
import qualified TestMat
import qualified TestNatHelper

main :: IO ()
main = do
  xs <- getArgs
  let x1 =
        [ TestMat.suiteCheckers
        ]
  (os, zs) <- case xs of
    "0" : os -> putStrLn "NORMAL (Explicit)" >> return (os, mempty)
    "1" : os -> putStrLn "VERBOSE" >> return (os, x1)
    --    "2" : os -> putStrLn "EXTRA VERBOSE" >> return (os, x1 ++ x2)
    os -> putStrLn "NORMAL" >> return (os, [])
  withArgs os $
    defaultMain $
      testGroup
        "alltests"
        ( [ TestEnum.suite
          , TestFin.suite
          , TestFinMat.suite
          , TestMat.suite
          , TestNatHelper.suite
          ]
            ++ zs
        )
