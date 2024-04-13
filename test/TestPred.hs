module Main (main) where
import System.Exit (exitFailure)
import Control.Monad (when)
import Pred

import Dibujo

import Test.HUnit

import qualified System.Exit as Exit
import Test.HUnit (Testable(test))
import Pred
       
funCambio :: a -> Dibujo a
funCambio a = rotar (figura a)

testCambiar :: Test
testCambiar = TestCase $ assertEqual "cambiar" (cambiar (== 1) funCambio (rotar (rotar (figura 1)))) (rotar (rotar (rotar (figura 1))))

testanyDibTrue :: Test
testanyDibTrue = TestCase $ assertEqual "anyDib" (anyDib (== 1) (encimar (figura 2) (figura 1))) True

testanyDibFalse :: Test
testanyDibFalse = TestCase $ assertEqual "anyDib" (anyDib (== 1) (encimar (figura 2) (figura 2))) False

testallDibTrue :: Test
testallDibTrue = TestCase $ assertEqual "allDib" (allDib (== 1) (encimar (figura 1) (figura 1))) True

testallDibFalse :: Test
testallDibFalse = TestCase $ assertEqual "allDib" (allDib (== 1) (encimar (figura 1) (figura 2))) False

andPTrue :: Test
andPTrue = TestCase $ assertEqual "andP" (andP (> 1) (< 4) 3) True

andPFalse :: Test
andPFalse = TestCase $ assertEqual "andP" (andP (> 0) (< 2) 3) False

orPTrue :: Test
orPTrue = TestCase $ assertEqual "orP" (orP (> 1) (< 4) 3) True

orPFalse :: Test
orPFalse = TestCase $ assertEqual "orP" (orP (> 0) (< 2) 3) True


tests :: Test
tests = TestList [TestLabel "testanyDibTrue" testanyDibTrue,
                  TestLabel "testanyDibFalse" testanyDibFalse,
                  TestLabel "testallDibTrue" testallDibTrue,
                  TestLabel "testallDibFalse" testallDibFalse,
                  TestLabel "andPTrue" andPTrue,
                  TestLabel "andPFalse" andPFalse,
                  TestLabel "orPTrue" orPTrue,
                  TestLabel "orPFalse" orPFalse,
                  TestLabel "testCambiar" testCambiar]
                    
main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess