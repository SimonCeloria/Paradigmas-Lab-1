module Main (main) where

import Test.HUnit

import qualified System.Exit as Exit
import Dibujo
import qualified Text.Read.Lex as Dibujo

data TriORect = Triangulo | Rectangulo deriving (Eq, Show)

testFigura :: Test
testFigura =
    TestCase $ assertEqual "figura" (show $ figura 5) "Figura 5"

testApilar :: Test
testApilar =
    TestCase $ assertEqual "apilar" (show $ apilar 1 1 (figura (5 :: Double)) (figura (5 :: Double)))
                 "Apilar 1.0 1.0 (Figura 5.0) (Figura 5.0)"

testEncimar :: Test
testEncimar =
    TestCase $ assertEqual "encimar" (show $ encimar (figura (5 :: Double)) (figura (5 :: Double)))
                 "Encimar (Figura 5.0) (Figura 5.0)"

testJuntar :: Test
testJuntar =
    TestCase $ assertEqual "juntar" (show $ juntar 1 1 (figura (5 :: Double)) (figura (5 :: Double)))
                 "Juntar 1.0 1.0 (Figura 5.0) (Figura 5.0)"

testEspejar :: Test
testEspejar =
    TestCase $ assertEqual "espejar" (show $ espejar (figura 5)) "Espejar (Figura 5)"

testRot45 :: Test
testRot45 =
    TestCase $ assertEqual "rot45" (show $ rot45 (figura 5)) "Rot45 (Figura 5)"

testRotar :: Test
testRotar =
    TestCase $ assertEqual "rotar" (show $ rotar (figura 5)) "Rotar (Figura 5)"
    
testR180 :: Test
testR180 =
    TestCase $ assertEqual "r180" (show $ r180 (figura 5)) "Rot45 (Rot45 (Rot45 (Rot45 (Figura 5))))"

testR270 :: Test
testR270 =
    TestCase $ assertEqual "r270" (show $ r270 (figura 5)) "Rot45 (Rot45 (Rot45 (Rot45 (Rot45 (Rot45 (Figura 5))))))"

testUpUpUp :: Test
testUpUpUp =
    TestCase $ assertEqual "^^^" (show $ figura 5 ^^^ figura 5)
                 "Encimar (Figura 5) (Figura 5)"

testSlashSlashSlash :: Test
testSlashSlashSlash =
    TestCase $ assertEqual "///" (show $ figura 5 /// figura 5)
                 "Juntar 1.0 1.0 (Figura 5) (Figura 5)"

testDotDotDot :: Test
testDotDotDot =
    TestCase $ assertEqual ".-." (show $ figura (5) .-. figura (5))
                 "Apilar 1.0 1.0 (Figura 5) (Figura 5)"

testComp :: Test
testComp =
    TestCase $ assertEqual "comp" (show $ comp 2 rotar (figura 5))
                 "Rotar (Rotar (Figura 5))"

testEncimar4 :: Test
testEncimar4 =
    TestCase $ assertEqual
    "encimar4"
      (show $ encimar4 (figura 1))                                                                          
      "Encimar (Figura 1) (Encimar (Rot45 (Rot45 (Figura 1))) (Encimar (Rot45 (Rot45 (Rot45 (Rot45 (Figura 1))))) (Rot45 (Rot45 (Rot45 (Rot45 (Rot45 (Rot45 (Figura 1)))))))))"

testCuarteto :: Test
testCuarteto =
    TestCase $ assertEqual "cuarteto" (show $ cuarteto (figura 5) (figura 5) (figura 5) (figura 5))
                 "Apilar 1.0 1.0 (Juntar 1.0 1.0 (Figura 5) (Figura 5)) (Juntar 1.0 1.0 (Figura 5) (Figura 5))"

testCiclar :: Test
testCiclar =
    TestCase $ assertEqual "ciclar" (show $ ciclar (figura 5))
                 "Apilar 1.0 1.0 (Juntar 1.0 1.0 (Figura 5) (Rotar (Figura 5))) (Juntar 1.0 1.0 (Rot45 (Rot45 (Rot45 (Rot45 (Figura 5))))) (Rot45 (Rot45 (Rot45 (Rot45 (Rot45 (Rot45 (Figura 5))))))))"

testMapDib :: Test
testMapDib =
    TestCase $ assertEqual "mapDib" (show $ mapDib (+1) (encimar (figura 5) (figura 5)))
                 "Encimar (Figura 6) (Figura 6)"

funCambio :: a -> Dibujo a
funCambio a = rotar (figura a)

testChange :: Test
testChange =
    TestCase $ assertEqual "change" (show $ change funCambio (rotar (figura Triangulo)))
                 "Rotar (Rotar (Figura Triangulo))"

testfoldDib :: Test
testfoldDib =
  TestCase $ assertEqual
    "foldDib"
    1
      (foldDib
        (const 1)
        id
        id
        id
        (\_ _ x y -> x * y)
        (\_ _ x y -> x * y)
        (+)
        (apilar 1.0 1.0 
          (juntar 1.0 1.0 (figura Triangulo) (figura Rectangulo)) 
          (rotar (rotar (figura Rectangulo)))
        )
      )

-- Test de identidad en Dibujo.hs
testId :: Test
testId =
    TestCase $ assertEqual "id" (show $ mapDib id (rotar (espejar (figura 1)))) "Rotar (Espejar (Figura 1))"

tests :: Test
tests =
                TestList
                    [ TestLabel "testFigura" testFigura
                    , TestLabel "testRotar" testRotar
                    , TestLabel "testApilar" testApilar
                    , TestLabel "testEncimar" testEncimar
                    , TestLabel "testRot45" testRot45
                    , TestLabel "testJuntar" testJuntar
                    , TestLabel "testEspejar" testEspejar
                    , TestLabel "testR180" testR180
                    , TestLabel "testR270" testR270
                    , TestLabel "testUpUpUp" testUpUpUp
                    , TestLabel "testSlashSlashSlash" testSlashSlashSlash
                    , TestLabel "testDotDotDot" testDotDotDot
                    , TestLabel "testComp" testComp
                    , TestLabel "testEncimar4" testEncimar4
                    , TestLabel "testCuarteto" testCuarteto
                    , TestLabel "testCiclar" testCiclar
                    , TestLabel "testMapDib" testMapDib
                    , TestLabel "testChange" testChange
                    , TestLabel "testfoldDib" testfoldDib
                    , TestLabel "testId" testId
                    ]

main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess