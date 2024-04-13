module Dibujos.Prueba (
    interpBas, testAll,
    pruebaConf
)where

import Dibujo (Dibujo, figura, juntar, apilar, rotar, encimar, rot45)
import FloatingPic(Conf(..), Output, half, zero)
import qualified Graphics.Gloss.Data.Point.Arithmetic as V
import Graphics.Gloss ( Picture, black, red, color, Picture(ThickCircle), Picture(Translate), withAlpha, Picture(Polygon))

-- Grado del dibujo de Prueba
-- Debe ser Mayor o Igual a 1 (7 Recomendado)
-- ⚠UTILIZAR UN NUMERO MAYOR A 7 PUEDE TRABARLO O TARDAR EN CARGAR⚠
-- En dicho caso cerrar la consola para cerrar la ventana de Prueba
grado :: Int
grado = 2

-- Les ponemos colorcitos para que no sea _tan_ feo
data Color = Black | Rojo | Transparente
    deriving (Show, Eq)

data BasicaSinColor = Rectangulo | Circulo
    deriving (Show, Eq)

colorear :: Color -> Picture -> Picture
colorear Black = color black
colorear Transparente = color (withAlpha 0 black)
colorear Rojo = color red

-- -- Seria Curioso Utilizar Circulos o hacer Hexagonos o formas Raras ... pero todo un tema
-- En especial con los circulos porque no usan vectores, solo 2 floats...
interpBasicaSinColor :: Output BasicaSinColor
interpBasicaSinColor Circulo (x,x1) (y,y1) (z,z1) = Translate x x1 $ ThickCircle  (y + y1) 5
interpBasicaSinColor Rectangulo x y w = Polygon [x, x V.+ y, x V.+ y V.+ w, x V.+ w, x]

-- -- Coloreo de Prueba:
-- Tipo de Prueba
type Prueba = (BasicaSinColor, Color)

interpBas :: Output Prueba
interpBas (b, c) x y w = colorear c $ interpBasicaSinColor b x y w

figRoja :: BasicaSinColor -> Dibujo Prueba
figRoja b = figura (b, Rojo)

figNegra :: BasicaSinColor -> Dibujo Prueba
figNegra b = figura (b, Black)

figTransparente :: BasicaSinColor -> Dibujo Prueba
figTransparente b = figura (b, Transparente)

-- -- Armado de Prueba:
-- Forma estructura triangular con la figura dada
encuadrar :: Dibujo Prueba -> Dibujo Prueba
encuadrar d = apilar 1 0.5 (apilar 1 1 (juntar 0.5 1 d (juntar 1 1 d d)) 
                                     (juntar 0.5 1 d (juntar 1 1 (figTransparente Rectangulo) d)))
                         (juntar 0.5 1 d (juntar 1 1 d d))

-- Funcion recursiva de Prueba
pruebaRecursive :: Int -> Dibujo Prueba
pruebaRecursive 1 = figRoja Circulo
pruebaRecursive n =  encuadrar (pruebaRecursive (n-1))

-- Encuadra Prueba
figCuadro :: Dibujo Prueba
figCuadro = encimar (figNegra Rectangulo) (pruebaRecursive grado)
-- -- Impresion de Prueba:

testAll :: Dibujo Prueba
testAll = figCuadro;

pruebaConf :: Conf
pruebaConf = Conf {
    name = "Prueba",
    pic = testAll,
    bas = interpBas
}
