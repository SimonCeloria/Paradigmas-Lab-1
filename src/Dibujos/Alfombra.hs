module Dibujos.Alfombra (
    interpBas, testAll,
    alfombraConf
)where

import Dibujo (Dibujo, figura, juntar, apilar, rotar, encimar, rot45)
import FloatingPic(Conf(..), Output, half)
import qualified Graphics.Gloss.Data.Point.Arithmetic as V
import Graphics.Gloss ( Picture, black, red, color, Picture(Polygon),withAlpha)

-- Grado del dibujo de Alfombra
-- Debe ser Mayor o Igual a 1 (7 Recomendado)
-- ⚠UTILIZAR UN NUMERO MAYOR A 7 PUEDE TRABARLO O TARDAR EN CARGAR⚠
-- En dicho caso cerrar la consola para cerrar la ventana de Alfombra
grado :: Int
grado = 6

-- Les ponemos colorcitos para que no sea _tan_ feo
data Color = Black | Rojo | Transparente
    deriving (Show, Eq)

data BasicaSinColor = Rectangulo
    deriving (Show, Eq)

colorear :: Color -> Picture -> Picture
colorear Black = color black
colorear Transparente = color (withAlpha 0 black)
colorear Rojo = color red

-- A diferencia de en los otros dibujos, aca se usan poligonos en lugar de lineas
-- Esto es porque con lineas resulta indistingibles las casillas con o sin cuadrados
-- Al menos para casos chicos, puesto que en los grandes se aglutinan las lineas.
interpBasicaSinColor :: Output BasicaSinColor
interpBasicaSinColor Rectangulo x y w = Polygon  [x, x V.+ y, x V.+ y V.+ w, x V.+ w, x]

-- -- Coloreo de Alfombra:
-- Tipo de Alfombra
type Alfombra = (BasicaSinColor, Color)

interpBas :: Output Alfombra
interpBas (b, c) x y w = colorear c $ interpBasicaSinColor b x y w

figRoja :: BasicaSinColor -> Dibujo Alfombra
figRoja b = figura (b, Rojo)

figNegra :: BasicaSinColor -> Dibujo Alfombra
figNegra b = figura (b, Black)

figTransparente :: BasicaSinColor -> Dibujo Alfombra
figTransparente b = figura (b, Transparente)

-- -- Armado de Alfombra:
-- Forma estructura triangular con la figura dada
encuadrar :: Dibujo Alfombra -> Dibujo Alfombra
encuadrar d = apilar 1 0.5 (apilar 1 1 (juntar 0.5 1 d (juntar 1 1 d d)) 
                                     (juntar 0.5 1 d (juntar 1 1 (figTransparente Rectangulo) d)))
                         (juntar 0.5 1 d (juntar 1 1 d d))

-- Funcion recursiva de Alfombra
alfombraRecursive :: Int -> Dibujo Alfombra
alfombraRecursive 1 = figRoja Rectangulo
alfombraRecursive n =  encuadrar (alfombraRecursive (n-1))

-- Encuadra Alfombra
figCuadro :: Dibujo Alfombra
figCuadro = encimar (figNegra Rectangulo) (alfombraRecursive grado)
-- -- Impresion de Alfombra:

testAll :: Dibujo Alfombra
testAll = figCuadro

alfombraConf :: Conf
alfombraConf = Conf {
    name = "Alfombra",
    pic = testAll,
    bas = interpBas
}
