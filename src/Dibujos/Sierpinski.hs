module Dibujos.Sierpinski (
    interpBas, testAll,
    sierpinskiConf
)where

import Dibujo (Dibujo, figura, juntar, apilar, rotar, encimar)
import FloatingPic(Conf(..), Output, half)
import qualified Graphics.Gloss.Data.Point.Arithmetic as V
import Graphics.Gloss ( Picture, blue, color, line, violet, withAlpha)

-- Grado del dibujo de Sierpinski
-- Debe ser Mayor o Igual a 1 (10 Recomendado)
-- ⚠UTILIZAR UN NUMERO MAYOR A 10 PUEDE TRABARLO O TARDAR EN CARGAR⚠
-- En dicho caso cerrar la consola para cerrar la ventana de Sierpinski
grado :: Int
grado = 10

-- Les ponemos colorcitos para que no sea _tan_ feo
data Color = Azul | Violeta | Transparente
    deriving (Show, Eq)

data BasicaSinColor = Rectangulo | Triangulo
    deriving (Show, Eq)

colorear :: Color -> Picture -> Picture
colorear Azul = color blue
colorear Transparente = color (withAlpha 0 blue)
colorear Violeta = color violet

interpBasicaSinColor :: Output BasicaSinColor
interpBasicaSinColor Rectangulo x y w = line [x, x V.+ y, x V.+ y V.+ w, x V.+ w, x]
interpBasicaSinColor Triangulo x y w = line $ map (x V.+) [(0,0), y V.+ half w, w, (0,0)]

-- -- Coloreo de Sierpinski:
-- Tipo de Sierpinski
type Sierpinski = (BasicaSinColor, Color)

interpBas :: Output Sierpinski
interpBas (b, c) x y w = colorear c $ interpBasicaSinColor b x y w

figVioleta :: BasicaSinColor -> Dibujo Sierpinski
figVioleta b = figura (b, Violeta)

figAzul :: BasicaSinColor -> Dibujo Sierpinski
figAzul b = figura (b, Azul)

figTransparente :: BasicaSinColor -> Dibujo Sierpinski
figTransparente b = figura (b, Transparente)

-- -- Armado de Sierpinski:
-- Forma estructura triangular con la figura dada
piramidal :: Dibujo Sierpinski -> Dibujo Sierpinski
piramidal d = apilar 1 1 (juntar (1/3) 1 (figTransparente Rectangulo)
                                         (juntar 1 0.5 d (figTransparente Rectangulo)))
                         (juntar 1 1 d d)

-- Funcion recursiva de Sierpinski
sierpinskiRecursive :: Int -> Dibujo Sierpinski
sierpinskiRecursive 1 = rotar (figVioleta Triangulo)
sierpinskiRecursive n =  piramidal (sierpinskiRecursive (n-1))

-- Encuadra Sierpinski
figCuadro :: Dibujo Sierpinski
figCuadro = encimar (figAzul Rectangulo) (sierpinskiRecursive grado)
-- -- Impresion de Sierpinski:

testAll :: Dibujo Sierpinski
testAll = figCuadro

sierpinskiConf :: Conf
sierpinskiConf = Conf {
    name = "Sierpinski",
    pic = testAll,
    bas = interpBas
}
