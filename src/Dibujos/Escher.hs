module Dibujos.Escher (
    interpBas, testAll,
    escherConf
)where

import Dibujo (Dibujo, figura, juntar, apilar, rotar, encimar, espejar, rot45, cuarteto)
import FloatingPic(Conf(..), Output)
import qualified Graphics.Gloss.Data.Point.Arithmetic as V
import Graphics.Gloss ( Picture, blue, color, line, violet, withAlpha )


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
interpBasicaSinColor Triangulo x y w = line $ map (x V.+) [(0,0), y, w, (0,0)]

-- -- Coloreo de Escher:
-- Tipo de Escher
type Escher = (BasicaSinColor, Color)

interpBas :: Output Escher
interpBas (b, c) x y w = colorear c $ interpBasicaSinColor b x y w


figAzul :: BasicaSinColor -> Dibujo Escher
figAzul b = figura (b, Azul)


figTransparente :: BasicaSinColor -> Dibujo Escher
figTransparente b = figura (b, Transparente)

-- -- Armado de Escher:

triangulo2 :: Dibujo Escher
triangulo2 = espejar (rot45 (figAzul Triangulo))

triangulo3 :: Dibujo Escher
triangulo3 = rotar (rotar (rotar triangulo2))

dibujoU:: Dibujo Escher
dibujoU = encimar (encimar triangulo2 (rotar triangulo2)) (encimar (rotar (rotar triangulo2)) (rotar (rotar (rotar triangulo2))) )

dibujoT:: Dibujo Escher
dibujoT = encimar (figAzul Triangulo) (encimar triangulo2 triangulo3)


lado:: Int -> Dibujo Escher
lado 1 = cuarteto (figTransparente Rectangulo) (figTransparente Rectangulo) (rotar dibujoT) dibujoT
lado 2 = cuarteto (lado 1) (lado 1) (rotar dibujoT) dibujoT
lado n = cuarteto (lado (n-1)) (lado (n-1)) (rotar dibujoT) dibujoT

esquina :: Int -> Dibujo Escher
esquina 1 = cuarteto (figTransparente Rectangulo) (figTransparente Rectangulo) (figTransparente Rectangulo) dibujoU
esquina 2 = cuarteto (esquina 1) (lado 1) (rotar (lado 1)) dibujoU
esquina n = cuarteto (esquina (n-1)) (lado (n-1)) (rotar (lado (n-1))) dibujoU

noneto :: Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a
noneto p q r s t u v w x = apilar 1 2 (juntar 1 2 p (juntar 1 1 q r)) 
    (apilar 1 1 (juntar 1 2 s (juntar 1 1 t u)) (juntar 1 2 v (juntar 1 1 w x)))

escher :: Int -> Dibujo Escher
escher n = noneto (esquina n) (lado n) (rotar (rotar (rotar (esquina n))))
    (rotar (lado n)) dibujoU (rotar (rotar (rotar (lado n))))
    (rotar (esquina n)) (rotar (rotar (lado n))) (rotar (rotar (esquina n)))

-- -- Impresion de Escher:

row :: [Dibujo a] -> Dibujo a
row [] = error "row: no puede ser vacío"
row [d] = d
row (d:ds) = juntar 1 (fromIntegral $ length ds) d (row ds)

column :: [Dibujo a] -> Dibujo a
column [] = error "column: no puede ser vacío"
column [d] = d
column (d:ds) = apilar 1 (fromIntegral $ length ds) d (column ds)

grilla :: [[Dibujo a]] -> Dibujo a
grilla = column . map row

testAll :: Dibujo Escher
testAll = grilla [
    [escher 2]
    ]

escherConf :: Conf
escherConf = Conf {
    name = "Escher",
    pic = testAll,
    bas = interpBas
}
