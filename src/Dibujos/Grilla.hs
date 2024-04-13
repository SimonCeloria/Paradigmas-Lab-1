module Dibujos.Grilla (
    grilla,
    interpCoord, 
    paintNums,
    grillaConf
)where

import Dibujo (Dibujo, juntar, apilar, figura)
import FloatingPic (Conf(..), Output)

import Graphics.Gloss ( text, scale, translate )
import GHC.Float (int2Float)

type Coord = (Int, Int)

interpCoord :: Output Coord
interpCoord (x,y) _ _ _= translate (int2Float (x*100 + 35)) (int2Float ((7-y)*100 +45)) $ scale 0.1 0.1 $ text (show (x,y))

row :: [Dibujo a] -> Dibujo a
row [] = error "row: no puede ser vacío"
row [d] = d
row (d:ds) = juntar (fromIntegral $ length ds) 1 d (row ds)

column :: [Dibujo a] -> Dibujo a
column [] = error "column: no puede ser vacío"
column [d] = d
column (d:ds) = apilar (fromIntegral $ length ds) 1 d (column ds)

grilla :: [[Dibujo a]] -> Dibujo a
grilla = column . map row

paintNums :: Dibujo Coord
paintNums = grilla [[ figura (x, y) | y <- [0..7]] | x <- [7,6,5,4,3,2,1,0]]

grillaConf :: Conf
grillaConf = Conf {
    name = "Grilla",
    pic = paintNums,
    bas = interpCoord
}