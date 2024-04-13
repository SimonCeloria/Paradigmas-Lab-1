{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module Interp
  ( interp,
    initial,
    ov
  )
where

import Dibujo
import FloatingPic 
import Graphics.Gloss.Data.Vector
import Graphics.Gloss (Display (InWindow), color, display, makeColorI, pictures, translate, white, Picture)
import qualified Graphics.Gloss.Data.Point.Arithmetic as V

-- Dada una computación que construye una configuración, mostramos por
-- pantalla la figura de la misma de acuerdo a la interpretación para
-- las figuras básicas. Permitimos una computación para poder leer
-- archivos, tomar argumentos, etc.
initial :: Conf -> Float -> IO ()
initial (Conf n dib intBas) size = display win white $ withGrid fig size
  where
    win = InWindow n (ceiling size, ceiling size) (0, 0)
    fig = interp intBas dib (0, 0) (size, 0) (0, size)
    desp = -(size / 2)
    withGrid p x = translate desp desp $ pictures [p, color grey $ grid (ceiling $ size / 10) (0, 0) x 10]
    grey = makeColorI 100 100 100 100

-- isoseles :: FloatingPic
-- isoseles x y z = line $ map (x V.+) [half, raiz (raiz y), half (y V.+ z), cero]

-- rectangulo :: FloatingPic
-- rectangulo x y z = line $ map (x V.+) [cero, y, y V.+ z, z, cero]

sumdiv2 :: Vector -> Vector -> Vector
sumdiv2 x y = half(x V.+ y)

restdiv2 :: Vector -> Vector -> Vector
restdiv2 x y = half(x V.- y)

-- Interpretación de (^^^)
ov :: Picture -> Picture -> Picture
ov p q = pictures [p,q]

r45 :: FloatingPic -> FloatingPic
r45  fp x y z= fp (x V.+ sumdiv2 y z) (sumdiv2 y z) (restdiv2 z y)

rot :: FloatingPic -> FloatingPic
rot fp d w h= fp (d V.+ w) h ( V.negate w)

esp :: FloatingPic -> FloatingPic
esp fp d w h = fp (d V.+ w) (V.negate w) h

sup :: FloatingPic -> FloatingPic -> FloatingPic
sup fp gp d w h= pictures [fp d w h, gp d w h] 

jun :: Float -> Float -> FloatingPic -> FloatingPic -> FloatingPic
jun m n fp gp d w h = pictures [fp d w' h , gp (d V.+ w') (r' V.* w) h]
  where 
    w' = m/(m+n) V.* w
    r' = n/(m+n)

api :: Float -> Float -> FloatingPic -> FloatingPic -> FloatingPic
api m n fp gp d w h = pictures[fp (d V.+ h') w (r V.* h), gp d w h']
                    where r = m/(m+n)
                          r' = n/(m+n)
                          h' = r' V.* h

interp :: Output a -> Output (Dibujo a)
interp funcBase = foldDib funcBase rot esp r45 api jun sup