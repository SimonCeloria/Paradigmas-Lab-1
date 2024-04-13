module Dibujo (Dibujo,
  comp, figura,
  encimar, apilar,
  juntar, rot45,
  rotar, espejar,
  (^^^),(///),
  (.-.), r90,
  r180, r270,
  encimar4, cuarteto,
  ciclar, mapDib,
  change, foldDib,
    -- agregar las funciones constructoras
  ) where
      
--import Data.Monoid (Ap(Ap))


-- nuestro lenguaje 
data Dibujo a = Figura a | Rotar (Dibujo a) 
              | Espejar (Dibujo a) | Rot45 (Dibujo a) 
              | Apilar Float Float (Dibujo a) (Dibujo a)
              | Juntar Float Float (Dibujo a) (Dibujo a)
              | Encimar (Dibujo a) (Dibujo a)
              deriving(Eq,Show)

-- combinadores
infixr 6 ^^^

infixr 7 .-.

infixr 8 ///

comp :: Int -> (a -> a) -> a -> a
comp 0 _ a = a
comp n f a = f ( comp (n-1) f a)


-- Funciones constructoras
figura :: a -> Dibujo a
figura = Figura

encimar :: Dibujo a -> Dibujo a -> Dibujo a
encimar = Encimar

apilar :: Float -> Float -> Dibujo a -> Dibujo a -> Dibujo a
apilar = Apilar

juntar  :: Float -> Float -> Dibujo a -> Dibujo a -> Dibujo a
juntar = Juntar

rot45 :: Dibujo a -> Dibujo a
rot45 = Rot45

rotar :: Dibujo a -> Dibujo a
rotar = Rotar


espejar :: Dibujo a -> Dibujo a
espejar = Espejar

(^^^) :: Dibujo a -> Dibujo a -> Dibujo a
(^^^) = encimar

(.-.) :: Dibujo a -> Dibujo a -> Dibujo a
(.-.) = apilar 1 1

(///) :: Dibujo a -> Dibujo a -> Dibujo a
(///) = juntar 1 1 

-- rotaciones
r90 :: Dibujo a -> Dibujo a
r90 = comp 2 rot45

r180 :: Dibujo a -> Dibujo a
r180 = comp 2 r90

r270 :: Dibujo a -> Dibujo a
r270 = comp 3 r90

-- una figura repetida con las cuatro rotaciones, superimpuestas.
encimar4 :: Dibujo a -> Dibujo a
encimar4 dib = (^^^) dib ((^^^) (r90 dib) ((^^^) (r180 dib) (r270 dib))) 

-- cuatro figuras en un cuadrante.
cuarteto :: Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a
cuarteto x y z w = (.-.) ((///) x y) ((///) z w)

-- un cuarteto donde se repite la imagen, rotada (¡No confundir con encimar4!)
ciclar :: Dibujo a -> Dibujo a
ciclar fig = cuarteto fig (rotar fig) (r180 fig) (r270 fig) -- Imprimo 4 figuras y despues roto cada figura segun el angulo

--mapDib :: (a ->  b) -> Dibujo a -> Dibujo b
mapDib :: (a -> b) -> Dibujo a -> Dibujo b
mapDib f (Figura dib) = Figura (f dib)
mapDib f (Rotar dib) = Rotar (mapDib f dib)
mapDib f (Espejar dib) = Espejar (mapDib f dib)
mapDib f (Rot45 dib) = Rot45 (mapDib f dib)
mapDib f (Apilar num1 num2 dib1 dib2) = Apilar num1 num2 (mapDib f dib1) (mapDib f dib2)
mapDib f (Juntar num1 num2 dib1 dib2) = Juntar num1 num2 (mapDib f dib1) (mapDib f dib2)
mapDib f (Encimar dib1 dib2) = Encimar (mapDib f dib1) (mapDib f dib2)
-- verificar que las operaciones satisfagan
-- 1. map figura = id
-- 2. map (g . f) = mapDib g . mapDib f
-- nuestro lenguaje 
-- data Dibujo a = Figura a | Rotar (Dibujo a) 
--               | Espejar (Dibujo a) | Rot45 (Dibujo a) 
--               | Apilar Float Float (Dibujo a) (Dibujo a)
--               | Juntar Float Float (Dibujo a) (Dibujo a)
--               | Encimar (Dibujo a) (Dibujo a)
--               deriving(Eq,Show)

-- Cambiar todas las básicas de acuerdo a la función.
change :: (a -> Dibujo b) -> Dibujo a -> Dibujo b
change f (Figura dib) = f dib
change f (Rotar dib) = Rotar (change f dib)
change f (Espejar dib) = Espejar (change f dib)
change f (Rot45 dib) = Rot45 (change f dib)
change f (Apilar num1 num2 dib1 dib2) = Apilar num1 num2 (change f dib1) (change f dib2)
change f (Juntar num1 num2 dib1 dib2) = Juntar num1 num2 (change f dib1) (change f dib2)
change f (Encimar dib1 dib2) = Encimar (change f dib1) (change f dib2)


-- Principio de recursión para Dibujos.
foldDib ::
  (a -> b) ->
  (b -> b) ->
  (b -> b) ->
  (b -> b) ->
  (Float -> Float -> b -> b -> b) ->
  (Float -> Float -> b -> b -> b) ->
  (b -> b -> b) ->
  Dibujo a ->
  b
foldDib fFigura _ _ _ _ _ _ (Figura a) = fFigura a
foldDib fFigura fRotar fEsp fRotar45 fApi fJun fEnc (Rotar a) = fRotar (foldDib fFigura fRotar fEsp fRotar45 fApi fJun fEnc a)
foldDib fFigura fRotar fEsp fRotar45 fApi fJun fEnc (Espejar a) = fEsp (foldDib fFigura fRotar fEsp fRotar45 fApi fJun fEnc a)
foldDib fFigura fRotar fEsp fRotar45 fApi fJun fEnc (Rot45 a) = fRotar45 (foldDib fFigura fRotar fEsp fRotar45 fApi fJun fEnc a)
foldDib fFigura fRotar fEsp fRotar45 fApi fJun fEnc (Apilar x y dib1 dib2) = fApi x y (foldDib fFigura fRotar fEsp fRotar45 fApi fJun fEnc dib1)
                                                                                      (foldDib fFigura fRotar fEsp fRotar45 fApi fJun fEnc dib2)
foldDib fFigura fRotar fEsp fRotar45 fApi fJun fEnc (Juntar x y dib1 dib2) = fJun x y (foldDib fFigura fRotar fEsp fRotar45 fApi fJun fEnc dib1)
                                                                                      (foldDib fFigura fRotar fEsp fRotar45 fApi fJun fEnc dib2)
foldDib fFigura fRotar fEsp fRotar45 fApi fJun fEnc (Encimar dib1 dib2) = fEnc (foldDib fFigura fRotar fEsp fRotar45 fApi fJun fEnc dib1)
                                                                               (foldDib fFigura fRotar fEsp fRotar45 fApi fJun fEnc dib2)                                                                            


