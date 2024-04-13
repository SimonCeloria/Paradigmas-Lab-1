module Pred (
  Pred,
  cambiar, anyDib, allDib, orP, andP, falla
) where
import Dibujo (Dibujo, figura, foldDib, change)

type Pred a = a -> Bool

-- Dado un predicado sobre básicas, cambiar todas las que satisfacen
-- el predicado por la figura básica indicada por el segundo argumento.
cambiar :: Pred a -> (a->Dibujo a) ->Dibujo a -> Dibujo a
cambiar p f= change (\a -> if p a then f a else figura a)           


-- Alguna básica satisface el predicado.
anyDib :: Pred a -> Dibujo a -> Bool
anyDib p = foldDib p id id id (\_ _ a b -> a||b) (\_ _ a b -> a||b) (||) 

-- Todas las básicas satisfacen el predicado.
allDib :: Pred  a -> Dibujo a -> Bool
allDib p = foldDib p id id id (\_ _ a b -> a&&b) (\_ _ a b -> a&&b) (&&)

-- Los dos predicados se cumplen para el elemento recibido.
andP :: Pred a -> Pred a -> a-> Bool
andP pa pb dib = pa dib && pb dib

-- Algún predicado se cumple para el elemento recibido.
orP :: Pred a -> Pred a -> a ->Bool
orP pa pb dib = pa dib || pb dib

falla ::Bool   --De momento lo defino asi para q vscode no me tire error xd
falla = True