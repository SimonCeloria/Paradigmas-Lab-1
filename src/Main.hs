module Main (main) 
where

--import Dibujos.Ejemplo (ejemploConf)
import Dibujos.Feo (feoConf)
import Dibujos.Grilla (grillaConf)
import Dibujos.Escher (escherConf)
import Dibujos.Sierpinski (sierpinskiConf)
import Dibujos.Alfombra (alfombraConf)
import Dibujos.Prueba (pruebaConf)

--import Dibujos.Cuadrados(cuadConf)
import FloatingPic (Conf (..))
import Interp (initial)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import Control.Monad (when)
import InterpHaha (ConfH, simpleHaha, initialH')
import InterpSVG (ConfSVG, initialSVG', simpleSVG)

-- Lista de configuraciones de los dibujos
configs :: [Conf]
configs = [feoConf, grillaConf, escherConf, sierpinskiConf, alfombraConf, pruebaConf] --[ejemploConf, feoConf,cuadConf 3]

configsH :: [ConfH]
configsH = map (\(Conf n p _) -> simpleHaha n p) configs

configsSVG :: [ConfSVG]
configsSVG = map (\(Conf n p _) -> simpleSVG n p) configs

-- Dibuja el dibujo n
initial' :: [Conf] -> String -> IO ()
initial' [] n = do
  putStrLn $ "No hay un dibujo llamado " ++ n
initial' (c : cs) n =
  if n == name c
    then
      initial c 800
    else
      initial' cs n

main :: IO ()
main = do
  args <- getArgs
  when (length args > 2 || null args) $ do
    putStrLn "Se debe elegir exactamente un dibujo. Para ver los dibujos use --lista ."
    exitFailure
  when (head args == "--lista") $ do
    putStrLn "Los dibujos disponibles son:"
    mapM_ (putStrLn . name) configs
    putStrLn "\nQue dibujo desea ejecutar ?"
    drawing <- getLine
    initial' configs $ drawing
    exitSuccess
  when (head args == "-a" && not (null $ tail args)) $ do
    initialH' configsH (args!!1) 
    exitSuccess
  when (head args == "-s" && not (null $ tail args)) $ do
    initialSVG' configsSVG (args!!1) 
    exitSuccess
  initial' configs $ head args