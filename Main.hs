module Main where

import Parser.ParserCSV
import Interpolador.Interpolar
import Procesador.Procesar
import Text.CSV

import Text.Tabular.AsciiArt

main :: IO ()
main = do
  putStrLn "-------------------------------------------------------------------------"
  putStrLn "Ingrese un mes en formato MMM: "
  nombreMes <- getLine
  putStrLn "Ingrese un año en formato AA: "
  año <- getLine

  let nombreDeArchivo1 = "pruebaDolar.csv"
  let nombreDeArchivo2 = "pruebaReal.csv"
--  let nombreDeArchivo1 = "cotizacionDolar.csv"
  entrada <- readFile nombreDeArchivo1
  entrada2 <- readFile nombreDeArchivo2

  let datosCSV1 = parseCSV nombreDeArchivo1 entrada
  let datosCSV2 = parseCSV nombreDeArchivo2 entrada2

  case datosCSV1 of
    Left err -> print "error"
    Right csvParseado -> do
      let datosMoneda = (procesarDatos "Dolar" csvParseado nombreMes año)
      mostrarTabla ( datosMoneda !! 0) 
--      mostrarPoli ( polinomio ( datosMoneda !! 0) )

  case datosCSV2 of
    Left err -> print "error"
    Right csvParseado -> do
      let datosMoneda = (procesarDatos "Real" csvParseado nombreMes año)
      mostrarTabla (datosMoneda !! 0)    
