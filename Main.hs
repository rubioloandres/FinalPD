module Main where

import Parser.ParserCSV
import Interpolador.Interpolar
import Procesador.Procesar
import Text.CSV

main :: IO ()
main = do
  putStrLn "Ingrese un mes en formato MMM: "
  nombreMes <- getLine
  putStrLn "Ingrese un año en formato AA: "
  año <- getLine

  let nombreDeArchivo1 = "pruebaDolar.csv"
--  let nombreDeArchivo1 = "cotizacionDolar.csv"
  entrada <- readFile nombreDeArchivo1
--  let nombreDeArchivo2 = "pruebaReal.csv"
--  entrada2 <- readFile nombreDeArchivo2

  let datosCSV1 = parseCSV nombreDeArchivo1 entrada
  case datosCSV1 of
    Left err -> print "error"
    Right csvParseado -> (procesarDatos "dolar" csvParseado nombreMes año) 

--  let datosCSV2 = parseCSV nombreDeArchivo2 entrada2
--  case datosCSV2 of
--    Left err -> print "error"
--    Right csvParseado -> (procesarDatos "real" csvParseado nombreMes año) 

