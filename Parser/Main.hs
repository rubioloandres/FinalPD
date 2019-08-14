module Main where

import Text.CSV

main :: IO ()
main = do
  let nombreDeArchivo = "cotizacionDolar2019.csv"
  entrada <- readFile nombreDeArchivo

  let csv = parseCSV nombreDeArchivo entrada
  either manejarError mostrar csv
manejarError csv = putStrLn "error en parseo"
mostrar csv = print csv
