module Parser.ParserCSV where

import Text.CSV

-- parsear :: IO ()
parsear = do

  let nombreDeArchivo = "cotizacionDolar2019.csv"
  entrada <- readFile nombreDeArchivo
  
  let csv = parseCSV nombreDeArchivo entrada
  either manejarError procesar csv
  
  let datosCSV = parseCSV nombreDeArchivo entrada
  either manejarError mostrarPuntos datosCSV
  
manejarError csv = putStrLn "error en parseo"

procesar csv = (mostrar.obtenerDatos) csv

mostrar csv = print csv
obtenerDatos csv = tail csv

mostrarPuntos csv = print ( crearPuntos csv )

crearPuntos csv = zip [1..] (extraerColumna (obtenerDatos csv) 1 :: [Float])

extraerColumna :: Read t => CSV -> Int -> [t]
extraerColumna csv numeroDeColumna =
  [ read (columna !! numeroDeColumna) | columna <- csv
                       , length columna > numeroDeColumna
                       , columna /= [""] ]
                       
ultimaCotizacion csv = last ( extraerColumna (obtenerDatos csv) 1 :: [Float] )