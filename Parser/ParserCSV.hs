module Parser.ParserCSV where

import Text.CSV

manejarError :: a -> IO ()
manejarError csv = putStrLn "error en parseo"

procesar :: Show a => [a] -> IO ()
procesar csv = mostrar (obtenerDatos csv)

mostrar :: Show a => a -> IO ()
mostrar csv = print csv

-- con esta funcion eliminamos el nombre de la columna (header de csv)
obtenerDatos :: [a] -> [a]
obtenerDatos csv = tail csv

mostrarPuntos :: [Record] -> IO ()
mostrarPuntos csv = print ( crearPuntos csv )

crearPuntos :: (Num a, Enum a) => [Record] -> [(a, Double)]
crearPuntos csv = zip [1..] (extraerColumna (obtenerDatos csv) 1 :: [Double])

extraerColumna :: Read t => CSV -> Int -> [t]
extraerColumna csv numeroDeColumna =
  [ read (columna !! numeroDeColumna) | columna <- csv
                       , length columna > numeroDeColumna
                       , columna /= [""] ]

ultimaCotizacion :: [Record] -> Double
ultimaCotizacion csv = last ( extraerColumna (obtenerDatos csv) 1 :: [Double] )



parsear :: IO ()
parsear = do

  let nombreDeArchivo = "cotizacionDolar2019.csv"
  entrada <- readFile nombreDeArchivo
  
  let csv = parseCSV nombreDeArchivo entrada 
  either manejarError procesar csv
  
  let datosCSV = parseCSV nombreDeArchivo entrada
  either manejarError mostrarPuntos datosCSV
