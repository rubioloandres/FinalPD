module Parser.ParserCSV where

import Text.CSV
import Data.List

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

--crearPuntos :: (Num a, Enum a) => [Record] -> [(a, Double)]
-- crearPuntos csv = zip [0..] ( extraerColumna (obtenerDatos csv) 1 :: [Double] ) 
crearPuntos csv = zip [0..] ( extraerColumna (obtenerDatos csv) 1 :: [Double] ) 

extraerColumna :: Read t => CSV -> Int -> [t]
extraerColumna csv numeroDeColumna =
  [ read (columna !! numeroDeColumna) | columna <- csv , length columna > numeroDeColumna , columna /= [""] ] 
  
-- extraerColumna :: Read t => CSV -> Int -> [t]
extraerCol csv numeroDeColumna =
  [ (columna !! numeroDeColumna) | columna <- csv , length columna > numeroDeColumna , columna /= [""] ]
--  [ (columna !! numeroDeColumna) | columna <- csv , length columna > numeroDeColumna , columna /= [""] ]   

ultimaCotizacion :: [Record] -> Double
ultimaCotizacion csv = last ( extraerColumna (obtenerDatos csv) 1:: [Double] )

obtenerAñoDeInicio csv =  read ( drop 4 (head ( extraerCol (obtenerDatos csv) 0 )) ) :: Double

--------------------------------------------------------
average xs = realToFrac (sum xs) / genericLength xs

-- la ultima resta no deberia hacerse
aplicarResta :: Double -> [Double] -> Double
aplicarResta x lista = case (lookup x $ (zip <*> tail) lista) of
    Just n -> n - x
    Nothing -> 0

-- variaciones lista = map (\ x -> ( aplicarResta x lista) ) lista
variaciones lista = tail ( reverse (map (\ x -> ( aplicarResta x lista) ) lista) )

ultimoPunto lista = (length lista) - 1

-- punto1AñoFuturo ultimoPunto promedioVariciones = (12 + ultimoPunto, 12 * promedioVariciones)
punto1AñoFuturo :: Int -> Double -> Double -> (Double,Double)
punto1AñoFuturo ultimoP promedioVariciones ultimaCotizacion = ( (fromIntegral ultimoP) + 12, (12 * promedioVariciones) + ultimaCotizacion)

punto6MesesFuturo ultimoP promedioVariciones ultimaCotizacion = ( (fromIntegral ultimoP) + 6, (6 * promedioVariciones) + ultimaCotizacion)

parsear :: IO ()
parsear = do

  let nombreDeArchivo = "cotizacionDolar2019.csv"
  entrada <- readFile nombreDeArchivo
  
  let csv = parseCSV nombreDeArchivo entrada 
  either manejarError procesar csv
  
  let datosCSV = parseCSV nombreDeArchivo entrada
  either manejarError mostrarPuntos datosCSV
