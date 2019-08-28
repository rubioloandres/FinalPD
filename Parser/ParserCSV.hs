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

-- crearPuntos csv = zip [0..] ( extraerColumna (obtenerDatos csv) 1 :: [Double] ) 

crearPuntos csv = crearPuntosAnuales ([0..]) ( extraerColumna (obtenerDatos csv) 1 :: [Double] ) 

extraerColumna :: Read t => CSV -> Int -> [t]
extraerColumna csv numeroDeColumna =
  [ read (columna !! numeroDeColumna) | columna <- csv , length columna > numeroDeColumna , columna /= [""] ] 
  
-- extraerColumna :: Read t => CSV -> Int -> [t]
extraerCol csv numeroDeColumna =
  [ (columna !! numeroDeColumna) | columna <- csv , length columna > numeroDeColumna , columna /= [""] ]
--  [ (columna !! numeroDeColumna) | columna <- csv , length columna > numeroDeColumna , columna /= [""] ]   

obtenerUltimaCotizacion :: [Record] -> Double
obtenerUltimaCotizacion csv = last ( extraerColumna (obtenerDatos csv) 1:: [Double] )

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

--PARA MESES
-- ultimoPunto lista = (length lista) - 1

--PARA AÑOS
ultimoPunto lista = ((length lista) - 1) * 12

-- punto1AñoFuturo ultimoPunto promedioVariciones = (12 + ultimoPunto, 12 * promedioVariciones)
punto1AñoFuturo :: Int -> Double -> Double -> (Double,Double)
punto1AñoFuturo ultimoP promedioVariciones ultimaCotizacion = ( (fromIntegral ultimoP) + 12, (12 * promedioVariciones) + ultimaCotizacion)

punto2AñosFuturo ultimoP promedioVariciones ultimaCotizacion = ( (fromIntegral ultimoP) + 24, (24 * promedioVariciones) + ultimaCotizacion)

punto3AñosFuturo ultimoP promedioVariciones ultimaCotizacion = ( (fromIntegral ultimoP) + 36, (36 * promedioVariciones) + ultimaCotizacion)

punto4AñosFuturo ultimoP promedioVariciones ultimaCotizacion = ( (fromIntegral ultimoP) + 48, (48 * promedioVariciones) + ultimaCotizacion)

punto5AñosFuturo ultimoP promedioVariciones ultimaCotizacion = ( (fromIntegral ultimoP) + 60, (60 * promedioVariciones) + ultimaCotizacion)

punto6AñosFuturo ultimoP promedioVariciones ultimaCotizacion = ( (fromIntegral ultimoP) + 72, (72 * promedioVariciones) + ultimaCotizacion)

punto7AñosFuturo ultimoP promedioVariciones ultimaCotizacion = ( (fromIntegral ultimoP) + 84, (84 * promedioVariciones) + ultimaCotizacion)

punto8AñosFuturo ultimoP promedioVariciones ultimaCotizacion = ( (fromIntegral ultimoP) + 96, (96 * promedioVariciones) + ultimaCotizacion)

punto6MesesFuturo ultimoP promedioVariciones ultimaCotizacion = ( (fromIntegral ultimoP) + 6, (6 * promedioVariciones) + ultimaCotizacion)

--------------------------------------------------------------------
--METODOS PARA DATOS ANUALES

puntos p1 p2 = (p1 * 12, p2)

crearPuntosAnuales puntosX puntosCSV = zipWith (puntos) puntosX puntosCSV


parsear :: IO ()
parsear = do

  let nombreDeArchivo = "cotizacionDolar2019.csv"
  entrada <- readFile nombreDeArchivo
  
  let csv = parseCSV nombreDeArchivo entrada 
  either manejarError procesar csv
  
  let datosCSV = parseCSV nombreDeArchivo entrada
  either manejarError mostrarPuntos datosCSV
