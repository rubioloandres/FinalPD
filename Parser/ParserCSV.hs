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

crearPuntos csv = crearPuntosAnuales ([0..]) ( extraerColumna (obtenerDatos csv) 1 :: [Double] ) 

extraerColumna :: Read t => CSV -> Int -> [t]
extraerColumna csv numeroDeColumna =
  [ read (columna !! numeroDeColumna) | columna <- csv , length columna > numeroDeColumna , columna /= [""] ] 
  
-- extraerColumna :: Read t => CSV -> Int -> [t]
extraerCol csv numeroDeColumna =
  [ (columna !! numeroDeColumna) | columna <- csv , length columna > numeroDeColumna , columna /= [""] ]

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

puntosAñoFuturo lista ultimoP promVars ultimaCot = map (\ año -> puntoAñoFuturo año ultimoP promVars ultimaCot) lista

puntoAñoFuturo añoFuturo ultimoP promedioVariciones ultimaCotizacion = do
  let punto = (fromIntegral ultimoP) + (12 * añoFuturo)
  let res = ((12 * añoFuturo)* promedioVariciones) + ultimaCotizacion
  if (res :: Double) > 0
    then (punto,res)
    else (punto, ((-ultimaCotizacion) / ((12 * añoFuturo) * promedioVariciones)))

--punto6MesesFuturo ultimoP promedioVariciones ultimaCotizacion = ( (fromIntegral ultimoP) + 6, (6 * promedioVariciones) + ultimaCotizacion)

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
