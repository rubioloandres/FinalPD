module Procesador.Procesar where

import Text.CSV
import Parser.ParserCSV
import Interpolador.Interpolar    
import Data.Char
import Data.List
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Maybe
import Text.Parsec.Error

data MonedaProcesada = MonedaProcesada 
                        { nombre :: String
                        , ultimaCotizacion :: Double
                        , fechaCotizacionFutura :: Double
                        , cotizacionFutura :: Double
                        , variacionCotizacion :: Double
                        , porcentajeVariacionCotizacion :: Double
                        , polinomio :: String
                        , cotizaciones :: [(Double,Double)]
                        , cotizacionesFuturas :: [(Double,Double)]
                        } deriving (Eq, Show, Read, Ord)

mesesDeAño :: [(String, Double)]
mesesDeAño =
            [("ene",0)
            ,("feb",1)
            ,("mar",2)
            ,("abr",3)
            ,("may",4)
            ,("jun",5)
            ,("jul",6)
            ,("ago",7)
            ,("sep",8)
            ,("oct",9)
            ,("nov",10)
            ,("dic",11)
            ]    

------------------------------------------------------------------------------
procesarEntradas :: [(FilePath, String)] -> [Either Text.Parsec.Error.ParseError CSV]
procesarEntradas pathsEntradas = map (\ pathEntrada -> ( parseCSV (fst pathEntrada) (snd pathEntrada) ) ) pathsEntradas

-- dado un mes y año, y una lista de datos de cotizaciones, devuelve un array de monedas procesadas
procesarMonedas :: String -> String -> [(String, Either a [[String]])] -> [MonedaProcesada]
procesarMonedas nomMes año listaDatos = catMaybes (map (\dato -> procesarMoneda nomMes año dato ) listaDatos)

-- dado un mes y año, y un dato de cotizacion, devuelve una moneda procesada
procesarMoneda :: String -> String -> (String, Either a [[String]]) -> Maybe MonedaProcesada
procesarMoneda nomMes año dato = case (snd dato) of
  Left _ -> Nothing
  Right csv ->  do
    let datosMoneda = (procesarDatos (fst dato) csv nomMes año)
    Just datosMoneda
 
-- dado un array de monedas procesadas, ordena el array en base al cambio de cotizaciones
-- siendo la primera la de mayor variacion positiva (mejor inversion)    
ordenarVariacionesDeCotizaciones :: [MonedaProcesada] -> [MonedaProcesada]
ordenarVariacionesDeCotizaciones monedas = reverse (sortBy (comparing porcentajeVariacionCotizacion) monedas)
------------------------------------------------------------------------------

-- dado el nombre de una moneda, un csv, un mes y un año
-- devuelve una MonedaProcesada, con los datos necesarios para su analisis y comparacion  
procesarDatos :: String -> [[String]] -> String -> String -> MonedaProcesada
procesarDatos moneda csv nomMes año =
    let numeroMes = buscarNumeroMes nomMes
        añoDeInicio = obtenerAñoDeInicio csv
        listaPuntos = crearPuntos csv
        ultimoPunto = obtenerUltimoPunto listaPuntos
        listaVariaciones = obtenerVariaciones listaPuntos
        promedioDeVariaciones = promedio listaVariaciones
        ultimaCotizacion = obtenerUltimaCotizacion csv
        añosFuturo = 8
        puntosFuturos = crearPuntosAñoFuturo añosFuturo ultimoPunto promedioDeVariaciones ultimaCotizacion
        puntosAInterpolar = (listaPuntos ++ puntosFuturos)
        polinomio = generarPolinomioInterpolante puntosAInterpolar
        numeroMesAEstimar = obtenerNumeroMesAEstimar numeroMes (toInt año) añoDeInicio 
        cotizacionFutura = redondear4Decimales (interpolarLagrange puntosAInterpolar numeroMesAEstimar)
        cotizacionActual = obtenerUltimaCotizacion csv
        cambioEnCotizacion = redondear4Decimales (obtenerVariacionCotizacion cotizacionFutura cotizacionActual)
        porcentajeCambioEnCotizacion = redondear2Decimales (porcentajeVariacion cambioEnCotizacion cotizacionActual)
        listaCotizacionesPorAño = convertirPuntosRegistrosEnAños listaPuntos
        listaCotizacionesFuturas = convertirPuntosRegistrosEnAños ([(last listaPuntos)] ++ (take 3 puntosFuturos)) 

    in MonedaProcesada { nombre = moneda
                       , ultimaCotizacion = cotizacionActual
                       , fechaCotizacionFutura = (numeroMesAEstimar / 12 ) + 2003
                       , cotizacionFutura = cotizacionFutura
                       , variacionCotizacion = cambioEnCotizacion
                       , porcentajeVariacionCotizacion = porcentajeCambioEnCotizacion
                       , polinomio = polinomio
                       , cotizaciones = listaCotizacionesPorAño
                       , cotizacionesFuturas = listaCotizacionesFuturas
                       }                                                                                           

------------------------------------------------------------------------------    

-- dado un mes 'MMM' nos devuelve un numero de mes
buscarNumeroMes :: String -> Maybe Double
buscarNumeroMes nombreMes = encontrarMes nombreMes mesesDeAño

-- dado un mes y una lista de meses, devuelve el numero de mes correspondiente
encontrarMes :: String -> [ (String, Double) ] -> Maybe Double
encontrarMes nombreMes = foldr (\(nomMes,numMes) acc -> if  (lowerString nombreMes) == nomMes  then Just numMes else acc) Nothing

-- convierte a minuscula un string
lowerString :: String -> String
lowerString = map toLower

------------------------------------------------------------------------------

-- dada una lista de puntos, devuelve el ultimo (representa un mes)
obtenerUltimoPunto :: Foldable t => t a -> Int
obtenerUltimoPunto lista = ((length lista) - 1) * 12

------------------------------------------------------------------------------

-- dado un punto, devuelve la segunda componente (cotizacion)
obtenerCotizacionDesdePunto :: (a, b) -> b
obtenerCotizacionDesdePunto punto = snd punto

-- dada una lista de puntos (mes,cotizacion), devuelve un array de variaciones de cotizaciones
obtenerVariaciones :: [(a, Double)] -> [Double]
obtenerVariaciones listaPuntos = calcularVariaciones (map (obtenerCotizacionDesdePunto) listaPuntos)

-- dada una lista de cotizaciones, devuelve un array de las variaciones entre las cotizaciones     
calcularVariaciones :: [Double] -> [Double]    
calcularVariaciones listaCotizaciones = tail ( reverse (map (\ cotizacion -> ( aplicarResta cotizacion listaCotizaciones) ) listaCotizaciones) )

-- dada una cotizacion y una lista de cotizaciones, devuelve la resta entre la cotizacion dada y la siguiente
aplicarResta :: Double -> [Double] -> Double
aplicarResta cotizacionActual listaCotizaciones = case (lookup cotizacionActual $ (zip <*> tail) listaCotizaciones) of
    Just cotizacionSiguiente -> cotizacionSiguiente - cotizacionActual
    Nothing -> 0

------------------------------------------------------------------------------

-- dada una lista, devuelve su promedio    
promedio :: (Fractional a1, Real a2) => [a2] -> a1
promedio lista = realToFrac (sum lista) / genericLength lista

-- dada una cantidad de años, un ultimo punto de cotizacion, el promedio de variaciones, y la ultima cotizacion registrada
-- devuelve un array de puntos (mesFuturo,cotizacion)
crearPuntosAñoFuturo :: Integral a => Double -> a -> Double -> Double -> [(Double, Double)]
crearPuntosAñoFuturo cantidadAños ultimoPunto promedioDeVariaciones ultimaCotizacion = puntosAñoFuturo (generarArrayDeAños cantidadAños) ultimoPunto promedioDeVariaciones ultimaCotizacion

-- dada una cantidad, genera un array de elementos desde 1 hasta la cantidad proporcionada
generarArrayDeAños :: (Ord a, Num a, Enum a) => a -> [a]
generarArrayDeAños cantidadAños = takeWhile (<= cantidadAños) [1..]

-- dada una lista de años futuros (a estimar), el ultimo punto de cotizacion generado, el promedio de variaciones calculado
-- y la ultima cotizacion registrada, devuelve un array de puntos (x,y)
-- x = representa un mes futuro
-- y = representa una cotizacion para el mes futuro
puntosAñoFuturo :: Integral a => [Double] -> a -> Double -> Double -> [(Double, Double)]
puntosAñoFuturo listaAñosFuturos ultimoPunto promedioDeVariaciones ultimaCotizacion = map (\ año -> puntoAñoFuturo año ultimoPunto promedioDeVariaciones ultimaCotizacion) listaAñosFuturos

-- dado un año futuro, el ultimo punto de cotizacion, el promedio de variaciones, y la ultima cotizacion registrada
-- devuelve un punto (mes,cotizacion)
puntoAñoFuturo :: Integral a => Double -> a -> Double -> Double -> (Double, Double)
puntoAñoFuturo añoFuturo ultimoPunto promedioVariciones ultimaCotizacion = do
  let punto = (fromIntegral ultimoPunto) + (12 * añoFuturo)
  let res = ((12 * añoFuturo)* promedioVariciones) + ultimaCotizacion
  if (res :: Double) > 0
    then ( punto, res )
    else ( punto, abs ( ultimaCotizacion / ((12 * añoFuturo) * promedioVariciones) ) )

------------------------------------------------------------------------------

-- dado un array de puntos, devuelve un string que representa el polinomio interpolante para los puntos proporcionados
generarPolinomioInterpolante :: Interpolador.Interpolar.PuntosDeInterpolacion Double -> String
generarPolinomioInterpolante puntos = calcularPolinomio puntos

------------------------------------------------------------------------------

-- dado un mes, un año y el año inicial del cual se tienen registros
-- devuelve el numero de mes correspondiente (necesario para interpolar)
-- numeroFecha = (12 * cantidadDeAños) + numeroMes
-- cantidadDeAños = añoIngresado - añoInicial
obtenerNumeroMesAEstimar :: Num p => Maybe p -> p -> p -> p
obtenerNumeroMesAEstimar mes año añoDeInicio =
  case mes of
  Nothing   -> 0
  Just numMes  -> (12 * (año - añoDeInicio)) + numMes

-- dada una cotizacion futura y una cotizacion actual, devuelve su diferencia
obtenerVariacionCotizacion :: Double -> Double -> Double
obtenerVariacionCotizacion cotizacionFutura cotizacionActual = cotizacionFutura - cotizacionActual

-- dada un cambio de cotizacion y la cotizacion actual, devuelve el porcentaje de cambio
porcentajeVariacion :: Fractional a => a -> a -> a
porcentajeVariacion variacion cotizacionActual = (variacion * 100) / cotizacionActual

-- dado un array de puntos, devuelve un array de puntos (x,y) cambiando la primer componente de cada uno
-- x = representa un mes
-- y = representa una cotizacion para ese mes
convertirPuntosRegistrosEnAños :: Fractional a => [(a, b)] -> [(a, b)]
convertirPuntosRegistrosEnAños puntos = map (\ (x,y) -> ( (x / 12 ) + 2003, y ) ) puntos

------------------------------------------------------------------------------
-- AUXILIARES

mostrarPolinomio :: Show a => a -> IO ()
mostrarPolinomio polinomio = print polinomio

verPolinomio :: IO ()
verPolinomio = calcularPolinomio2

-- dado un string, devuelve un entero
toInt :: Read a => String -> a
toInt string = read string

calcularCotizacion :: [Record] -> Double -> Double    
calcularCotizacion csv numMes = obtenerResultado (crearPuntos csv) numMes
