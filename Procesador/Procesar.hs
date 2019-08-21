module Procesador.Procesar where

import Text.CSV
import Parser.ParserCSV
import Interpolador.Interpolar    

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

procesarDatos :: [Char] -> [Record] -> [Char] -> [Char] -> IO ()
procesarDatos moneda csv nomMes año = do
    let numeroMes = buscarNumeroMes nomMes
    let añoDeInicio = obtenerAñoDeInicio csv
    print ( añoDeInicio )
    putStrLn "-----------------------------------------------------------"
    putStrLn ("Estimacion de cotizaciones para " ++ nomMes ++ " del 20" ++ año )
    putStrLn "-----------------------------------------------------------"
    print ("---Cotizacion "++ (moneda) ++" (a graficar)---")
    interpolar csv
    putStrLn "-----------------------------------------------------------"
    let valorDeX = encontrarXParaPolinomio numeroMes (toInt año) añoDeInicio
    print (valorDeX)
    print ("La cotizacion del "++ (moneda) ++" para la fecha deseada es: ")
    let cotizacionFutura = round4dp (calcularCotizacion csv valorDeX)
    putStrLn ( show (cotizacionFutura) )
    putStrLn "-----------------------------------------------------------"
    print ("Ultima cotizacion del "++ (moneda) ++": ")
    let cotizacionActual = ultimaCotizacion csv
    putStrLn ( show (cotizacionActual) )
    print ("Variacion de cotizacion del "++ (moneda) ++": ")
    let cambioEnCotizacion = round4dp (variacionCotizacion cotizacionFutura cotizacionActual)
    let porcentajeCambioEnCotizacion = round4dp (porcentajeVariacion cambioEnCotizacion cotizacionActual)
    putStrLn (show (cambioEnCotizacion) ++ " ( " ++ show (porcentajeCambioEnCotizacion) ++ "%)")
    putStrLn "-----------------------------------------------------------"

interpolar :: [Record] -> IO ()
interpolar csv = do 
    let listaPuntos = crearPuntos csv
    let ultimoP = ultimoPunto listaPuntos
    let listaVars = variaciones (map (\x -> snd x )  listaPuntos )
    let promVars = average listaVars
    let ultimaCot = ultimaCotizacion csv
    let puntoFut1A = punto1AñoFuturo ultimoP promVars ultimaCot
    let puntoFut6M = punto6MesesFuturo ultimoP promVars ultimaCot
    let pp = (listaPuntos ++ [puntoFut6M,puntoFut1A])
    print (pp)
    print ( calcularPolinomio (pp) )

buscarNumeroMes :: [Char] -> Maybe Double
buscarNumeroMes nombreMes = encontrarMes nombreMes mesesDeAño

encontrarMes :: (Eq nomMes) => nomMes -> [(nomMes,numMes)] -> Maybe numMes
encontrarMes nombreMes = foldr (\(nomMes,numMes) acc -> if nombreMes == nomMes then Just numMes else acc) Nothing

toInt :: Read a => String -> a
toInt string = read string

-- encontrarXParaPolinomio :: Num p => Maybe p -> p -> p
-- encontrarXParaPolinomio :: Maybe Double -> Double -> Int -> Double
-- numeroFecha = (12 * cantidadDeAños) + numeroMes
-- cantidadDeAños = añoIngresado - añoInicial


-- IMPORTANTE : calcular añoInicual a partir de datos de csv
encontrarXParaPolinomio mes año añoDeInicio =
    case mes of
    Nothing   -> 0
    Just numMes  -> (12 * (año - añoDeInicio)) + numMes

calcularCotizacion :: [Record] -> Double -> Double    
calcularCotizacion csv numMes = obtenerResultado (crearPuntos csv) numMes

verPoli :: IO ()
verPoli = calcularPolinomio2

variacionCotizacion :: Double -> Double -> Double
variacionCotizacion cotizacionFutura cotizacionActual = cotizacionFutura - cotizacionActual

porcentajeVariacion :: Fractional a => a -> a -> a
porcentajeVariacion variacion cotActual = (variacion * 100) / cotActual

---------------------------------------