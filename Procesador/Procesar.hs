module Procesador.Procesar where

import Text.CSV
import Parser.ParserCSV
import Interpolador.Interpolar    
import Graphics.EasyPlot

import Text.Tabular.AsciiArt
import Text.Tabular as TT

data MonedaProcesada = MonedaProcesada { nombre :: String
                                       , ultimaCotizacion :: Double
                                       , cotizacionFutura :: Double
                                       , variacionCotizacion :: Double
                                       , porcentajeVariacionCotizacion :: Double
                                       , polinomio :: String
                                       } deriving (Eq, Show, Read)

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

-- procesarDatos :: [Char] -> [Record] -> [Char] -> [Char] -> IO ()
procesarDatos moneda csv nomMes año = do
    let numeroMes = buscarNumeroMes nomMes
    let añoDeInicio = obtenerAñoDeInicio csv
    let listaPuntos = crearPuntos csv
    let ultimoP = ultimoPunto listaPuntos
    let listaVars = variaciones (map (\x -> snd x )  listaPuntos )
    let promVars = average listaVars
    let ultimaCot = obtenerUltimaCotizacion csv
    let puntoFut1A = punto1AñoFuturo ultimoP promVars ultimaCot
    let puntoFut2A = punto2AñosFuturo ultimoP promVars ultimaCot
    let puntoFut3A = punto3AñosFuturo ultimoP promVars ultimaCot
    let puntoFut4A = punto4AñosFuturo ultimoP promVars ultimaCot
    let puntoFut5A = punto5AñosFuturo ultimoP promVars ultimaCot    
    let puntoFut6A = punto6AñosFuturo ultimoP promVars ultimaCot 
    let puntoFut7A = punto7AñosFuturo ultimoP promVars ultimaCot 
    let puntoFut8A = punto8AñosFuturo ultimoP promVars ultimaCot 
    let puntosAInterpolar = (listaPuntos ++ [puntoFut1A,puntoFut2A,puntoFut3A,puntoFut4A,puntoFut5A,puntoFut6A,puntoFut7A,puntoFut8A])
    let poli = interpolar puntosAInterpolar
    let valorDeX = encontrarXParaPolinomio numeroMes (toInt año) añoDeInicio 
    let cotizacionFutura = round4dp (interpolarLagrange puntosAInterpolar valorDeX)
    let cotizacionActual = obtenerUltimaCotizacion csv
    let cambioEnCotizacion = round4dp (obtenerVariacionCotizacion cotizacionFutura cotizacionActual)
    let porcentajeCambioEnCotizacion = round4dp (porcentajeVariacion cambioEnCotizacion cotizacionActual)
    let mon = MonedaProcesada { nombre = moneda
                              , ultimaCotizacion = cotizacionActual
                              , cotizacionFutura = cotizacionFutura
                              , variacionCotizacion = cambioEnCotizacion
                              , porcentajeVariacionCotizacion = porcentajeCambioEnCotizacion
                              , polinomio = poli
                              }                          
    return mon                         

--interpolar listaPuntos = do 
--    print ( listaPuntos )
--    print ( calcularPolinomio (listaPuntos) )
interpolar listaPuntos = calcularPolinomio (listaPuntos)

mostrarPoli poli = print poli

buscarNumeroMes :: [Char] -> Maybe Double
buscarNumeroMes nombreMes = encontrarMes nombreMes mesesDeAño

encontrarMes :: (Eq nomMes) => nomMes -> [(nomMes,numMes)] -> Maybe numMes
encontrarMes nombreMes = foldr (\(nomMes,numMes) acc -> if nombreMes == nomMes then Just numMes else acc) Nothing

toInt :: Read a => String -> a
toInt string = read string

-- numeroFecha = (12 * cantidadDeAños) + numeroMes
-- cantidadDeAños = añoIngresado - añoInicial
encontrarXParaPolinomio mes año añoDeInicio =
    case mes of
    Nothing   -> 0
    Just numMes  -> (12 * (año - añoDeInicio)) + numMes

calcularCotizacion :: [Record] -> Double -> Double    
calcularCotizacion csv numMes = obtenerResultado (crearPuntos csv) numMes

verPoli :: IO ()
verPoli = calcularPolinomio2

obtenerVariacionCotizacion :: Double -> Double -> Double
obtenerVariacionCotizacion cotizacionFutura cotizacionActual = cotizacionFutura - cotizacionActual

porcentajeVariacion :: Fractional a => a -> a -> a
porcentajeVariacion variacion cotActual = (variacion * 100) / cotActual

---------------------------------------
-- Metodo interpolarLagrange
interpolarLagrange :: Fractional b => [(b, b)] -> b -> b
interpolarLagrange lst x = lagrange lst x

-----------------------------------------
-- Armado de tabla

datosTabla moneda = Table
  (Group SingleLine
     [ Group NoLine [TT.Header (nombre moneda)]
     ])
  (Group DoubleLine
     [ Group SingleLine [TT.Header "Ultima cotizacion", TT.Header "Cotizacion Futura"]
     , Group SingleLine [TT.Header "Variacion", TT.Header "(%)"]
     ])
  [ [show (ultimaCotizacion moneda), show (cotizacionFutura moneda), show (variacionCotizacion moneda), show (porcentajeVariacionCotizacion moneda)]
  ]

-- Mostrar tabla
mostrarTabla moneda = putStrLn (render id id id (datosTabla moneda)) 
