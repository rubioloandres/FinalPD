module Procesador.Procesar where

import Text.CSV
import Parser.ParserCSV
import Interpolador.Interpolar    
import Graphics.EasyPlot

import Text.Tabular.AsciiArt
import Text.Tabular as TT

import Data.List (sortBy)
import Data.Ord (comparing)

import System.Process

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

procesarDatos moneda csv nomMes año = do
    let numeroMes = buscarNumeroMes nomMes
    let añoDeInicio = obtenerAñoDeInicio csv
    let listaPuntos = crearPuntos csv
    let ultimoP = ultimoPunto listaPuntos
    let listaVars = variaciones (map (\x -> snd x )  listaPuntos )
    let promVars = average listaVars
    let ultimaCot = obtenerUltimaCotizacion csv
    let puntosFut = puntosAñoFuturo [1,2,3,4,5,6,7,8] ultimoP promVars ultimaCot
    let puntosAInterpolar = (listaPuntos ++ puntosFut)
    let poli = interpolar puntosAInterpolar
    let valorDeX = encontrarXParaPolinomio numeroMes (toInt año) añoDeInicio 
    let cotizacionFutura = round4dp (interpolarLagrange puntosAInterpolar valorDeX)
    let cotizacionActual = obtenerUltimaCotizacion csv
    let cambioEnCotizacion = round4dp (obtenerVariacionCotizacion cotizacionFutura cotizacionActual)
    let porcentajeCambioEnCotizacion = round2dp (porcentajeVariacion cambioEnCotizacion cotizacionActual)
    let listaCotPorAño = convertirXEnAños listaPuntos
    let listaCotFuturas = convertirXEnAños ([(last listaPuntos)] ++ (take 3 puntosFut))

    let mon = MonedaProcesada { nombre = moneda
                              , ultimaCotizacion = cotizacionActual
                              , fechaCotizacionFutura = (valorDeX / 12 ) + 2003
                              , cotizacionFutura = cotizacionFutura
                              , variacionCotizacion = cambioEnCotizacion
                              , porcentajeVariacionCotizacion = porcentajeCambioEnCotizacion
                              , polinomio = poli
                              , cotizaciones = listaCotPorAño
                              , cotizacionesFuturas = listaCotFuturas
                              }                          
    return mon                         

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

convertirXEnAños lista = map (\ (x,y) -> ( (x / 12 ) + 2003 ,y ) ) lista

-----------------------------------------
-- Armado de tablas

datosTablaCotizaciones monedas = Table
  (Group SingleLine
     [ Group NoLine [TT.Header (nombre (monedas !! 0))]
     , Group NoLine [TT.Header (nombre (monedas !! 1))]
     , Group NoLine [TT.Header (nombre (monedas !! 2))]
     , Group NoLine [TT.Header (nombre (monedas !! 3))]
     , Group NoLine [TT.Header (nombre (monedas !! 4))]
     ]
  )
  (Group DoubleLine
     [ Group SingleLine [TT.Header "Ultima cotizacion", TT.Header "Cotizacion Futura"]
     , Group SingleLine [TT.Header "Variacion", TT.Header "Variacion Porcentual"]
     ]
  )
  [ [ "$ " ++ show (ultimaCotizacion (monedas !! 0))
    , "$ " ++ show (cotizacionFutura (monedas !! 0))
    , "$ " ++ show (variacionCotizacion (monedas !! 0))
    , show (porcentajeVariacionCotizacion (monedas !! 0)) ++ " %"
    ],
    [ "$ " ++ show (ultimaCotizacion (monedas !! 1))
    , "$ " ++ show (cotizacionFutura (monedas !! 1))
    , "$ " ++ show (variacionCotizacion (monedas !! 1))
    , show (porcentajeVariacionCotizacion (monedas !! 1)) ++ " %"
    ],
    [ "$ " ++ show (ultimaCotizacion (monedas !! 2))
    , "$ " ++ show (cotizacionFutura (monedas !! 2))
    , "$ " ++ show (variacionCotizacion (monedas !! 2))
    , show (porcentajeVariacionCotizacion (monedas !! 2)) ++ " %"
    ],
    [ "$ " ++ show (ultimaCotizacion (monedas !! 3))
    , "$ " ++ show (cotizacionFutura (monedas !! 3))
    , "$ " ++ show (variacionCotizacion (monedas !! 3))
    , show (porcentajeVariacionCotizacion (monedas !! 3)) ++ " %"
    ],
    [ "$ " ++ show (ultimaCotizacion (monedas !! 4))
    , "$ " ++ show (cotizacionFutura (monedas !! 4))
    , "$ " ++ show (variacionCotizacion (monedas !! 4))
    , show (porcentajeVariacionCotizacion (monedas !! 4)) ++ " %"
    ]
  ]

datosTablaVariacionCotizaciones monedas = Table
  (Group SingleLine
      [ Group NoLine [TT.Header "1°"]
      , Group NoLine [TT.Header "2°"]
      , Group NoLine [TT.Header "3°"]
      , Group NoLine [TT.Header "4°"]
      , Group NoLine [TT.Header "5°"]
      ]
  )
  (Group DoubleLine
      [ Group SingleLine [TT.Header "Moneda"]
      , Group SingleLine [TT.Header "Variacion Porcentual"]
      ]
  )
  [ [ nombre (monedas !! 0), show (porcentajeVariacionCotizacion (monedas !! 0)) ++ " %"  ]
  , [ nombre (monedas !! 1), show (porcentajeVariacionCotizacion (monedas !! 1)) ++ " %"  ]
  , [ nombre (monedas !! 2), show (porcentajeVariacionCotizacion (monedas !! 2)) ++ " %"  ]
  , [ nombre (monedas !! 3), show (porcentajeVariacionCotizacion (monedas !! 3)) ++ " %"  ]
  , [ nombre (monedas !! 4), show (porcentajeVariacionCotizacion (monedas !! 4)) ++ " %"  ]
  ]

-- Mostrar tablas
mostrarTablaCotizaciones monedas = putStrLn (render id id id (datosTablaCotizaciones monedas)) 

mostrarTablaVariacionCotizaciones monedas = putStrLn (render id id id (datosTablaVariacionCotizaciones monedas)) 

-----------------------------------------
-- Visualizacion grafica de estimaciones

mostrarCotizaciones cotizaciones = print cotizaciones 

plotearCotizaciones datosMoneda = do
  putStrLn ("--- Evolucion del " ++ (nombre datosMoneda) ++ ". Escriba el comando 'quit' para cerrar el plot y continuar ---")
  plot' [Interactive] Windows [ Data2D [Title "Cotizaciones Historicas", Style Linespoints, Color Blue] [] (cotizaciones datosMoneda)                              
                              , Data2D [Title "Cotizaciones Estimadas", Style Linespoints, Color Green] [] (cotizacionesFuturas datosMoneda)
                              , Data2D [Title "Cotizacion Futura", Style Points, Color Red] [] [( (fechaCotizacionFutura datosMoneda),(cotizacionFutura datosMoneda) )]
                              ]
  putStrLn "----------------------------------------------------------------"
  
------------------------------------------
-- Procesamiento de monedas

mostrarGraficoMonedas [] =  putStrLn "-------------------------------------------------------------------------" 
mostrarGraficoMonedas (x:xs) = do
            plotearCotizaciones ( x ) 
            mostrarGraficoMonedas xs

procesarMonedas nomMes año listaDatos = map (procesarMoneda nomMes año) listaDatos

procesarMoneda nomMes año dato = case (snd dato) of
  Left  perr -> do
    let monedaNoProcesada = MonedaProcesada { nombre = "error"
                                            , ultimaCotizacion = 0.0
                                            , fechaCotizacionFutura = 0.0
                                            , cotizacionFutura = 0.0
                                            , variacionCotizacion = 0
                                            , porcentajeVariacionCotizacion = 0
                                            , polinomio = "error"
                                            , cotizaciones = []
                                            , cotizacionesFuturas = []
                                            }  
    monedaNoProcesada   
  Right csv ->  do
    let datosMoneda = (procesarDatos (fst dato) csv nomMes año)
    (datosMoneda !! 0)
   
ordenarVariacionesDeCotizaciones monedas = reverse (sortBy (comparing porcentajeVariacionCotizacion) monedas)
