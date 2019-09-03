module Procesador.Graficar where

import Procesador.Procesar
import Graphics.EasyPlot
import Text.Tabular.AsciiArt
import Text.Tabular as TT
------------------------------------------------------------------------------
-- Armado de tablas

-- dado un array de monedas procesadas, devuelve una tabla informativa
datosTablaCotizaciones :: [MonedaProcesada] -> TT.Table String [Char] [Char]
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

------------------------------------------------------------------------------

-- dado un array de monedas procesadas, devuelve una tabla comparativa de variaciones de sus cotizaciones
datosTablaVariacionCotizaciones :: [MonedaProcesada] -> TT.Table [Char] [Char] String
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

------------------------------------------------------------------------------
-- Mostrar tablas

-- dado un array de monedas procesadas, muestra una tabla informativa de monedas
mostrarTablaCotizaciones :: [MonedaProcesada] -> IO ()
mostrarTablaCotizaciones monedas = putStrLn (render id id id (datosTablaCotizaciones monedas)) 

-- dado un array de monedas procesadas, muestra una tabla comparativa de variaciones de cotizaciones
mostrarTablaVariacionCotizaciones :: [MonedaProcesada] -> IO ()
mostrarTablaVariacionCotizaciones monedas = putStrLn (render id id id (datosTablaVariacionCotizaciones monedas)) 

------------------------------------------------------------------------------
-- Visualizacion grafica de estimaciones

-- dada una lista de monedas procesadas, devuelve graficos sucesivos para cada moneda
mostrarGraficoMonedas :: [MonedaProcesada] -> IO ()
mostrarGraficoMonedas [] =  putStrLn "-------------------------------------------------------------------------" 
mostrarGraficoMonedas (x:xs) = do
            plotearCotizaciones ( x ) 
            mostrarGraficoMonedas xs

-- dada una moneda procesada, devuelve un grafico a traves de gnuplot
plotearCotizaciones :: MonedaProcesada -> IO ()
plotearCotizaciones datosMoneda = do
  putStrLn ("--- Evolucion del " ++ (nombre datosMoneda) ++ ". Escriba el comando 'q' para cerrar el plot y continuar ---")
  plot' [Interactive] Windows [ Data2D [Title "Cotizaciones Historicas", Style Linespoints, Color Blue] [] (cotizaciones datosMoneda)                              
                              , Data2D [Title "Cotizaciones Estimadas", Style Linespoints, Color Green] [] (cotizacionesFuturas datosMoneda)
                              , Data2D [Title "Cotizacion Futura", Style Points, Color Red] [] [( (fechaCotizacionFutura datosMoneda),(cotizacionFutura datosMoneda) )]
                              ]
  putStrLn "----------------------------------------------------------------"
  
------------------------------------------------------------------------------
-- AUXILIARES

mostrarCotizaciones :: Show a => a -> IO ()
mostrarCotizaciones cotizaciones = print cotizaciones 


