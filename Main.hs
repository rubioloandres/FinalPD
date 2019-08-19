module Main where

import Parser.ParserCSV
import Interpolador.Interpolar

import Text.CSV

mesesDeAño =
    [("ene",1)
    ,("feb",2)
    ,("mar",3)
    ,("abr",4)
    ,("may",5)
    ,("jun",6)
    ,("jul",7)
    ,("ago",8)
    ,("sep",9)
    ,("oct",10)
    ,("nov",11)
    ,("dic",12)
    ]

main :: IO ()
main = print "Principal"

obtenerCotizaciones = do

  putStr "Ingrese un mes en formato MMM: "
  nombreMes <- getLine
  let numeroMes = buscarNumeroMes nombreMes

  putStr "Ingrese una año en formato AA: "
  año <- getLine

  putStrLn "-----------------------------------------------------------"
  putStrLn ("Estimacion de cotizaciones para " ++ nombreMes ++ " del 20" ++ año )

  putStrLn "-----------------------------------------------------------"
  let nombreDeArchivo1 = "pruebaDolar.csv"
  entrada <- readFile nombreDeArchivo1

  let nombreDeArchivo2 = "pruebaReal.csv"
  entrada2 <- readFile nombreDeArchivo2

  putStrLn "---Cotizacion Dolar (a graficar)---"
  let datosCSV1 = parseCSV nombreDeArchivo1 entrada
  either manejarError interpolar datosCSV1

  putStrLn "---Cotizacion Real (a graficar)---"
  let datosCSV2 = parseCSV nombreDeArchivo2 entrada2
  either manejarError interpolar datosCSV2

  putStrLn "-----------------------------------------------------------"

--  print "El numero de mes es: "
  let valorDeX = encontrarXParaPolinomio numeroMes (toInt año)
--  print ( valorDeX )

  putStr "La cotizacion del dolar para la fecha deseada es: "
  case datosCSV1 of
    Left err -> print "error"
    Right msg -> putStrLn ( show (calcularCotizacion msg valorDeX) )
    
  putStr "La cotizacion del real para la fecha deseada es: "
  case datosCSV2 of
    Left err -> print "error"
    Right msg -> putStrLn ( show (calcularCotizacion msg valorDeX) )

  putStrLn "-----------------------------------------------------------"
  putStr "Ultima cotizacion del dolar: "
  case datosCSV1 of
    Left err -> print "error"
    Right msg -> putStrLn ( show ( ultimaCotizacion msg ) )
    
  putStr "Ultima cotizacion del real: "
  case datosCSV2 of
    Left err -> print "error"
    Right msg -> putStrLn ( show ( ultimaCotizacion msg ) )
    
  putStr "Variacion de cotizacion del dolar: "
  case datosCSV1 of
    Left err -> print "error"
    Right msg ->  putStrLn ( show ( variacionCotizacion msg valorDeX ) ++ " ("  ++ (show (porcentajeVariacion (variacionCotizacion msg valorDeX) (ultimaCotizacion msg) )) ++ "%) ")
    
  putStr "Variacion de cotizacion del real: "
  case datosCSV2 of
    Left err -> print "error"
    Right msgd -> putStrLn ( show ( variacionCotizacion msgd valorDeX ) ++ " ("  ++ (show (porcentajeVariacion (variacionCotizacion msgd valorDeX) (ultimaCotizacion msgd) )) ++ "%)")

  putStrLn "-----------------------------------------------------------"
---------------METODOS---------------------------------------
interpolar csv = print ( calcularPolinomio (crearPuntos csv) )

buscarNumeroMes nombreMes = encontrarMes nombreMes mesesDeAño

encontrarMes :: (Eq nomMes) => nomMes -> [(nomMes,numMes)] -> Maybe numMes
encontrarMes nombreMes = foldr (\(nomMes,numMes) acc -> if nombreMes == nomMes then Just numMes else acc) Nothing

toInt string = read string

-- numeroFecha = (12 * cantidadDeAños) + numeroMes
-- cantidadDeAños = añoIngresado - añoInicial
encontrarXParaPolinomio mes año =
  case mes of
    Nothing   -> 0
    Just numMes  -> (12 * (año - 19)) + numMes
    
calcularCotizacion csv numMes = obtenerResultado (crearPuntos csv) numMes

verPoli = calcularPolinomio2

-- variacionCotizacion cotizacionFutura cotizacionActual = cotizacionFutura - cotizacionActual
variacionCotizacion csv numMes = ( calcularCotizacion csv numMes ) - (ultimaCotizacion csv) 

-- porcentajeVariacion = (variacionCotizacion * 100) / cotizacionActual
porcentajeVariacion variacion cotActual = (variacion * 100) / cotActual