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

obtenerPolinomio = do

  putStrLn "Ingrese un mes en formato MMM: "
  nombreMes <- getLine
  let numeroMes = buscarNumeroMes nombreMes

  putStrLn "Ingrese una año en formato AA: "
  año <- getLine

  print "-------------------------"
  putStrLn ("El mes ingresado es " ++ nombreMes )
  putStrLn ("El año ingresado es " ++ año )

  print "-------------------------"
  let nombreDeArchivo1 = "pruebaDolar.csv"
  entrada <- readFile nombreDeArchivo1

  let nombreDeArchivo2 = "pruebaReal.csv"
  entrada2 <- readFile nombreDeArchivo2

  print "---Polinomio de Cotizacion Dolar---"
  let datosCSV1 = parseCSV nombreDeArchivo1 entrada
  either manejarError interpolar datosCSV1

  print "---Polinomio de Cotizacion Real---"
  let datosCSV2 = parseCSV nombreDeArchivo2 entrada2
  either manejarError interpolar datosCSV2

  print "-------------------------"

  print "El numero de mes es: "
  let valorDeX = encontrarXParaPolinomio numeroMes (toInt año)
  print ( valorDeX )

  print "La cotizacion del dolar para la fecha deseada es: "
  case datosCSV1 of
    Left err -> print "error"
    Right msg -> calcularCotizacion msg valorDeX
    
    
  print "La cotizacion del real para la fecha deseada es: "
  case datosCSV2 of
    Left err -> print "error"
    Right msg -> calcularCotizacion msg valorDeX


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
    Just val  -> (12 * (año - 19)) + val
    
calcularCotizacion csv numMes = print ( obtenerResultado (crearPuntos csv) numMes )

verPoli = calcularPolinomio2