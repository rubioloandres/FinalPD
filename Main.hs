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
main = do
  putStrLn "Ingrese un mes en formato MMM: "
  nombreMes <- getLine
  putStrLn "Ingrese una año en formato AA: "
  año <- getLine

  let nombreDeArchivo1 = "cotizacionDolar.csv"
  entrada <- readFile nombreDeArchivo1
  let nombreDeArchivo2 = "pruebaReal.csv"
  entrada2 <- readFile nombreDeArchivo2

  let datosCSV1 = parseCSV nombreDeArchivo1 entrada
  case datosCSV1 of
    Left err -> print "error"
    Right csvParseado -> (procesarCSV "dolar" csvParseado nombreMes año) 

  let datosCSV2 = parseCSV nombreDeArchivo2 entrada2
  case datosCSV2 of
    Left err -> print "error"
    Right csvParseado -> (procesarCSV "real" csvParseado nombreMes año) 
  
---------------METODOS---------------------------------------
procesarCSV :: [Char] -> [Record] -> [Char] -> [Char] -> IO ()
procesarCSV moneda csv nomMes año = do
  let numeroMes = buscarNumeroMes nomMes
  putStrLn "-----------------------------------------------------------"
  putStrLn ("Estimacion de cotizaciones para " ++ nomMes ++ " del 20" ++ año )
  putStrLn "-----------------------------------------------------------"
  print ("---Cotizacion "++ (moneda) ++" (a graficar)---")
  interpolar csv
  putStrLn "-----------------------------------------------------------"
  let valorDeX = encontrarXParaPolinomio numeroMes (toInt año)
  print ("La cotizacion del "++ (moneda) ++" para la fecha deseada es: ")
  putStrLn ( show (calcularCotizacion csv valorDeX) )
  putStrLn "-----------------------------------------------------------"
  print ("Ultima cotizacion del "++ (moneda) ++": ")
  putStrLn ( show ( ultimaCotizacion csv ) )
  print ("Variacion de cotizacion del "++ (moneda) ++": ")
  putStrLn ( show ( variacionCotizacion csv valorDeX ) ++ " ("  ++ (show (porcentajeVariacion (variacionCotizacion csv valorDeX) (ultimaCotizacion csv) )) ++ "%) ")
  putStrLn "-----------------------------------------------------------"

interpolar :: [Record] -> IO ()
interpolar csv = print ( calcularPolinomio (crearPuntos csv) )

buscarNumeroMes :: [Char] -> Maybe Double
buscarNumeroMes nombreMes = encontrarMes nombreMes mesesDeAño

encontrarMes :: (Eq nomMes) => nomMes -> [(nomMes,numMes)] -> Maybe numMes
encontrarMes nombreMes = foldr (\(nomMes,numMes) acc -> if nombreMes == nomMes then Just numMes else acc) Nothing

toInt :: Read a => String -> a
toInt string = read string

encontrarXParaPolinomio :: Num p => Maybe p -> p -> p
-- numeroFecha = (12 * cantidadDeAños) + numeroMes
-- cantidadDeAños = añoIngresado - añoInicial
encontrarXParaPolinomio mes año =
  case mes of
    Nothing   -> 0
    Just numMes  -> (12 * (año - 19)) + numMes

calcularCotizacion :: [Record] -> Double -> Double    
calcularCotizacion csv numMes = obtenerResultado (crearPuntos csv) numMes

verPoli :: IO ()
verPoli = calcularPolinomio2

variacionCotizacion :: [Record] -> Double -> Double
-- variacionCotizacion cotizacionFutura cotizacionActual = cotizacionFutura - cotizacionActual
variacionCotizacion csv numMes = ( calcularCotizacion csv numMes ) - (ultimaCotizacion csv) 

porcentajeVariacion :: Fractional a => a -> a -> a
-- porcentajeVariacion = (variacionCotizacion * 100) / cotizacionActual
porcentajeVariacion variacion cotActual = (variacion * 100) / cotActual