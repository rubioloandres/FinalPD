import Text.CSV
import Text.Parsec.Error
import Text.Tabular.AsciiArt

import Procesador.Procesar

main :: IO ()
main = do
  putStrLn "-------------------------------------------------------------------------"
  putStrLn "Ingrese un mes en formato MMM: "
  nombreMes <- getLine
  putStrLn "Ingrese un año en formato AA: "
  año <- getLine

  let nombreDeArchivo1 = "pruebaDolar.csv"
  let nombreDeArchivo2 = "pruebaReal.csv"
  entrada <- readFile nombreDeArchivo1
  entrada2 <- readFile nombreDeArchivo2

  let datosCSV1 = parseCSV nombreDeArchivo1 entrada
  let datosCSV2 = parseCSV nombreDeArchivo2 entrada2

  let datos = [datosCSV1,datosCSV2]
  let monedas = ["Dolar", "Real"]
  let monedasDatos = zip monedas datos
  let monedasProcesadas = procesarMonedas nombreMes año monedasDatos

  mostrarMonedas monedasProcesadas

