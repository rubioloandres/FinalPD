import Text.CSV
import Text.Parsec.Error
import Text.Tabular.AsciiArt
import System.Process
import Procesador.Procesar
import Procesador.Graficar

main :: IO ()
main = do
  system "cls" -- Windows
  -- system "clear"  -- Linux
  putStrLn "-------------------------------------------------------------------------"
  putStrLn "Ingrese un mes en formato MMM: "
  nombreMes <- getLine
  putStrLn "Ingrese un año en formato AA: "
  año <- getLine

  let nombreDeArchivo1 = "../../../Cotizaciones/cotizacionesDolar.csv"
  let nombreDeArchivo2 = "../../../Cotizaciones/cotizacionesReal.csv"
  let nombreDeArchivo3 = "../../../Cotizaciones/cotizacionesEuro.csv"
  let nombreDeArchivo4 = "../../../Cotizaciones/cotizacionesBolivar.csv" 
  let nombreDeArchivo5 = "../../../Cotizaciones/cotizacionesBitcoin.csv"
  entrada  <- readFile nombreDeArchivo1
  entrada2 <- readFile nombreDeArchivo2
  entrada3 <- readFile nombreDeArchivo3
  entrada4 <- readFile nombreDeArchivo4
  entrada5 <- readFile nombreDeArchivo5

  let datosCSV1 = parseCSV nombreDeArchivo1 entrada
  let datosCSV2 = parseCSV nombreDeArchivo2 entrada2
  let datosCSV3 = parseCSV nombreDeArchivo3 entrada3
  let datosCSV4 = parseCSV nombreDeArchivo4 entrada4
  let datosCSV5 = parseCSV nombreDeArchivo4 entrada5

  let datos = [datosCSV1,datosCSV2,datosCSV3,datosCSV4,datosCSV5]
  let monedas = ["Dolar", "Real","Euro","Bolivar","Bitcoin"]
  let monedasDatos = zip monedas datos
  let monedasProcesadas = procesarMonedas nombreMes año monedasDatos
  let monedasOrdenadas = ordenarVariacionesDeCotizaciones monedasProcesadas

  mostrarTablaCotizaciones monedasProcesadas
  mostrarGraficoMonedas monedasProcesadas
  mostrarTablaVariacionCotizaciones monedasOrdenadas


