import Text.CSV
import System.Process
import Procesador.Procesar
import Procesador.Graficar

main :: IO ()
main = do
  _ <- system "cls" -- Windows //  -- system "clear"  -- Linux
  putStrLn "-------------------------------------------------------------------------"
  putStrLn "Ingrese un mes en formato MMM: "
  nombreMes <- getLine
  putStrLn "Ingrese un año en formato AA: "
  año <- getLine

  let pathsArchivos = [ "../../../Cotizaciones/cotizacionesDolar.csv"
                      , "../../../Cotizaciones/cotizacionesReal.csv"
                      , "../../../Cotizaciones/cotizacionesEuro.csv"
                      , "../../../Cotizaciones/cotizacionesBolivar.csv"
                      , "../../../Cotizaciones/cotizacionesBitcoin.csv"]

  entradas <- mapM readFile pathsArchivos
  let datosCSVs = procesarEntradas (zip pathsArchivos entradas)
  let monedas = ["Dolar", "Real","Euro","Bolivar","Bitcoin"]
  let monedasDatos = zip monedas datosCSVs
  let monedasProcesadas = procesarMonedas nombreMes año monedasDatos
  let monedasOrdenadas = ordenarVariacionesDeCotizaciones monedasProcesadas

  mostrarTablaCotizaciones monedasProcesadas
  mostrarGraficoMonedas monedasProcesadas
  mostrarTablaVariacionCotizaciones monedasOrdenadas

  

