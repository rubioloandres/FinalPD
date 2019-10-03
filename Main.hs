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
  
  let monedas = ["Dolar", "Real","Euro","Bolivar","Bitcoin"]
  let pathsArchivos = map ( \m -> "../../../Cotizaciones/cotizaciones" ++ m ++ ".csv" ) monedas 
  entradas <- mapM readFile pathsArchivos
  let datosCSVs = procesarEntradas (zip pathsArchivos entradas)
  let monedasDatos = zip monedas datosCSVs
  let monedasProcesadas = procesarMonedas nombreMes año monedasDatos
  let monedasOrdenadas = ordenarVariacionesDeCotizaciones monedasProcesadas

  mostrarTablaCotizaciones monedasProcesadas
  mostrarGraficoMonedas monedasProcesadas
  mostrarTablaVariacionCotizaciones monedasOrdenadas

  

