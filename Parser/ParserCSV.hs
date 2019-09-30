module Parser.ParserCSV where

import Text.CSV

-- devuelve un mensaje de error (si se produjo un error en el parseo del csv)
manejarError :: a -> IO ()
manejarError errCsv = putStrLn "error en parseo"

-- muestra datos a partir de un csv
procesar :: Show a => [a] -> IO ()
procesar csv = mostrar (obtenerDatos csv)

-- imprime en pantalla un csv
mostrar :: Show a => a -> IO ()
mostrar csv = print csv

-- imprime en pantalla puntos generados a partir de un csv
mostrarPuntos :: [Record] -> IO ()
mostrarPuntos csv = print (crearPuntos csv)

-- elimina el header del csv
obtenerDatos :: [a] -> [a]
obtenerDatos csv = tail csv

-- dado un año y una cotizacion, devuelve un punto/tupla (x,y)
crearPunto :: Num a => a -> b -> (a, b)
crearPunto año cotizacion = (año * 12, cotizacion)

-- dados un array de elementos X y un array de elementos Y, devuelve un array de puntos/tuplas
crearPuntosAnuales :: Num a => [a] -> [b] -> [(a, b)]
crearPuntosAnuales años cotizaciones = zipWith (crearPunto) años cotizaciones

-- dado un csv, devuelve un array de puntos/tuplas (x,y)
-- x = representa un numero de mes
-- y = representa una cotizacion para el numero de mes  
crearPuntos :: (Num a, Enum a) => [Record] -> [(a, Double)]
crearPuntos csv = crearPuntosAnuales ([0..]) ( extraerColumnaComoRead (obtenerDatos csv) 1 :: [Double] ) 

-- dado un csv y un numero de columna, devuelve un array (de reads) con los datos de la columna indicada 
extraerColumnaComoRead :: Read t => CSV -> Int -> [t]
extraerColumnaComoRead csv numeroDeColumna =
  [ read (columna !! numeroDeColumna) | columna <- csv , length columna > numeroDeColumna , columna /= [""] ] 

-- dado un csv y un numero de columna, devuelve un array (de strings) con los datos de la columna indicada
extraerColumnaComoString :: [[String]] -> Int -> [String]   
extraerColumnaComoString csv numeroDeColumna =
  [ (columna !! numeroDeColumna) | columna <- csv , length columna > numeroDeColumna , columna /= [""] ]

-- dado un csv, devuelve el año del primer registro de cotizacion
obtenerAñoDeInicio :: [[String]] -> Double
obtenerAñoDeInicio csv =  read ( drop 4 (head ( extraerColumnaComoString (obtenerDatos csv) 0 )) ) :: Double

-- dado un csv, devuelve la ultima cotizacion registrada
obtenerUltimaCotizacion :: [Record] -> Double
obtenerUltimaCotizacion csv = last ( extraerColumnaComoRead (obtenerDatos csv) 1 :: [Double] )
--------------------------------------------------------------------

-- funcion para testear el modulo
parsear :: IO ()
parsear = do

  let nombreDeArchivo = "test.csv"
  entrada <- readFile nombreDeArchivo
  
  let csv = parseCSV nombreDeArchivo entrada 
  case csv of  
    Left err -> manejarError err
    Right c -> do
      procesar c
      mostrarPuntos c

