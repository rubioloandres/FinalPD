module Interpolador.Interpolar where

type Polinomio coeficientes = [coeficientes]
type PuntosDeInterpolacion a = [(a,a)]

-- array de puntos a interpolar
arregloDePuntos :: PuntosDeInterpolacion Double
arregloDePuntos = [(0,1),(1,2),(7,50),(14,100)]

-- dado un array de puntos, devuelve un polinomio interpolante
calcularPolinomio :: PuntosDeInterpolacion Double -> String
calcularPolinomio arregloDePuntos = mostrarPolinomio $ polinomioPara arregloDePuntos

-- utiliza el array de puntos del modulo, para testear la generacion del polinomio
calcularPolinomio2 :: IO ()
calcularPolinomio2 = putStrLn.mostrarPolinomio $ polinomioPara arregloDePuntos

obtenerResultado :: PuntosDeInterpolacion Double -> Double -> Double
obtenerResultado arregloDePuntos x = evaluar ( polinomioPara arregloDePuntos ) x

polinomioPara :: PuntosDeInterpolacion Double -> Polinomio Double
polinomioPara [] = []
polinomioPara puntos@(_:xs) = a:polinomioPara (map(\(x,y)->(x,  y-a*x^e ) ) xs)   where 
 a = coeficienteDeTerminoDeMayorGrado puntos
 e = length xs

redondear4Decimales :: Double -> Double
redondear4Decimales x = fromIntegral (round $ x * 1e4) / 1e4

redondear2Decimales :: Double -> Double
redondear2Decimales x = fromIntegral (round $ x * 1e2) / 1e2

coeficienteDeTerminoDeMayorGrado :: PuntosDeInterpolacion Double -> Double
coeficienteDeTerminoDeMayorGrado [(_,y)] = y
coeficienteDeTerminoDeMayorGrado (a:b:ps) = (coeficienteDeTerminoDeMayorGrado(a:ps)-coeficienteDeTerminoDeMayorGrado(b:ps))/(x a - x b) where
 x (a,_) = a

mostrarPolinomio :: (Eq a , Num a , Show a) => Polinomio a -> String
mostrarPolinomio [] = "0"
mostrarPolinomio (0:xs) = mostrarPolinomio xs
mostrarPolinomio (x:xs) = s (length xs) ++ " + "++ mostrarPolinomio xs where
 s 0 = show x
 s 1 = show x ++ "x"
 s n = show x ++ "x^" ++ show n

evaluar :: (Eq a , Num a) => Polinomio a -> a -> a 
evaluar [] num = 0
evaluar (0:xs) num = evaluar xs num
evaluar (x:xs) num = (s (length xs) ) + (evaluar xs num) where
 s 0 = x
 s 1 = ( x * num )
 s n = ( ( num ^ n ) * x )

 -- Metodo de Lagrange
lagrange :: Fractional b => [(b, b)] -> b -> b
lagrange lst x =
    let n   = length lst - 1
        xs  = map fst lst
        ys  = map snd lst
        p i = product[(x - xs !! j) / (xs !! i - xs !! j) | j <- [0 .. n], i /= j]
        q   = [(ys !! i) * (p i)| i <- [0..n]]
    in  sum q

---------------------------------------
-- dada una lista de puntos, y una incongnita, devuelve el valor correspontiente a la incognita
interpolarLagrange :: [(Double, Double)] -> Double -> Double
interpolarLagrange lst x = do
    let res = lagrange lst x
    if (res :: Double) > 0
        then res
        else (abs res) * 0.1