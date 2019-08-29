module Interpolador.Interpolar where

type Polinomio coeficientes = [coeficientes]
type PuntosDeInterpolacion a = [(a,a)]

arregloDePuntos :: PuntosDeInterpolacion Double
-- arregloDePuntos = [(1,37.4069),(2,38.4086),(3,41.3624),(4,43.2338)]
arregloDePuntos = [(0,1),(1,2),(7,50),(14,100)]

calcularPolinomio :: PuntosDeInterpolacion Double -> String
calcularPolinomio arregloDePuntos = mostrarPolinomio $ polinomioPara arregloDePuntos

calcularPolinomio2 :: IO ()
calcularPolinomio2 = putStrLn.mostrarPolinomio $ polinomioPara arregloDePuntos

obtenerResultado :: PuntosDeInterpolacion Double -> Double -> Double
obtenerResultado arregloDePuntos x = calcular ( polinomioPara arregloDePuntos ) x
-- obtenerResultado x = calcular ( polinomioPara arregloDePuntos ) x 

polinomioPara :: PuntosDeInterpolacion Double -> Polinomio Double
-- polinomioPara ::  Fractional a => PuntosDeInterpolacion a -> Polinomio a
polinomioPara [] = []
polinomioPara puntos@(_:xs) = a:polinomioPara (map(\(x,y)->(x,  y-a*x^e ) ) xs)   where 
 a = coeficienteDeTerminoDeMayorGrado puntos
 e = length xs

round4dp :: Double -> Double
round4dp x = fromIntegral (round $ x * 1e4) / 1e4

round2dp :: Double -> Double
round2dp x = fromIntegral (round $ x * 1e2) / 1e2

coeficienteDeTerminoDeMayorGrado :: PuntosDeInterpolacion Double -> Double
-- coeficienteDeTerminoDeMayorGrado :: Fractional a => PuntosDeInterpolacion a -> a
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

calcular :: (Eq a , Num a) => Polinomio a -> a -> a 
calcular [] num = 0
calcular (0:xs) num = calcular xs num
calcular (x:xs) num = (s (length xs) ) + (calcular xs num) where
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