module Interpolacion.Interpolar where

type Polinomio coeficientes = [coeficientes]
type PuntosDeInterpolacion a = [(a,a)]

arregloDePuntos :: PuntosDeInterpolacion Double
arregloDePuntos = [(0,1),(1,2),(7,50),(14,100)]

calcularPolinomio = putStrLn.mostrarPolinomio$ polinomioPara arregloDePuntos

polinomioPara ::  Fractional a => PuntosDeInterpolacion a -> Polinomio a
polinomioPara [] = []
polinomioPara puntos@(_:xs) = a:polinomioPara (map(\(x,y)->(x,y-a*x^e) ) xs)   where 
 a = coeficienteDeTerminoDeMayorGrado puntos
 e = length xs

coeficienteDeTerminoDeMayorGrado :: Fractional a => PuntosDeInterpolacion a -> a
coeficienteDeTerminoDeMayorGrado [(_,y)] = y
coeficienteDeTerminoDeMayorGrado (a:b:ps) = (coeficienteDeTerminoDeMayorGrado(a:ps)-coeficienteDeTerminoDeMayorGrado(b:ps))/(x a - x b) where
 x (a,_) = a

mostrarPolinomio :: (Eq a , Num a , Show a) => Polinomio a -> String
mostrarPolinomio [] = "0"
mostrarPolinomio (0:xs) = mostrarPolinomio xs
mostrarPolinomio (x:xs) = s (length xs) ++ " + "++ mostrarPolinomio xs where
 s 0 = show x
 s 1 = show x++"x"
 s n = show x++"x^"++show n