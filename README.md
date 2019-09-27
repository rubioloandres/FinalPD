# En Busca de la Haskellicidad
_Estimador y comparador de diferentes tipos de cambio, que toma como base de referencia al valor de compra con pesos argentinos._

## Motivación 
_Vivimos en una sociedad marcada por el impacto de la inestabilidad económica que se da en el país y la región, por lo que es necesario mantenernos informados, y a partir de esto anticiparnos a ciertos problemas, o bien para tomar decisiones acertadas en el caso de inversiones._

## Objetivo
_Basándonos en datos históricos de las cotizaciones de diferentes tipos de cambio, se propone realizar una aplicación que sea capaz de realizar estimaciones de cotizaciones, para una fecha determinada.
Este proyecto propone una solución en respuesta a lo planteado, desarrollándose un estimador que es capaz de procesar datos históricos, realizar cálculos, y retornar cotizaciones futuras.
Con estos datos obtenidos, podremos analizar la evolución monetaria en el transcurso del tiempo, y también las variaciones frente a otros tipos de cambio._

## Solución
_La aplicación se vale de dos tipos de entradas para dar inicio al procesamiento, por un lado los datos temporales (mes y año) y por otro los datos históricos de cotizaciones (CSV).
Los datos temporales, son ingresados manualmente, al inicio de la ejecución del programa.
Las monedas que fueron tenidas en cuenta son el Dolar Estadounidense, el Real, el Euro, el Bolivar venezolano y el Bitcoin.
Las fechas a partir de las cuales se llevan registros inician en 2003. Los últimos registros de cotizaciones corresponden al 2019.
Mediante una combinacion de datos históricos y datos futuros calculados a partir de estos, se realiza una estimacion utilizando interpolacion.
Procesadas todas las monedas, se ordenan según el criterio de máxima variación y se muestran gráficos comparativos, donde se detalla la última cotización registrada, la cotización futura, y también la variación monetaria y porcentual.
Por otra parte, se muestran gráficos que representan la evolución de cada moneda a largo del tiempo._
**A partir de estos gráficos estimativos, se podrá tomar la decisión sobre en qué moneda es más conveniente invertir.**

## Ejecución
_Se requiere una instalacion previa de [GNUPlot](https://github.com/rubioloandres/FinalPD/blob/master/INSTALLGNUPlot.md)_
_El proyecto cuenta con un archivo ejecutable, alojado en la carpeta_ 
```
dist/build/FinalPD
```
_Este archivo puede ser iniciado desde una consola._ 
_Inicialmente nos solicitará un mes, que deberá ser ingresado en el formato MMM._ 
```
Si deseamos obtener una estimación para octubre, corresponde ingresar 'OCT'.
```
_También solicitará ingresar un año, en el formato AA._ 
```
Si deseamos una estimación para 2020, corresponde ingresar '20'.
```
_Luego de ingresar estos datos, visualizaremos una tabla informativa, acerca de los puntos más importantes para cada cotización_
_Se podrá observar, para cada moneda, un gráfico que muestra la evolución de cada moneda en el tiempo, y la cotización futura correspondiente._
```
Deberá ingresarse el comando 'q' o 'quit', para cerrar la ventana emergente del graficador, y así continuar.
```
**Finalmente, se muestra una tabla comparativa, donde aparecerá de forma ordenada, las diferentes alternativas, siendo la primera, la mejor opción para invertir.** 

## Construcción
### Herramientas
* [Visual Studio Code](https://code.visualstudio.com/) - Editor - Version 1.38.1
* [GHC](https://www.haskell.org/ghc/)  - Compilador - Version 8.6.5
* [Cabal](https://www.haskell.org/cabal/) - Arquitectura - Version 2.4.1.0
* [GitKraken](https://www.gitkraken.com/) - Interfaz gráfica para git
* [GNUPlot](http://www.gnuplot.info/) - Utilidad de gráficos - Version 5.2
* [Wolfram](https://www.wolframalpha.com/) - Análisis y Cálculos 

### Librerías
* [Base](http://hackage.haskell.org/package/base) - Libreria Base
* [Text.CSV](http://hackage.haskell.org/package/csv-0.1.2/docs/Text-CSV.html) - Parseo de datos CSV
* [Easyplot](http://hackage.haskell.org/package/easyplot) - Graficador de evolución de cotizaciones
* [Tabular](http://hackage.haskell.org/package/tabular) - Tablas de datos bidimensionales
* [Parsec](http://hackage.haskell.org/package/parsec) - Combinadores de analizador monádico
* [Process](http://hackage.haskell.org/package/process) - Bibliotecas de procesos

### Autoría
* [Haskell Data Analysis Cookbook](https://subscription.packtpub.com/book/big_data_and_business_intelligence/9781783286331/1/ch01lvl1sec12/keeping-and-representing-data-from-a-csv-file) - Parser CSV
* [BCRA](https://www.bcra.gob.ar/PublicacionesEstadisticas/Evolucion_moneda.asp)  - Cotizaciones
* [Interpolation-Methods-Haskell](https://github.com/jatempa/Interpolation-Methods-Haskell) - Interpolación

---
Desarrollado por [Andres Rubiolo](https://github.com/rubioloandres)