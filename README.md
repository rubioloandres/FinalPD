En Busca de la Haskellicidad

Motivaci�n: 
Vivimos en una sociedad marcada por el impacto de la inestabilidad econ�mica que se da en el pa�s y la regi�n, por lo que es necesario mantenernos informados, y a partir de esto anticiparnos a ciertos problemas, o bien para tomar decisiones acertadas en el caso de inversiones.

Objetivo:
Bas�ndonos en datos hist�ricos de las cotizaciones de diferentes tipos de cambio, se propone realizar una aplicaci�n que sea capaz de realizar estimaciones de cotizaciones, para una fecha determinada.
Este proyecto propone una soluci�n en respuesta a lo planteado, desarroll�ndose un estimador que es capaz de procesar datos hist�ricos, realizar c�lculos, y retornar cotizaciones futuras.
Con estos datos obtenidos, podremos analizar la evoluci�n monetaria en el transcurso del tiempo, y tambi�n las variaciones frente a otros tipos de cambio.

Soluci�n:
La aplicaci�n se vale de dos tipos de entradas para dar inicio al procesamiento, por un lado los datos temporales (mes y a�o) y por otro los datos hist�ricos de cotizaciones (CSV).
Los datos temporales, son ingresados manualmente, al inicio de la ejecuci�n del programa.
Las monedas que fueron tenidas en cuenta fueron el D�lar Estadounidense, el Real, el Euro, el Bol�var venezolano y el Bitcoin.
Las fechas a partir de las cuales se llevan registros inician en 2003. Los �ltimos registros de cotizaciones corresponden al 2019.
Mediante una combinacion de datos hist�ricos y datos futuros calculados a partir de estos, se realiza una estimacion utilizando interpolacion.
Procesadas todas las monedas, se ordenan seg�n el criterio de m�xima variaci�n y se muestran gr�ficos comparativos, donde se detalla la �ltima cotizaci�n registrada, la cotizaci�n futura, y tambi�n la variaci�n monetaria y porcentual.
Por otra parte, se muestran gr�ficos que representan la evoluci�n de cada moneda a largo del tiempo.
A partir de estos gr�ficos estimativos, se podr� tomar la decisi�n sobre en qu� moneda es m�s conveniente invertir.

Ejecuci�n: 
El proyecto cuenta con un archivo ejecutable, alojado en la carpeta dist/build/FinalPD.
Este archivo puede ser iniciado desde una consola. 
Inicialmente nos solicitara un mes, que deber� ser ingresado en el formato MMM. Por ejemplo, si deseamos obtener una estimacion para octubre, corresponde OCT.
Tambi�n solicitar� ingresar un a�o, en el formato AA. Por ejemplo, si deseamos una estimaci�n para 2020, corresponde 20.
Luego de ingresar estos datos, visualizaremos una tabla informativa, acerca de los puntos m�s importantes para cada cotizaci�n
Tambi�n se podr� observar, para cada moneda, un gr�fico que muestra la evoluci�n de cada moneda en el tiempo, y la cotizaci�n futura correspondiente.
Deber� ingresarse el comando �q� o �quit�, para cerrar la ventana emergente del graficador, y as� continuar.
Finalmente, se muestra una tabla comparativa, donde aparecer� de forma ordenada, las diferentes alternativas, siendo la primera, la mejor opci�n para invertir. 

Autor�a:
_ Haskell Data Analysis Cookbook (Parser CSV)
_ BCRA (Cotizaciones)
_ Interpolation-Methods-Haskell Jorge Atempa (Interpolacion)