En Busca de la Haskellicidad

Motivacion
Vivimos en una sociedad marcada por el impacto de la inestabilidad económica que se da en el país y la region, por lo que es necesario mantenernos informados, y a partir de esto anticiparnos a ciertos problemas, o bien para tomar decisiones acertadas en el caso de inversiones.

Objetivo
Basandonos en datos historicos de las cotizaciones de diferentes tipos de cambio, se propone realizar una aplicación que sea capaz de realizar estimaciones de cotizaciones, para una fecha determinada.
Este proyecto propone una solución en respuesta a lo planteado, desarrollándose un estimador que es capaz de procesar datos históricos, realizar cálculos, y retornar cotizaciones futuras.
Con estos datos obtenidos, podremos analizar la evolución monetaria en el transcurso del tiempo, y también las variaciones frente a otros tipos de cambio.

Solucion
La aplicación se vale de dos tipos de entradas para dar inicio al procesamiento, por un lado los datos temporales (mes y año) y por otro los datos históricos de cotizaciones (CSV).
Los datos temporales, son ingresados manualmente, al inicio de la ejecución del programa.
Las monedas que fueron tenidas en cuenta fueron el Dólar Estadounidense, el Real, el Euro, el Bolívar venezolano y el Bitcoin.
Las fechas a partir de las cuales se llevan registros inician en 2003. Los últimos registros de cotizaciones corresponden al 2019.
Mediante una combinacion de datos históricos y datos futuros calculados a partir de estos, se realiza una estimacion utilizando interpolacion.
Procesadas todas las monedas, se ordenan según el criterio de máxima variación y se muestran gráficos comparativos, donde se detalla la última cotización registrada, la cotización futura, y también la variación monetaria y porcentual.
Por otra parte, se muestran gráficos que representan la evolución de cada moneda a largo del tiempo.
A partir de estos gráficos estimativos, se podrá tomar la decisión sobre en qué moneda es más conveniente invertir.

Ejecución 
El proyecto cuenta con un archivo ejecutable, alojado en la carpeta dist/build/FinalPD.
Este archivo puede ser iniciado desde una consola. 
Inicialmente nos solicitara un mes, que deberá ser ingresado en el formato MMM. Por ejemplo, si deseamos obtener una estimacion para octubre, corresponde OCT.
También solicitará ingresar un año, en el formato AA. Por ejemplo, si deseamos una estimación para 2020, corresponde 20.
Luego de ingresar estos datos, visualizaremos una tabla informativa, acerca de los puntos más importantes para cada cotización
También se podrá observar, para cada moneda, un gráfico que muestra la evolución de cada moneda en el tiempo, y la cotización futura correspondiente.
Deberá ingresarse el comando “q” o “quit”, para cerrar la ventana emergente del graficador, y así continuar.
Finalmente, se muestra una tabla comparativa, donde aparecerá de forma ordenada, las diferentes alternativas, siendo la primera, la mejor opción para invertir. 