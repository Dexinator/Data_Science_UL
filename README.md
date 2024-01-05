# Data_Science_UL

Respuestas abiertas:
Sección A
Se trabajó como era solicitado, de forma lineal y no lineal. El modelo no lineal no logró expresa la variabilidad ni en un 50%. Mientras los modelos no lineales lograron un accuracy de más del 70%.
Además pudimos observar que algunas variables no eran tan relevantes para el modelo y fueron removidas, no tuvieron un impacto significativo en el rendimiento pero sí en la simplicidad del mismo.
Utilizar un modelo más robusto podría ser algo interesante a explorar pero podríamos caer en sobre-complicar/no tan parsimoniosos.
En caso de buscar hacerlo tras un breve análisis creo que un modelo SVM podría ser bueno para explicar el comportamiento de la variable Target.

Sección B
a)Revisar cantidad de valores vacíos, revisar si existen outliers, covarianza/correlación entre las variables, formato de los datos consistente, visualización de los datos
b)Optimización de presupuestos, al tener datos estacionales podemos ayudarnos de ellos para preveer controles de inventario x departamento.
Con las correlaciones entre variables podemos entender mejor donde comenzar algunas campañas de MKT y dónde otras, específicamente con la relación entre ventas y tamaño de la tienda.
Conocer a dónde enviar productos premium y a donde productos de gama más baja. Si tenemos la relación entre venta x depto x tienda
c)Son un total de 3,428,731 registros entre todos los conjuntos de datos
d)45
e)81
f)Gráfica anexada como Top10Ventas.png
g)Tendencia_Ventas.png, para los periodos estacionales hice un análisis semanal y otro mensual. Tengo la sospecha de que comparar las ventas semanalmente para estacionalidad en este caso no es lo mejor. Debido a que las semanas no siempre coinciden con los periodos de pago de nóminas por lo que podrían existir semanas desplazadas pero que reflejan el mismo comportamiento. Es por esto que consideré pertinente agrupar los datos por mes y ver cómo se comportaba una serie de tiempo mensual. En cualquier caso las semanas que más estacionalidad presentan son la 17, 25, 30, 31, 40, 47 y 48. Los meses son:Diciembre y Abril
h)La variable Size
i)Sí existían Outliers y se removieron utilizando el método rango intercuartílico.
j)Sí existían y se quitaron utilizando funciones para reemplazarlos por ceros
k)Diciembre, Abril y Julio
l)Gráfica agregada como Top10Ventas.png
m)Gráfica agregada como Histograma_Tiendas_xTamaño.png
n)Haciendo pruebas estadísticas como Augmented Dickey-Fuller para estacionariedad y la Densidad espectral para estacionalidad.
o)La tendencia la puedes ir quitando haciendo diferenciaciones de los datos que te permite conservar la relación de los datos con el tiempo sin la inercia de la tendencia
La estacionalidad también suele disminuir al hacer diferenciaciones pero existen técnicas para suavizar los datos como las medias móviles que contriubyen a ello. Trabajar con modelos ARIMA también nos puede llegar a ayudar a entender los datos sin su estacionalidad.
