# M?ster -> Detecci?n de anomal?as
# Universidad de Granada

###########################################################################
# MULTIVARIATE STATISTICAL OUTLIERS -> LOF 
###########################################################################
# Los outliers son respecto a un conjunto de variables.

#####################################################################
# Lectura de valores y Preprocesamiento
#####################################################################
# Tanto LOF como clustering usan distancias entre registros, por lo que habr?
# que trabajar sobre los datos previamente normalizados

# Construimos las siguientes variables:
# mis.datos.numericos -> Contendr? las columnas num?ricas de iris, es decir, iris [1:4]
# mis.datos.numericos.normalizados-> Contendr? los datos normalizados
# Asignamos como nombres de filas de mis.datos.numericos.normalizados los mismos nombres 
# de filas que mis.datos.numericos

# Ampliaci?n: Utilice la funci?n is.numeric y sapply para construir autom?ticamente un data 
# frame con las columnas num?ricas de otro data frame.

# COMPLETAR
mis.datos.numericos <- iris
mis.datos.numericos <- mis.datos.numericos[,sapply(mis.datos.numericos,is.numeric)]
mis.datos.numericos.normalizados <- scale(mis.datos.numericos)
rownames(mis.datos.numericos.normalizados) = rownames(mis.datos.numericos)

###########################################################################
# Transparencia 97

# Para comprobar que el m?todo de Mahalanobis no es aplicable, 
# ejecutad lo siguiente (hay que tener cargada la librer?a mvoutlier)
X11()
corr.plot(mis.datos.numericos[,1], mis.datos.numericos[,3]) 

# El gr?fico nos muestra un gr?fico de dispersi?n al cruzar las variables 1 y 3.
# Vemos que hay dos grupos bien definidos de datos.
# Los puntos que hay entre ellos deber?an ser marcados como outliers
# Usando la distancia de Mahalanobis cl?sica (azul) el elipsoide
# contiene a ambos grupos por lo que los puntos que hubiese entre ellos no ser?an outliers
# Usando la distancia de Mahalanobis construida con la estimaci?n robusta de la matriz de 
# covarianzas
# y las correspondientes medias, el elipsoide (rojo) se construye con el grupo de datos
# m?s numeroso y todos los datos del otro grupo se marcan como outliers :-(

# Tambi?n podemos mostrar un BiPlot llamando a la funci?n MiBiplot sobre mis.datos.numericos
# El gr?fico mostrado es una simplificaci?n ya que ahora estamos mostrando las cuatro 
# variables conjuntamente 
# en un gr?fico 2 dimensional (Transparencia 72)
# Podemos apreciar que hay dos nubes de puntos bien separadas.

# As? pues,el m?todo de detecci?n de outliers usando la distancia de Mahalanobis no es adecuado
MiBiplot(mis.datos.numericos)

###########################################################################
###########################################################################
# DISTANCE BASED OUTLIERS (LOF)
###########################################################################
###########################################################################
# Transparencia 112
numero.de.vecinos.lof = 5
# Establecemos el n?mero de vecinos a considerar numero.de.vecinos.lof = 5 y llamamos a la 
# funci?n lofactor
# pas?ndole como primer par?metro el conjunto de datos normalizados y como par?metro k el 
# valor de numero.de.vecinos.lof
# Esta funci?n devuelve un vector con los scores de LOF de todos los registros
# Lo llamamos lof.scores
lof.scoeres = lofactor(mis.datos.numericos.normalizados, k = numero.de.vecinos.lof)
# Hacemos un plot de los resultados (basta llamar a la funci?n plot sobre lof.scores) 
# para ver los scores obtenidos por LOF.
plot(lof.scoeres)
# Podemos apreciar que hay 4 valores de lof notablemente m?s altos que el resto
# As? pues, establecemos la variable siguiente:
numero.de.outliers = 4
# Ordenamos los lof.scores y obtenemos los ?ndices de los registros ordenados seg?n el lof.score
indices.de.lof.outliers.ordenados = order(lof.scoeres, decreasing = TRUE)
indices.de.lof.outliers.ordenados
# Seleccionamos los 4 primeros y los almacenamos en 
indices.de.lof.top.outliers = indices.de.lof.outliers.ordenados[1:numero.de.outliers]
indices.de.lof.top.outliers
# Construimos un vector is.lof.outlier de TRUE/FALSE que nos dice si cada registro de los datos
# originales es o no un outlier. Para ello, debemos usar la funci?n rownames sobre el dataset
# y el operador %in% sobre indices.de.lof.top.outliers
is.lof.outlier = rownames(mis.datos.numericos)%in%indices.de.lof.top.outliers
is.lof.outlier
# Mostramos un Biplot de los outliers llamando a la funci?n MiBiPlot_Multivariate_Outliers
# MiBiPlot_Multivariate_Outliers = function (datos, vectorTFoutliers, titulo)
MiBiPlot_Multivariate_Outliers(mis.datos.numericos,is.lof.outlier,"LOF Outlier")
