# Francisco Pérez Hernández
setwd("~/Dropbox/zMaster/zRStudio/Master-en-Ciencia-De-Datos-e-Ingeniería-de-Computadores/2 Mineria de datos, Aprendizaje no supervisado y deteccion de anomalias/Anomalias")
rm(list=ls()) 

##### FUNCIONES
library(mvoutlier)  
library(ggbiplot)
library(DMwR)
MiBiplot = function(datos){
  PCA.model = princomp(scale(datos))
  biplot = ggbiplot(PCA.model, obs.scale = 1, var.scale=1 , varname.size = 5,alpha = 1/2) 
  X11()
  print(biplot)
}
MiBiPlot_Multivariate_Outliers = function (datos, vectorTFoutliers, titulo){
  identificadores_de_datos = rownames(datos)
  identificadores_de_datos[!vectorTFoutliers] = ''
  cat(identificadores_de_datos)
  
  PCA.model = princomp(scale(datos))
  outlier.shapes = c(".","x") #c(21,8)
  biplot = ggbiplot(PCA.model, obs.scale = 1, var.scale=1 , varname.size = 5,groups =  vectorTFoutliers, alpha = 1/2) #alpha = 1/10, 
  biplot = biplot + labs(color = "Outliers")
  biplot = biplot + scale_color_manual(values = c("black","red"))
  biplot = biplot + geom_text(label = identificadores_de_datos, stat = "identity", size = 3, hjust=0, vjust=0)
  biplot = biplot + ggtitle(titulo)
  
  X11()
  print(biplot)
}

###########################################################################
# MULTIVARIATE STATISTICAL OUTLIERS -> LOF 
###########################################################################
# Los outliers son respecto a un conjunto de variables.
#####################################################################
# Lectura de valores y Preprocesamiento
#####################################################################
# Tanto LOF como clustering usan distancias entre registros, por lo que habrá
# que trabajar sobre los datos previamente normalizados

# Construimos las siguientes variables:
# mis.datos.numericos -> Contendrá las columnas numéricas de las variables seleccionadas
# mis.datos.numericos.normalizados-> Contendrá los datos normalizados
# Asignamos como nombres de filas de mis.datos.numericos.normalizados los mismos nombres 
# de filas que mis.datos.numericos

# Ampliación: Utilice la función is.numeric y sapply para construir automáticamente un data 
# frame con las columnas numéricas de otro data frame.
wine <- read.csv("wine-5an-nn.dat",, comment.char = "@", header = FALSE)
names(wine) <- c("Alcohol", "MalicAcid", "Ash", "AlcalinityOfAsh", "Magnesium", 
                 "TotalPhenols", "flavanoids", "NonflavanoidsPhenols", "Proanthocyanins", 
                 "ColorIntensity", "Hue", "OD280/OD315", "Proline", "Class")
mis.datos.numericos  = wine[,c(1,2,4,8)]  
mis.datos.numericos <- mis.datos.numericos[,sapply(mis.datos.numericos,is.numeric)]
mis.datos.numericos.normalizados <- scale(mis.datos.numericos)
rownames(mis.datos.numericos.normalizados) = rownames(mis.datos.numericos)

###########################################################################
# Para comprobar que el método de Mahalanobis no es aplicable, 
# ejecutad lo siguiente 
X11()
corr.plot(mis.datos.numericos[,1], mis.datos.numericos[,3]) 

# El gráfico nos muestra un gráfico de dispersión al cruzar las variables 1 y 3.
# Vemos que hay dos grupos bien definidos de datos.
# Los puntos que hay entre ellos deberán ser marcados como outliers
# Usando la distancia de Mahalanobis clásica (azul) el elipsoide
# contiene a ambos grupos por lo que los puntos que hubiese entre ellos no serán outliers
# Usando la distancia de Mahalanobis construida con la estimación robusta de la matriz de 
# covarianzas
# y las correspondientes medias, el elipsoide (rojo) se construye con el grupo de datos
# más numeroso y todos los datos del otro grupo se marcan como outliers :-(

# También podemos mostrar un BiPlot llamando a la función MiBiplot sobre mis.datos.numericos
# El gráfico mostrado es una simplificación ya que ahora estamos mostrando las cuatro 
# variables conjuntamente 
# en un gráfico 2 dimensional
# Podemos apreciar que hay dos nubes de puntos bien separadas.

# Así pues,el método de detección de outliers usando la distancia de Mahalanobis no es adecuado
MiBiplot(mis.datos.numericos)

###########################################################################
###########################################################################
# DISTANCE BASED OUTLIERS (LOF)
###########################################################################
###########################################################################
numero.de.vecinos.lof = 5
# Establecemos el número de vecinos a considerar numero.de.vecinos.lof = 5 y llamamos a la 
# función lofactor
# pasándole como primer parámetro el conjunto de datos normalizados y como parámetro k el 
# valor de numero.de.vecinos.lof
# Esta función devuelve un vector con los scores de LOF de todos los registros
# Lo llamamos lof.scores
lof.scoeres = lofactor(mis.datos.numericos.normalizados, k = numero.de.vecinos.lof)
# Hacemos un plot de los resultados (basta llamar a la función plot sobre lof.scores) 
# para ver los scores obtenidos por LOF.
plot(lof.scoeres)
# Podemos apreciar que hay 2 valores de lof notablemente más altos que el resto
# Así pues, establecemos la variable siguiente:
numero.de.outliers = 2
# Ordenamos los lof.scores y obtenemos los índices de los registros ordenados según el lof.score
indices.de.lof.outliers.ordenados = order(lof.scoeres, decreasing = TRUE)
indices.de.lof.outliers.ordenados
# Seleccionamos los 2 primeros y los almacenamos en 
indices.de.lof.top.outliers = indices.de.lof.outliers.ordenados[1:numero.de.outliers]
indices.de.lof.top.outliers
# Construimos un vector is.lof.outlier de TRUE/FALSE que nos dice si cada registro de los datos
# originales es o no un outlier. Para ello, debemos usar la función rownames sobre el dataset
# y el operador %in% sobre indices.de.lof.top.outliers
is.lof.outlier = rownames(mis.datos.numericos)%in%indices.de.lof.top.outliers
is.lof.outlier
# Mostramos un Biplot de los outliers llamando a la función MiBiPlot_Multivariate_Outliers
# MiBiPlot_Multivariate_Outliers = function (datos, vectorTFoutliers, titulo)
MiBiPlot_Multivariate_Outliers(mis.datos.numericos,is.lof.outlier,"LOF Outlier")
