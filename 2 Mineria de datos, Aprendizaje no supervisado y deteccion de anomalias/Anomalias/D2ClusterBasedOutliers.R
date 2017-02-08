# Francisco Pérez Hernández
setwd("~/Dropbox/zMaster/zRStudio/Master-en-Ciencia-De-Datos-e-Ingeniería-de-Computadores/2 Mineria de datos, Aprendizaje no supervisado y deteccion de anomalias/Anomalias")

rm(list=ls()) 
####### FUNCIONES
library(ggbiplot)
library(cluster)    # PAM
library(MASS) 
# Dentro de MiBiPlot_Clustering_Outliers se llama a la función ggbiplot, la cual está basada
# en la función ggplot que tiene un bug de diseño ya que dentro del parámetro aes
# solo se pueden llamar a variables del entorno global y no del entorno local.
# Por tanto, desgraciadamente, debemos establecer variables globales que 
# son usadas dentro de nuestra función MiBiPlot_Clustering_Outliers:
# BIPLOT.isOutlier
# BIPLOT.asignaciones.clusters
# BIPLOT.cluster.colors
MiBiPlot_Clustering_Outliers = function (datos, titulo){
  PCA.model = princomp(scale(datos))
  outlier.shapes = c("o","x") #c(21,8)
  
  identificadores_de_datos = rownames(datos)
  identificadores_de_datos[!BIPLOT.isOutlier] = ''
  #cat(identificadores_de_datos)
  
  BIPLOT.asignaciones.clusters = factor(BIPLOT.asignaciones.clusters)
  
  biplot = ggbiplot(PCA.model, obs.scale = 1, var.scale=1 , varname.size = 3, alpha = 0) +              
    geom_point(aes(shape = BIPLOT.isOutlier, colour = factor(BIPLOT.asignaciones.clusters)))  +
    scale_color_manual(values = BIPLOT.cluster.colors) +
    scale_shape_manual(values = outlier.shapes) +
    ggtitle(titulo) +
    geom_text(label = identificadores_de_datos, stat = "identity", size = 3, hjust=0, vjust=0)      
  
  X11()
  print(biplot)
}


###########################################################################
# MULTIVARIATE STATISTICAL OUTLIERS. CLUSTERING OUTLIERS 
###########################################################################
# Los outliers son respecto a un conjunto de variables.

#####################################################################
# Lectura de valores y Preprocesamiento
#####################################################################
# Trabajamos sobre las columnas numéricas 1,2,4 y 8 de wine
# Tanto LOF como clustering usan distancias entre registros, por lo que habrá
# que trabajar sobre los datos previamente normalizados

# Construimos los siguiente conjuntos:
# mis.datos.numericos -> con las columnas 1,2,4 y 8
# mis.datos.numericos.normalizados -> con los valores normalizados
# a Los rownames de mis.datos.numericos.normalizados les asignamos los rownames de 
# mis.datos.numericos

# Establecemos la variable numero.de.outliers a 5 y numero.de.clusters a 3
wine <- read.csv("wine-5an-nn.dat",, comment.char = "@", header = FALSE)
names(wine) <- c("Alcohol", "MalicAcid", "Ash", "AlcalinityOfAsh", "Magnesium", 
                 "TotalPhenols", "flavanoids", "NonflavanoidsPhenols", "Proanthocyanins", 
                 "ColorIntensity", "Hue", "OD280/OD315", "Proline", "Class")
mis.datos.numericos  = wine[,c(1,2,4,8)]  
mis.datos.numericos.normalizados           = scale(mis.datos.numericos)
rownames(mis.datos.numericos.normalizados) = rownames(mis.datos.numericos)
numero.de.outliers   = 5
numero.de.clusters   = 3
set.seed(2)  # Para establecer la semilla para la primera iteración de kmeans

###########################################################################
# Cómputo de los outliers según la distancia euclídea de cada dato 
# al centroide de su cluster
# El centroide podrá ser cualquiera (podrá provenir de un k-means 
# o ser un medoide, por ejemplo)
###########################################################################

###########################################################################
# k-Means
# Construimos el modelo kmeans (modelo.kmeans) con los datos normalizados. 
# Para ello, usamos la función de R llamada "kmeans"

# A partir del resultado de kmeans, accedemos a:
modelo.kmeans = kmeans(mis.datos.numericos.normalizados,centers = numero.de.clusters)
# a) $cluster para obtener 
#   los índices de asignación de cada dato al cluster correspondiente 
#   El resultado lo guardamos en la variable indices.clustering
#   Por ejemplo, si el dato con índice 69 está asignado al tercer cluster,
#   en el vector indices.clustering habrá un 3 en la componente número 69
indices.clustering = modelo.kmeans$cluster
indices.clustering
# b) $centers para obtener los datos de los centroides.
#   Los datos están normalizados por lo que los centroides también lo están.
#   El resultado lo guardamos en la variable centroides.normalizados
centroides.normalizados = modelo.kmeans$centers
centroides.normalizados

# -------------------------------------------------------------------------
# Calculamos la distancia euclídea de cada dato a su centroide (con los valores normalizados)
# Para ello, usad la siguiente función:
distancias_a_centroides = function (datos.normalizados, 
                                    indices.asignacion.clustering, 
                                    datos.centroides.normalizados){
  sqrt(rowSums((datos.normalizados - 
                  datos.centroides.normalizados[indices.asignacion.clustering,])^2))
}

dist.centroides = distancias_a_centroides(mis.datos.numericos.normalizados,
                                               indices.clustering,
                                               centroides.normalizados)
dist.centroides

# Ordenamos dichas distancias a través de la función order y obtenemos
# los índices correspondientes. Nos quedamos con los primeros
# (tantos como diga la variable numero.de.outliers)
indices.outliers = order(dist.centroides,decreasing=T)[1:numero.de.outliers]
indices.outliers

###########################################################################
# Creamos la función top_clustering_outliers para realizar las tareas anteriores
top_clustering_outliers = function(datos.normalizados, 
                                   indices.asignacion.clustering, 
                                   datos.centroides.normalizados, 
                                   numero.de.outliers)
{
  dist_centroides = distancias_a_centroides(datos.normalizados,
                                            indices.asignacion.clustering,
                                            datos.centroides.normalizados)
  indices = order(dist_centroides, decreasing = T)[1:numero.de.outliers]
  list(distancias=dist_centroides[indices],indices=indices)
}
# La función devolverá una lista con dos miembros:
# indices    -> Contiene los índices de los top outliers
# distancias -> Contiene las distancias a los centroides de los anteriores outliers

# Devuelve los índices de los top-k clustering outliers y sus distancias a los centroides

# Llamamos a la función top_clustering_outliers e imprimimos los índices y las distancias a sus 
# centroides de los outliers
top.outliers.kmeans = top_clustering_outliers(mis.datos.numericos.normalizados,
                                              indices.clustering,
                                              centroides.normalizados,
                                              numero.de.outliers)
cat("Índices de los top k clustering outliers (k-means, usando distancia euclídea)")
top.outliers.kmeans$indices 
cat("Distancias a sus centroides de los top k clustering outliers (k-means, 
    usando distancia euclídea)")
top.outliers.kmeans$distancias

###########################################################################
# Biplot de los outliers

# Creamos un vector is.kmeans.outlier de TRUE/FALSE que nos diga si cada
# registro es o no un outlier.

# Para crear el Biplot llamamos a la función MiBiPlot_Clustering_Outliers
# Dentro de esta función se llama a la función ggbiplot, la cual está basada
# en la función ggplot que tiene un bug de diseño ya que dentro del parámetro aes
# solo se pueden llamar a variables del entorno global y no del entorno local.
# Por tanto, desgraciadamente, debemos establecer variables globales que 
# son usadas dentro de nuestra función MiBiPlot_Clustering_Outliers:
numero.de.datos   = nrow(mis.datos.numericos)
is.kmeans.outlier = rep(FALSE, numero.de.datos) 
is.kmeans.outlier[top.outliers.kmeans$indices] = TRUE

BIPLOT.isOutlier             = is.kmeans.outlier
BIPLOT.cluster.colors = c("blue","red","brown") # Tantos colores como diga numero.de.clusters
BIPLOT.asignaciones.clusters = indices.clustering
MiBiPlot_Clustering_Outliers(mis.datos.numericos, "K-Means Clustering Outliers")

###########################################################################
# Los datos de los centroides construidos por el modelo están normalizados:
centroides.normalizados

# Queremos revertir la operación z-score 
# Para revertir la operación de normalización, simpemente tenemos que despejar
# en la fórmula:
#z-score = (dato - media.columna) / sd.columna
#dato = z-score * sd.columna + media.columna 

# Para aplicar la anterior fórmula, seguimos los siguientes pasos:
# Construimos un vector mis.datos.medias con las medias de cada columna 
# (usad la función colMeans)
mis.datos.medias = colMeans(mis.datos.numericos)
mis.datos.medias

# Construimos un vector mis.datos.desviaciones con las desviaciones típicas de cada columna.
# Para ello usamos apply con la función sd (standard deviation) 
mis.datos.desviaciones = apply(mis.datos.numericos,2,sd,na.rm=TRUE)
mis.datos.desviaciones

# Ahora hay que multiplicar cada dato del centroide por la desviación de la correspondiente 
# columna.
# es decir, tenemos que multiplicar centroides.normalizados[i]  por 
# mis.datos.desviaciones[i] 
# Para ello, usamos la función sweep con el operador producto "*"
centroides.valores = sweep(centroides.normalizados, 2, mis.datos.desviaciones, "*")
centroides.valores

# Finalmente, tenemos que sumar a dichos valores la media de la columna correspondiente
# para lo que volvemos a usar sweep con el anterior resultado y mis.datos.medias
centroides.valores = sweep(centroides.valores,2,mis.datos.medias,"+")
centroides.valores

###########################################################################
# Aplicar clustering con PAM (Partition around medoids)
# Previamente tenemos que calcular la matriz de distancias de todos con todos usando 
# la función dist
matriz.de.distancias = dist(mis.datos.numericos.normalizados)
# A continuación, usamos la función pam del paquete cluster, pasándole como parámetros 
# la matriz
# de distancias y k = número de clusters
# Guardamos el resultado en modelo.pam
set.seed(2)
modelo.pam = pam(matriz.de.distancias,k=numero.de.clusters)
# Para obtener las asignaciones de cada dato a su cluster accedemos a modelo.pam$clustering
indices.asignacion.clustering.pam = modelo.pam$clustering
indices.asignacion.clustering.pam # Asignación de cada dato a su cluster

# Para obtener los índices de los medoides accedemos a modelo.pam$medoids
medoides.indices = modelo.pam$medoids
medoides.indices # Indices de los k medoides

# Mostramos los valores normalizados y no normalizados de los índices
# Para ello, como los medoides son registros reales de nuestro conjunto de datos
# basta con acceder a sus valores. No tenemos que revertir el proceso de normalización
# como tuvimos que hacer con los centroides.

medoides.valores.normalizados = mis.datos.numericos.normalizados[medoides.indices, ]
medoides.valores.normalizados

medoides.valores = mis.datos.numericos[medoides.indices,]
medoides.valores

# Mostramos los índices de los outliers llamando a la función top_clustering_outliers
top.outliers.pam = top_clustering_outliers(mis.datos.numericos.normalizados,
                                           indices.asignacion.clustering.pam,
                                           medoides.valores.normalizados,
                                           numero.de.outliers)
top.outliers.pam

###########################################################################
# El objetivo es calcular la distancia de cada punto a su centroide usando la distancia 
# de Mahalanobis.

# Vamos a construir la siguiente función:
# top_clustering_outliers_distancia_mahalanobis = function(datos, 
#                                                          indices.asignacion.clustering, 
#                                                          numero.de.outliers)

# Para hacerlo, tenemos que aislar en un mismo data frame aquellos registros que
# pertenezcan al mismo cluster, obteniendo así k data frames.
# El data frame -i- tendrá los valores (en todas las variables) de los registros
# que están en el cluster -i-
# A cada data frame, le calcularemos la matriz de covarianzas, necesaria
# para calcular las distancias de Mahalanobis de todos los registros
# de ese cluster al centro de su distribución.

# Así pues, realizamos el siguiente proceso:
# Construimos el data frame "seleccion", de forma que 
# seleccion[, i] será un vector de T/F indicando qué registros pertenecen al cluster i.
# seleccion
#     cluster 1   cluster 2   cluster 3
# 1   TRUE        FALSE        FALSE     -> El registro 1 está en el cluster 1
# 2   FALSE       FALSE        TRUE      -> El registro 2 está en el cluster 3
# 3   ....

# En el ejemplo, nos quedará:
#    [,1]  [,2]  [,3]
# 1   TRUE FALSE FALSE
# 2   TRUE FALSE FALSE
# ......
# 57  FALSE TRUE FALSE
# ......

# Así pues, datos.numericos[seleccion[, i] , ]  son los datos numéricos de los registros que
# pertenecen al cluster i
# Basta usar la función cov sobre las anteriores selecciones, para obtener las k matrices 
# de covarianzas
# Guardamos las matrices de covarianzas en una lista lista.matriz.de.covarianzas 
# (usad para ello la función lapply)
# Construimos también una lista lista.vector.de.medias, de forma que la lista i-ésima 
# contendrá las medias de las variables de aquellos registros que están en el cluster -i-

# De forma alternativa, podemos usar la función cov.rob del paquete MASS
# Esta función realiza una estimación robusta de la matriz de covarianzas y de la 
# media
# Cunado apliquemos dicha función, accederemos a $cov para obtener la estimación robusta
# de la matriz de covarianzas y a $center para obtener la estimación robusta de la media.

# Ahora, basta obtener las distancias de Mahalanobis. Para ello, usamos la función mahalanobis
# a la que se le pasa como parámetros:
# - El data frame de datos. En nuestro caso serán cada uno de los data frames obtenidos 
# a partir de "seleccion"
# - El vector que contiene la medias de las variables. En nuestro caso, será la componente 
# correspondiente de lista.vector.de.medias
#   Nota: Para extraer una componente x de una lista L, debe usar L[[x]]
# - La matriz de covarianzas. En nuestro caso, será la componente correspondiente de 
# lista.matriz.de.covarianzas
# Construimos la variable mah.distances aplicando lo anterior a los k data frames, usando 
# la función lapply
# mah.distances es una lista de k listas. La lista -i- contiene las distancias de 
# Mahalanobis del cluster -i-

# mah.distances
# [[1]]
# 1          2          3          ......
# 0.8032616  4.2108010  1.2007133  ......
# ......
# ......
# [[3]]
# 54         55         ......
# 2.7151536  4.6704382  ......

# Una vez obtenido mah.distances, ya solo nos queda:
# - Unir todas las distancias en una única lista
# - Ordenar las distancias
# - Quedarnos con los top n

# La función devolverá una lista con:
# - Los índices de los top outliers
# - Las distancias de Mahalanobis de dichos outliers

# Llamamos a la función así construida con los datos de wine y mostramos el Biplot 
# correspondiente.

top_clustering_outliers_distancia_mahalanobis = function(datos, 
                                                         indices.asignacion.clustering, 
                                                         numero.de.outliers){
  cluster.ids = unique(indices.asignacion.clustering)
  k           = length(cluster.ids)
  seleccion   = sapply(1:k, function(x) indices.asignacion.clustering == x)
  # Usando medias y covarianzas:
  # lista.matriz.de.covarianzas   = lapply(1:k, function(x) cov(mis.datos.numericos[seleccion
  #[,x],]))
  # lista.vector.de.medias        = lapply(1:k, function(x) colMeans(mis.datos.numericos
  #[seleccion[,x],]))
  # Usando la estimación robusta de la media y covarianza: (cov.rob del paquete MASS:
  lista.matriz.de.covarianzas   = lapply(1:k, function(x) 
    cov.rob(mis.datos.numericos[seleccion[,x],])$cov)
  lista.vector.de.medias        = lapply(1:k, function(x) 
    cov.rob(mis.datos.numericos[seleccion[,x],])$center)
  mah.distances   = lapply(1:k, 
                           function(x) mahalanobis(mis.datos.numericos[seleccion[,x],], 
                                                   lista.vector.de.medias[[x]], 
                                                   lista.matriz.de.covarianzas[[x]]))  
  todos.juntos = unlist(mah.distances)
  todos.juntos.ordenados = names(todos.juntos[order(todos.juntos, decreasing=TRUE)])
  indices.top.mah.outliers = as.numeric(todos.juntos.ordenados[1:numero.de.outliers])
  list(distancias = mah.distances[indices.top.mah.outliers]  , indices = 
         indices.top.mah.outliers)
}
top.clustering.outliers.mah = top_clustering_outliers_distancia_mahalanobis(mis.datos.numericos, 
                                                                            indices.clustering, 
                                                                            numero.de.outliers)
numero.de.datos = nrow(mis.datos.numericos)
is.kmeans.outlier.mah = rep(FALSE, numero.de.datos) 
is.kmeans.outlier.mah[top.clustering.outliers.mah$indices] = TRUE
BIPLOT.isOutlier             = is.kmeans.outlier.mah
BIPLOT.cluster.colors        = c("blue","red","brown")     # Tantos colores como diga 
#numero.de.clusters
MiBiPlot_Clustering_Outliers(mis.datos.numericos, "K-Means Clustering Outliers")

###########################################################################
# Ampliación: 
# Definir la función top_clustering_outliers_distancia_relativa
# Esta función hará lo mismo que la función top_clustering_outliers
# pero usando como criterio la distancia relativa 
top_clustering_outliers_distancia_relativa = function(datos.normalizados, 
                                                      indices.asignacion.clustering, 
                                                      datos.centroides.normalizados, 
                                                      numero.de.outliers){
  dist_centroides = distancias_a_centroides (datos.normalizados, 
                                             indices.asignacion.clustering, 
                                             datos.centroides.normalizados)
  cluster.ids = unique(indices.asignacion.clustering)
  k           = length(cluster.ids)
  distancias.a.centroides.por.cluster    = sapply(1:k , 
                                                  function(x) dist_centroides 
                                                  [indices.asignacion.clustering  == 
                                                    cluster.ids[x]])
  
  distancias.medianas.de.cada.cluster    = sapply(1:k , 
                                                  function(x) median(dist_centroides[[x]]))
  
  todas.las.distancias.medianas.de.cada.cluster  =  
    distancias.medianas.de.cada.cluster[indices.asignacion.clustering]
  ratios = dist_centroides   /  todas.las.distancias.medianas.de.cada.cluster
  
  indices.top.outliers           = order(ratios, decreasing=T)[1:numero.de.outliers]
  
  list(distancias = ratios[indices.top.outliers]  , indices = indices.top.outliers)
}
top.outliers.kmeans.distancia.relativa = 
  top_clustering_outliers_distancia_relativa(mis.datos.numericos.normalizados, 
                                             indices.clustering, 
                                             centroides.normalizados, 
                                             numero.de.outliers)
cat("Índices de los top k clustering outliers (k-means, usando distancia relativa)")
top.outliers.kmeans.distancia.relativa$indices 
cat("Distancias a sus centroides de los top k clustering outliers (k-means, usando 
    distancia relativa)")
top.outliers.kmeans.distancia.relativa$distancias

