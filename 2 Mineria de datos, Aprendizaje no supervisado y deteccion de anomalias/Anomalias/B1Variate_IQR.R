# Francisco Pérez Hernández
setwd("~/Dropbox/zMaster/zRStudio/Master-en-Ciencia-De-Datos-e-Ingeniería-de-Computadores/2 Mineria de datos, Aprendizaje no supervisado y deteccion de anomalias/Anomalias")
# Base de datos sacada de: http://sci2s.ugr.es/keel/dataset_smja.php?cod=594 
rm(list=ls()) 
### FUNCIONES
library(reshape)   # melt
library(EnvStats)
library(ggplot2)
#######################################################################
# Muestra un plot básico con los outliers en rojo
# Necesita como parámetros de entrada:
# el dataset "datos", un vector de T/F indicando si el registro i-ésimo 
# de "datos" es o no un outlier (según el método aplicado) 
# y el título a aparecer en el gráfico
MiPlot_Univariate_Outliers = function (datos, indices_de_Outliers, titulo){
  numero.de.datos = nrow(as.matrix(datos))
  vectorTFoutliers =  rep(FALSE, numero.de.datos)
  vectorTFoutliers[indices_de_Outliers] = TRUE
  vector.colores.outlier = rep("black", numero.de.datos)
  vector.colores.outlier [vectorTFoutliers] = "red"
  
  cat("\nNumero de datos: ")
  cat(numero.de.datos)
  cat("\n?Qui?n es outlier?: ")
  cat(vectorTFoutliers)
  cat('\n')
  
  X11()
  plot(datos, col=vector.colores.outlier, main = titulo)
}
#######################################################################
# Devuelve los nombres de aquellas filas de datos especificadas 
# en el segundo parámetro (vector de T/F)
Nombres_de_Filas = function (datos, vector_TF_datos_a_incluir) {
  numero.de.filas = nrow(datos)
  
  if (is.null(row.names(datos)))
    row.names(datos) = rep(1:numero.de.filas)
  
  nombres.de.filas = rep("", numero.de.filas)
  nombres.de.filas[vector_TF_datos_a_incluir==TRUE] = row.names(datos)[vector_TF_datos_a_incluir==TRUE]
  nombres.de.filas
}
###########################################################################
# Calcula los outliers IQR 
# Devuelve un vector TRUE/FALSE indicando si el registro i-ésimo 
# de "datos" es o no un outlier IQR con respecto a la columna de índice "indice"
# coef es 1.5 para los outliers normales y hay que pasarle 3 para los outliers extremos
vector_es_outlier_IQR = function (datos, indice.de.columna, coef = 1.5){
  columna.datos = datos[,indice.de.columna]
  cuartil.primero = quantile(columna.datos)[2]  #quantile[1] es el m?nimo y quantile[5] el m?ximo.
  cuartil.tercero = quantile(columna.datos)[4] 
  iqr = cuartil.tercero - cuartil.primero
  extremo.superior.outlier = (iqr * coef) + cuartil.tercero
  extremo.inferior.outlier = cuartil.primero - (iqr * coef)
  es.outlier  = columna.datos > extremo.superior.outlier |
    columna.datos < extremo.inferior.outlier
  return (es.outlier)
}
#######################################################################
# Calcula los outliers IQR y muestra sus etiquetas en un BoxPlot
MiBoxPlot_IQR_Univariate_Outliers = function (datos, indice.de.columna, coef = 1.5){
  # Importante: Para que aes busque los parámetros en el ámbito local, debe incluirse  environment = environment()
  
  datos = as.data.frame(datos)
  vector.TF.outliers.IQR = vector_es_outlier_IQR(datos, indice.de.columna, coef)
  nombres.de.filas = Nombres_de_Filas(datos, vector.TF.outliers.IQR)
  nombre.de.columna = colnames(datos, indice.de.columna)
  
  ggboxplot = ggplot(data = datos, aes(x=factor(""), y=datos[,indice.de.columna]) , environment = environment()) + 
    xlab(nombre.de.columna) + ylab("") +
    geom_boxplot(outlier.colour = "red") + 
    geom_text(aes(label = nombres.de.filas)) #, position = position_jitter(width = 0.1))   
  
  X11()
  ggboxplot
}
###########################################################################
# Devuelve un vector con las claves de los outliers IQR
# con respecto a la columna de índice "indice"
# coef es 1.5 para los outliers normales y hay que pasarle 3 para los outliers extremos
vector_claves_outliers_IQR = function(datos, indice, coef = 1.5){
  columna.datos = datos[,indice]
  vector.de.outliers = vector_es_outlier_IQR(datos, indice, coef)
  which(vector.de.outliers  == TRUE)
}
#######################################################################
# Muestra de forma conjunta todos los BoxPlots de las columnas de datos
# Para ello, normaliza los datos.
# También muestra con un punto en rojo los outliers de cada columna
# Para hacerlo con ggplot, lamentablemente hay que construir antes una tabla 
# que contenga en cada fila el valor que a cada tupla le da cada variable -> paquete reshape->melt
MiBoxPlot_juntos  = function (datos, vector_TF_datos_a_incluir = c()){  
  # Requiere reshape
  # Importante: Para que aes busque los parámetros en el ámbito local, debe incluirse  environment = environment()
  
  nombres.de.filas = Nombres_de_Filas(datos, vector_TF_datos_a_incluir)
  
  datos = scale(datos)
  datos.melted = melt(datos)
  colnames(datos.melted)[2]="Variables"
  colnames(datos.melted)[3]="zscore"
  factor.melted = colnames(datos.melted)[1]
  columna.factor = as.factor(datos.melted[,factor.melted])
  levels(columna.factor)[!levels(columna.factor) %in% nombres.de.filas] = ""  
  
  ggplot(data = datos.melted, aes(x=Variables, y=zscore), environment = environment()) + 
    geom_boxplot(outlier.colour = "red") + 
    geom_text(aes(label = columna.factor), size = 3) 
}
#######################################################################
# Muestra de forma conjunta todos los BoxPlots de las columnas de datos
# Para ello, normaliza los datos
# También muestra las etiquetas de los outliers de cada columna
MiBoxPlot_juntos_con_etiquetas = function (datos, coef = 1.5){
  matriz.datos.TF.outliers = sapply(1:ncol(datos), function(x) vector_es_outlier_IQR(datos, x, coef))  # Aplicamos outlier IQR a cada columna
  vector.datos.TF.outliers = apply(matriz.datos.TF.outliers, 1, sum)   
  vector.datos.TF.outliers[vector.datos.TF.outliers > 1] = 1            # Si un registro es outlier en alguna columna lo incluimos
  
  MiBoxPlot_juntos(datos, vector.datos.TF.outliers)
}



###########################################################################
# UNIVARIATE STATISTICAL OUTLIERS -> IQR 
###########################################################################
# Vamos a trabajar con los siguientes objetos:
# mydata.numeric: frame de datos
# indice.columna: Índice de una columna de datos de mydata.numeric
# nombre.mydata:  Nombre del frame para que aparezca en los plots
# En este script los estableceremos a la base de datos wine, columna 1 y nombre "wine"
wine <- read.csv("wine-5an-nn.dat",, comment.char = "@", header = FALSE)
names(wine) <- c("Alcohol", "MalicAcid", "Ash", "AlcalinityOfAsh", "Magnesium", "TotalPhenols", "flavanoids", "NonflavanoidsPhenols", 
                 "Proanthocyanins", "ColorIntensity", "Hue", "OD280/OD315", "Proline", "Class")
mydata.numeric  = wine[,-14] #Eliminamos la variable class  
indice.columna  = 3
nombre.mydata   = "wine"

# ------------------------------------------------------------------------
# Ahora creamos los siguientes objetos:
# mydata.numeric.scaled -> Debe contener los valores normalizados de mydata.numeric. Para ello, usad la función scale
# columna -> Contendrá la columna de datos correspondiente a indice.columna. Basta realizar una selección con corchetes de mydata.numeric
# nombre.columna -> Debe contener el nombre de la columna. Para ello, aplicamos la función names sobre mydata.numeric
# columna.scaled -> Debe contener los valores normalizados de la anterior
mydata.numeric.scaled <- scale(mydata.numeric)
columna <- mydata.numeric[,indice.columna]
nombre.columna <- names(mydata.numeric[indice.columna])
columna.scaled <- mydata.numeric.scaled[,indice.columna]

###########################################################################
###########################################################################
# Parte primera. Cómputo de los outliers IQR
###########################################################################
###########################################################################

###########################################################################
# Calcular los outliers según la regla IQR. Directamente sin funciones propias
###########################################################################
# Calculamos las siguientes variables:
# cuartil.primero -> primer cuartil, 
# cuartil.tercero -> tercer cuartil
# iqr             -> distancia IQR

# Para ello, usamos las siguientes funciones:
# quantile(columna, x) para obtener los cuartiles x=0.25 para el primer cuartil, 0.5 para la mediana y 0.75 para el tercero
# IQR para obtener la distancia intercuartil (o bien reste directamente el cuartil tercero y el primero)

# Calculamos las siguientes variables -los extremos que delimitan los outliers-
# extremo.superior.outlier.normal  = cuartil tercero + 1.5 IQR
# extremo.inferior.outlier.normal  = cuartil primero - 1.5 IQR
# extremo.superior.outlier.extremo = cuartil tercero + 3 IQR
# extremo.inferior.outlier.extremo = cuartil primero - 3 IQR

# Construimos sendos vectores: 
# vector.es.outlier.normal 
# vector.es.outlier.extremo

# Son vectores de valores lógicos TRUE/FALSE que nos dicen si cada registro es o no un outlier con respecto a la columna fijada
# Para ello, basta comparar con el operador > o el operador < la columna con alguno de los valores extremos anteriores
cuartil.primero <- quantile(columna,0.25)
cuartil.tercero <- quantile(columna,0.75)
iqr <- iqr(columna)
extremo.superior.outlier.normal  = cuartil.tercero + (1.5* iqr)
extremo.inferior.outlier.normal  = cuartil.primero - (1.5 *iqr)
extremo.superior.outlier.extremo = cuartil.tercero + (3 *iqr)
extremo.inferior.outlier.extremo = cuartil.primero - (3* iqr)
vector.es.outlier.normal <- (columna<extremo.inferior.outlier.normal | columna > extremo.superior.outlier.normal)
vector.es.outlier.extremo <- (columna<extremo.inferior.outlier.extremo | columna>extremo.superior.outlier.extremo)

###########################################################################
# Índices y valores de los outliers
###########################################################################
# Construimos las siguientes variables:
# claves.outliers.normales     -> Vector con las claves (identificador numérico de fila) de los valores que son outliers. Para 
# obtenerlo, usad which sobre vector.es.outlier.normal
# data.frame.outliers.normales -> data frame obtenido con la selección del data frame original de las filas que son outliers. Puede 
# usarse obien vector.es.outlier.normal o bien claves.outliers.normales
# Este dataframe contiene los datos de todas las columnas de aquellas filas que son outliers.                                  
# nombres.outliers.normales    -> vector con los nombres de fila de los outliers. Para obtenerlo, usad row.names sobre el data 
# frame anterior
# valores.outliers.normales    -> vector con los datos de los outliers. Se muestra solo el valor de la columna que se fijó al 
# inicio del script 
# Idem con los extremos
claves.outliers.normales <- which(vector.es.outlier.normal)
claves.outliers.normales
data.frame.outliers.normales <- mydata.numeric[claves.outliers.normales,]
data.frame.outliers.normales
nombres.outliers.normales <- row.names(data.frame.outliers.normales)
nombres.outliers.normales
valores.outliers.normales <- mydata.numeric[claves.outliers.normales,indice.columna]
valores.outliers.normales

claves.outliers.extremos <- which(vector.es.outlier.extremo)
claves.outliers.extremos
data.frame.outliers.extremos <- mydata.numeric[claves.outliers.extremos,]
data.frame.outliers.extremos
nombres.outliers.extremos <- row.names(data.frame.outliers.extremos)
nombres.outliers.extremos
valores.outliers.extremos <- mydata.numeric[claves.outliers.extremos,indice.columna]
valores.outliers.extremos

###########################################################################
# Desviación de los outliers con respecto a la media de la columna
###########################################################################

# Construimos la variable:
# valores.normalizados.outliers.normales -> Contiene los valores normalizados de los outliers. 
# Usad columna.scaled y (o bien vector.es.outlier.normal o bien claves.outliers.normales)
valores.normalizados.outliers.normales <- columna.scaled[vector.es.outlier.normal]
valores.normalizados.outliers.normales

###########################################################################
# Plot
###########################################################################
# Mostramos en un plot los valores de los registros (los outliers se muestran en color rojo)
# Para ello, llamamos a la siguiente función:
# Lo hacemos con los outliers normales y con los extremos
MiPlot_Univariate_Outliers(columna,claves.outliers.normales,nombre.columna)
MiPlot_Univariate_Outliers(columna, claves.outliers.extremos, nombre.columna)

###########################################################################
# BoxPlot
###########################################################################
# Vemos el diagrama de caja 

# Para ello, llamaríamos a la función boxplot, pero no muestra el outlier en la 
# columna ash :-(
# boxplot(columna, xlab=nombre.columna, main=nombre.mydata, las = 1)   # las = 1 all 
# axis labels horizontal, range = 3 for exteme outliers

# Para resolverlo, vemos el diagrama de caja con ggplot geom_boxplot
# Para ello, llamamos a la siguiente función
# Llamamos a la misma función pero con los datos normalizados
# Lo hacemos para resaltar que el Boxplot es el mismo ya que el poder de la normalización 
# es que no afecta a la posición relativa de los datos 
boxplot(columna, xlab=nombre.columna, main=nombre.mydata,las=1)
MiBoxPlot_IQR_Univariate_Outliers(mydata.numeric, indice.columna)
MiBoxPlot_IQR_Univariate_Outliers(mydata.numeric.scaled, indice.columna)

###########################################################################
# Cómputo de los outliers IQR con funciones propias
###########################################################################
vector_es_outlier_IQR(mydata.numeric,indice.columna) #devuelve un vector TRUE/FALSE
vector_claves_outliers_IQR(mydata.numeric,indice.columna) #devuelve los indices de los outliers 

###########################################################################
###########################################################################
# Trabajamos con varias columnas simultáneamente
# Los outliers siguen siendo univariate, es decir, con respecto a una única columna
# Vamos a aplicar el proceso anterior de forma automática a todas las columnas
# Para ello, usaremos la función sapply
###########################################################################
###########################################################################

###########################################################################
# Índices y valores de los outliers
###########################################################################
# Obtenemos la siguiente matriz:
# frame.es.outlier -> matriz de T/F en la que por cada registro (fila), nos dice si
#                     es un outlier IQR en la columna correspondiente
# Tenemos que aplicar la función vector.es.outlier.IQR sobre cada una de las columnas
# Para ello, usamos sapply:
#   El primer argumento de sapply será el rango de las columnas que vamos a barrer, 
# es decir, 1:ncol(mydata.numeric)
#   El segundo argumento de sapply será la función a aplicar, es decir, vector_es_outlier_IQR 
#   Consulte la ayuda para obtener más información sobre sapply
frame.es.outlier <- sapply(1:ncol(mydata.numeric),function(x) 
  vector_es_outlier_IQR(mydata.numeric,x))
frame.es.outlier

#----------------------------------------------------------------------
# Construyamos la variable:
# numero.total.outliers.por.columna -> Número de outliers que hay en cada variable (columna)

# Para ello, usamos apply sobre la dimensión 2 (las columnas) y aplicamos la función sum
# Consulte la ayuda para obtener más información sobre apply
numero.total.outliers.por.columna <- apply(frame.es.outlier,2,sum)
numero.total.outliers.por.columna

#----------------------------------------------------------------------
# Obtenemos la siguiente variable:
# indices.de.outliers.en.alguna.columna -> Contiene los índices de aquellos registros que 
# tengan un valor anómalo en cualquiera de las columnas

# Para ello, usamos sapply como hacíamos anteriormente, pero con la 
# función vector_claves_outliers_IQR
# sapply devuelve una lista con los resultados de applicar la función correspondiente,
# en nuestro caso, vector_claves_outliers_IQR. 
# El resultado lo guardamos en indices.de.outliers.en.alguna.columna
indices.de.outliers.en.alguna.columna <- sapply(1:ncol(frame.es.outlier),function(x) 
  vector_claves_outliers_IQR(frame.es.outlier,x))
indices.de.outliers.en.alguna.columna
# Como esta función devuelve una lista, al final, tenemos una lista de listas.
# Puede observar que en la columna 4 hay tres outliers, en las filas con índices 14, 142, 164.
indices.de.outliers.en.alguna.columna <- unlist(indices.de.outliers.en.alguna.columna)
indices.de.outliers.en.alguna.columna
# Para "desempaquetar" el resultado, usamos la función unlist
# El anterior resultado nos quiere decir que las filas con claves 21, 69, 139, 169 ... 
# tienen un outlier en alguna de sus columnas

#----------------------------------------------------------------------

###########################################################################
# Desviación de los outliers con respecto a la media de la columna
###########################################################################
# Mostramos los valores normalizados de los registros que tienen un valor anómalo en 
# cualquier columna 
# Pero mostramos los valores de todas las columnas 
# (no solo la columna con respecto a la cual cada registro era un valor anómalo)
mydata.numeric.scaled[indices.de.outliers.en.alguna.columna,]

###########################################################################
# BoxPlot
###########################################################################
# Mostramos los boxplots en un mismo gráfico.
# Tenemos que usar los datos normalizados, para que así sean comparables

# Llamamos a la función boxplot

# Llamamos a la función  MiBoxPlot_Juntos
# MiBoxPlot_juntos  = function (datos, vector_TF_datos_a_incluir)  
# Esta función normaliza los datos y muestra, de forma conjunta, los diagramas de cajas
# Así, podemos apreciar qué rango de valores toma cada outlier en las distintas columnas.

# Para etiquetar los outliers en el gráfico
# llamamos a la función MiBoxPlot_juntos_con_etiquetas 
X11()
boxplot(mydata.numeric.scaled , main=nombre.mydata) #, range = 3)
MiBoxPlot_juntos(mydata.numeric,indices.de.outliers.en.alguna.columna)
MiBoxPlot_juntos_con_etiquetas(mydata.numeric)

