# Francisco Pérez Hernández
setwd("~/Dropbox/zMaster/zRStudio/Master-en-Ciencia-De-Datos-e-Ingeniería-de-Computadores/2 Mineria de datos, Aprendizaje no supervisado y deteccion de anomalias/Anomalias")

###########################################################################
# MULTIVARIATE STATISTICAL OUTLIERS -> Multivariate Normal Distribution --> Mahalanobis
###########################################################################
# Los outliers son respecto a un conjunto de variables.
# Un registro será un outlier porque tenga un valor anómalo en alguna variable
# o porque tenga una combinación anómala de valores.
rm(list=ls()) 
###### FUNCIONES
library(reshape)   # melt
library(ggbiplot)
library(CerioliOutlierDetection)
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


# Necesita:
# mydata.numeric
# mydata.numeric.scaled
wine <- read.csv("wine-5an-nn.dat",, comment.char = "@", header = FALSE)
names(wine) <- c("Alcohol", "MalicAcid", "Ash", "AlcalinityOfAsh", "Magnesium", 
                 "TotalPhenols", "flavanoids", "NonflavanoidsPhenols", "Proanthocyanins", 
                 "ColorIntensity", "Hue", "OD280/OD315", "Proline", "Class")
mydata.numeric  = wine[,-c(14,11,12,13)]  
mydata.numeric.scaled = scale(mydata.numeric)

###########################################################################
# Paquete mvoutlier
###########################################################################
###########################################################################
# Obtención de los outliers multivariantes
# Calcula los outliers calculando las distancias de Mahalanobis y usando la aproximación de 
# la Chi cuadrado
# La estimación de la matriz de covarianzas es la estimación robusta según MCD
# No hay que normalizar los datos ya que la distancia de Mahalanobis está
# diseñada, precisamente para evitar el problema de la escala.
# uni.plot genera el gráfico similar a MiPlot_Univariate_Outliers con todas las columnas
# Además, devuelve en $outliers los índices de los outliers

# Establecemos los valores de significación
# alpha.value.penalizado es para tener en cuenta el error FWER
alpha.value = 0.05
alpha.value.penalizado = 1 - ( 1 - alpha.value) ^ (1 / nrow(mydata.numeric))

# Establecemos la semilla para el método iterativo que calcula MCD 
set.seed(12)  

# Llamamos a uni.plot del paquete mvoutlier con symb=FALSE, alpha = alpha.value.penalizado
# Guardamos el resultado en la variable mvoutlier.plot
# Esta función calcula los outliers MULTIVARIANTES según la distancia de Mahalanobis
# considerando la estimación robusta de la matriz de covarianzas -MCD- y la estimación robusta 
# de la media de cada variable.
# También imprime un plot 1-dimensional para ver los valores que toman los outliers en cada 
# atributo
# pero el plot no imprime las etiquetas de los outliers 
X11()
library(mvoutlier)       
mvoutlier.plot <- uni.plot(mydata.numeric,symb=FALSE,alpha = alpha.value.penalizado)

###########################################################################
# Análisis de los outliers
# Vamos a ver las variables que más influyen en la designación de los outliers
# a) Viendo el valor normalizado sobre cada variable para ver cuanto se desvía de la media
#     Pero esto no es suficiente ya que no es fácil apreciar interacciones entre atributos
# b) Gráficamente, con un biplot sobre las componentes principales
#     El Biplot permite ver las dimensiones importantes que influyen en la designación de 
# los outliers
# Construimos la variable 
# is.MCD.outlier 
# que será un vector TRUE/FALSE que nos dice si cada dato es o no un outlier 
# Para ello, accedemos a mvoutlier.plot$outliers
# Contamos el número total de outliers y lo guardamos en la variable numero.de.outliers.MCD
is.MCD.outlier <- mvoutlier.plot$outliers
is.MCD.outlier
numero.de.outliers.MCD <- sum(is.MCD.outlier)
numero.de.outliers.MCD

# -------------------------------------------------------------------------
# ¿Cuál es el valor normalizado de cada outlier, es decir, ¿Cuánto se desvía de la media de 
# cada columna?
# Esta desviación ya se ha mostrado antes al llamar a uni.plot, pero solo se muestran los 
# outliers como puntos rojos
# Al no tener las etiquetas, no sabemos cuáles son los valores de los outliers en cada columna.
# Construimos una tabla numérica data.frame.solo.outliers que muestre los valores 
# normalizados de los outliers en todas las columnas 
# Para ello, usamos mydata.numeric.scaled y is.MCD.outlier:
data.frame.solo.outliers <- mydata.numeric.scaled[is.MCD.outlier,]
data.frame.solo.outliers

# -------------------------------------------------------------------------
# Mostramos los boxplots de forma conjunta con las etiquetas de los outliers
# Para ello llamamos a la función MiBoxPlot_juntos pasando como parámetro is.MCD.outlier
# MiBoxPlot_juntos  = function (datos, vector_TF_datos_a_incluir)  
MiBoxPlot_juntos(mydata.numeric,is.MCD.outlier)

# -------------------------------------------------------------------------
# El BoxPlot conjunto nos informa sobre los valores extremos que hay en cada variable
# Puede apreciarse que casi todos los outliers multivariate corresponden a outliers univariate
# El BiPlot nos muestra también esta información, junto con las correlaciones entre variables
# Los puntos mostrados son resultados de proyecciones de n dimensiones a 2, por lo que 
# solo es una representación aproximada (mejor cuanto mayor sea la suma de los porcentajes
# que aparecen como componentes principales PC1 y PC2)
# Llamamos a la función MiBiPlot_Multivariate_Outliers
# MiBiPlot_Multivariate_Outliers = function (datos, vectorTFoutliers, titulo)
MiBiPlot_Multivariate_Outliers(mydata.numeric,is.MCD.outlier,"MCD")

# -------------------------------------------------------------------------
# Vamos a construir una matriz con los gráficos de dispersión obtenidos al cruzar todas las 
# variables
# Y vamos a destacar en rojo el dato correspondiente a 126.
# MiPlot_Univariate_Outliers = function (datos, indices_de_Outliers, titulo)
# El parámetro indices_de_Outliers únicamente contendrá el índice de 126.
indices.de.interes = 126
MiPlot_Univariate_Outliers(mydata.numeric,indices.de.interes,"MCD")


###########################################################################
# Ampliación
###########################################################################
###########################################################################
# Paquete CerioliOutlierDetection
# Andrea Cerioli. Multivariate outlier detection with high-breakdown estimators. Journal of the
# American Statistical Association, 105(489):147-156, 2010.

# Calcula los outliers calculando las distancias de Mahalanobis y usando la aproximación de 
# Hardin/Rocke
# La estimación de la matriz de covarianzas es la estimación robusta según MCD

# Repetir los cómputos hechos anteriormente con el paquete mvoutlier
# pero usando ahora cualquiera de las siguientes funciones:
# cerioli2010.irmcd.test -> La función calcula el alpha penalizado. Se le pasa el valor usual 
# de alpha 0.05 o 0.025 en el parámetro signif.gamma
# cerioli2010.fsrmcd.test -> Hay que calcular manualmente el alpha penalizado y pasarlo en el 
# parámetro signif.alpha
# Nota: También admiten un vector de niveles. Parámetro: signif.alpha=c(0.05,0.01,0.001)
# En cualquier caso, no se detecta ningún outlier en mtcars con este método
is.Cerioli.outlier = cerioli2010.irmcd.test(mydata.numeric, signif.gamma=alpha.value)$outliers
indices.outliers.cerioli = which(is.Cerioli.outlier == TRUE)
indices.outliers.cerioli
numero.de.outliers.cerioli = sum(is.Cerioli.outlier)
numero.de.outliers.cerioli 

MiBiPlot_Multivariate_Outliers(mydata.numeric, is.Cerioli.outlier, "MCD. Cerioli's package")
