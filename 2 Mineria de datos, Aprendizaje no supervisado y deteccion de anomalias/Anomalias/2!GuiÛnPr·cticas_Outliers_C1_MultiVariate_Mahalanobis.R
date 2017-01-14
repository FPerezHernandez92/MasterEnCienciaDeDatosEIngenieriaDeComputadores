# M?ster -> Detecci?n de anomal?as
# Universidad de Granada

###########################################################################
# MULTIVARIATE STATISTICAL OUTLIERS -> Multivariate Normal Distribution --> Mahalanobis
###########################################################################
# Los outliers son respecto a un conjunto de variables.
# Un registro ser? un outlier porque tenga un valor an?malo en alguna variable
# o porque tenga una combinaci?n an?mala de valores.

# Necesita:
# mydata.numeric
# mydata.numeric.scaled

# Trabajamos sobre mtcars[,-c(8:11)]
mydata.numeric = mtcars[,-c(8:11)]
mydata.numeric.scaled = scale(mydata.numeric)

###########################################################################
# Paquete mvoutlier
###########################################################################
###########################################################################
# Obtenci?n de los outliers multivariantes
# Transparencia 95

# Calcula los outliers calculando las distancias de Mahalanobis y usando la aproximaci?n de 
# la Chi cuadrado
# La estimaci?n de la matriz de covarianzas es la estimaci?n robusta seg?n MCD
# No hay que normalizar los datos ya que la distancia de Mahalanobis est?
# dise?ada, precisamente para evitar el problema de la escala.

# uni.plot genera el gr?fico similar a MiPlot_Univariate_Outliers con todas las columnas
# Adem?s, devuelve en $outliers los ?ndices de los outliers

# Establecemos los valores de significaci?n
# alpha.value.penalizado es para tener en cuenta el error FWER
alpha.value = 0.05
alpha.value.penalizado = 1 - ( 1 - alpha.value) ^ (1 / nrow(mydata.numeric)) # Transparencia 91

# Establecemos la semilla para el m?todo iterativo que calcula MCD 
set.seed(12)  

# Llamamos a uni.plot del paquete mvoutlier con symb=FALSE, alpha = alpha.value.penalizado
# Guardamos el resultado en la variable mvoutlier.plot
# Esta funci?n calcula los outliers MULTIVARIANTES seg?n la distancia de Mahalanobis
# considerando la estimaci?n robusta de la matriz de covarianzas -MCD- y la estimaci?n robusta 
# de la media de cada variable.
# Tambi?n imprime un plot 1-dimensional para ver los valores que toman los outliers en cada 
# atributo
# pero el plot no imprime las etiquetas de los outliers 

# Nota: Es posible que haya que instalar el paquete pcaPP para que se pueda ejecutar uni.plot

X11()

# COMPLETAR
mvoutlier.plot <- uni.plot(mydata.numeric,symb=FALSE,alpha = alpha.value.penalizado)

###########################################################################
# An?lisis de los outliers

# Vamos a ver las variables que m?s influyen en la designaci?n de los outliers
# a) Viendo el valor normalizado sobre cada variable para ver cu?nto se desv?a de la media
#     Pero esto no es suficiente ya que no es f?cil apreciar interacciones entre atributos
# b) Gr?ficamente, con un biplot sobre las componentes principales
#     El Biplot permite ver las dimensiones importantes que influyen en la designaci?n de 
# los outliers

# Construimos la variable 
# is.MCD.outlier 
# que ser? un vector TRUE/FALSE que nos dice si cada dato es o no un outlier 
# Para ello, accedemos a mvoutlier.plot$outliers
# Contamos el n?mero total de outliers y lo guardamos en la variable numero.de.outliers.MCD

# COMPLETAR
is.MCD.outlier <- mvoutlier.plot$outliers
is.MCD.outlier
numero.de.outliers.MCD <- sum(is.MCD.outlier)
numero.de.outliers.MCD

# -------------------------------------------------------------------------
# ?Cu?l es el valor normalizado de cada outlier, es decir, ?Cu?nto se desv?a de la media de 
# cada columna?
# Esta desviaci?n ya se ha mostrado antes al llamar a uni.plot, pero s?lo se muestran los 
# outliers como puntos rojos
# Al no tener las etiquetas, no sabemos cu?les son los valores de los outliers en cada columna.

# Construimos una tabla num?rica data.frame.solo.outliers que muestre los valores 
# normalizados de los outliers en todas las columnas 
# Para ello, usamos mydata.numeric.scaled y is.MCD.outlier:

# COMPLETAR
data.frame.solo.outliers <- mydata.numeric.scaled[is.MCD.outlier,]
data.frame.solo.outliers

# -------------------------------------------------------------------------
# Mostramos los boxplots de forma conjunta con las etiquetas de los outliers
# Para ello llamamos a la funci?n MiBoxPlot_juntos pasando como par?metro is.MCD.outlier
# MiBoxPlot_juntos  = function (datos, vector_TF_datos_a_incluir)  

# COMPLETAR
MiBoxPlot_juntos(mydata.numeric,is.MCD.outlier)

# -------------------------------------------------------------------------
# Transparencia 72  (Biplot)

# El BoxPlot conjunto nos informa sobre los valores extremos que hay en cada variable
# Puede apreciarse que casi todos los outliers multivariate corresponden a outliers univariate
# Las ?nicas excepciones son Fiat 128 y Ferrari Dino, aunque Fiat 128 es casi un outlier en mpg

# El BiPlot nos muestra tambi?n esta informaci?n, junto con las correlaciones entre variables
# Los puntos mostrados son resultados de proyecciones de n dimensiones a 2, por lo que 
# s?lo es una representaci?n aproximada (mejor cuanto mayor sea la suma de los  porcentajes
# que aparecen como componentes principales PC1 y PC2)
# Llamamos a la funci?n MiBiPlot_Multivariate_Outliers
# MiBiPlot_Multivariate_Outliers = function (datos, vectorTFoutliers, titulo)

# COMPLETAR
MiBiPlot_Multivariate_Outliers(mydata.numeric,is.MCD.outlier,"MCD")

# -------------------------------------------------------------------------
# El BiPlot muestra claramente que Ferrari Dino no es outlier univariate en ninguna variable
# (no est? en el extremo delimitado por los vectores correspondientes a las variables)
# Posiblemente sea un outlier multivariate debido a la combinaci?n anormal de varias variables.

# Vamos a construir una matriz con los gr?ficos de dispersi?n obtenidos al cruzar todas las 
# variables
# Y vamos a destacar en rojo el dato correspondiente a Ferrari Dino.
# Para ello, obtenemos el ?ndice de Ferrari Dino usando las funciones which y rownames
# y llamamos a la funci?n MiPlot_Univariate_Outliers 
# MiPlot_Univariate_Outliers = function (datos, indices_de_Outliers, titulo)
# El par?metro indices_de_Outliers ?nicamente contendr? el ?ndice del Ferrari Dino.
# Puede apreciarse que no hay una combinaci?n clara de 2 variables que hagan del Ferrari un 
# outlier.
# Es posible que intervengan m?s de dos variables.
# Efectivamente, si observamos la tabla data.frame.solo.outliers
# parece ser que consigue una aceleraci?n qsec muy buena -1.3 (bastante cercana a la 
# mayor -> Maserati Bora -1.8)
# con una potencia hp normal 0.4 (Maserati 2.7). Tener un peso wt ligero -0.4 seguramente es 
# un factor decisivo (Maserati 0.3)
# La combinaci?n peso, aceleraci?n, hp es lo que hace de Ferrari Dino un outlier multivariate.

# COMPLETAR
indices.de.interes <- which(rownames(mydata.numeric) == "Ferrari Dino")
indices.de.interes
MiPlot_Univariate_Outliers(mydata.numeric,indices.de.interes,"MCD")

###########################################################################
# Ampliaci?n
###########################################################################
###########################################################################
# Paquete CerioliOutlierDetection
# Andrea Cerioli. Multivariate outlier detection with high-breakdown estimators. Journal of the
# American Statistical Association, 105(489):147-156, 2010.

# Calcula los outliers calculando las distancias de Mahalanobis y usando la aproximaci?n de 
# Hardin/Rocke
# La estimaci?n de la matriz de covarianzas es la estimaci?n robusta seg?n MCD

# Repetir los c?mputos hechos anteriormente con el paquete mvoutlier
# pero usando ahora cualquiera de las siguientes funciones:
# cerioli2010.irmcd.test -> La funci?n calcula el alpha penalizado. Se le pasa el valor usual 
# de alpha 0.05 o 0.025 en el par?metro signif.gamma
# cerioli2010.fsrmcd.test -> Hay que calcular manualmente el alpha penalizado y pasarlo en el 
# par?metro signif.alpha
# Nota: Tambi?n admiten un vector de niveles. Par?metro: signif.alpha=c(0.05,0.01,0.001)
# En cualquier caso, no se detecta ning?n outlier en mtcars con este m?todo

# COMPLETAR
is.Cerioli.outlier = cerioli2010.irmcd.test(mydata.numeric, signif.gamma=alpha.value)$outliers
indices.outliers.cerioli = which(is.Cerioli.outlier == TRUE)
indices.outliers.cerioli
numero.de.outliers.cerioli = sum(is.Cerioli.outlier)
numero.de.outliers.cerioli 

MiBiPlot_Multivariate_Outliers(mydata.numeric, is.Cerioli.outlier, "MCD. Cerioli's package")
