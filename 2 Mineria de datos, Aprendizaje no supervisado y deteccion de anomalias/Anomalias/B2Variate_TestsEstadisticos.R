# Francisco Pérez Hernández
setwd("~/Dropbox/zMaster/zRStudio/Master-en-Ciencia-De-Datos-e-Ingeniería-de-Computadores/2 Mineria de datos, Aprendizaje no supervisado y deteccion de anomalias/Anomalias")
###########################################################################
# UNIVARIATE STATISTICAL OUTLIERS -> 1-variate Normal Distribution
# Grubbs' test. Normal 1-dim. 1 único outlier
# grubbs.test {outliers}

# Rosner's test. Normal 1-dim. <= k outliers.
# rosnerTest {EnvStats}
rm(list=ls()) 
######### FUNCIONES
library(outliers)
library(EnvStats)
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
# Aplica el test de Grubbs e imprime los resultados
MiPlot_resultados_TestGrubbs = function(datos){
  alpha = 0.05
  test.de.Grubbs = grubbs.test(datos, two.sided = TRUE)
  cat('p.value: ')
  cat(test.de.Grubbs$p.value)
  cat('\n')
  if (test.de.Grubbs$p.value < alpha){
    indice.de.outlier.Grubbs = order(abs(datos - mean(datos)), decreasing = T)[1]
    indice.de.outlier.Grubbs
    cat('Índice de outlier: ')
    cat(indice.de.outlier.Grubbs)
    cat('\n')
    valor.de.outlier.Grubbs  = datos[indice.de.outlier.Grubbs]
    cat('Valor del outlier: ')
    cat(valor.de.outlier.Grubbs)
    MiPlot_Univariate_Outliers (datos, "Test de Grubbs", indice.de.outlier.Grubbs)
  }
  else
    cat('No hay outliers')
}
#######################################################################
# Aplica el test de Rosner e imprime los resultados
MiPlot_resultados_TestRosner = function(datos){  
  test.de.rosner = rosnerTest(datos, k=4)
  is.outlier.rosner = test.de.rosner$all.stats$Outlier
  k.mayores.desviaciones.de.la.media = test.de.rosner$all.stats$Obs.Num
  indices.de.outliers.rosner = k.mayores.desviaciones.de.la.media[is.outlier.rosner]
  valores.de.outliers.rosner = datos[indices.de.outliers.rosner]
  cat("\nTest de Rosner")
  cat("\nÍndices de las k-mayores desviaciones de la media: ")
  cat(k.mayores.desviaciones.de.la.media)
  cat("\nDe las k mayores desviaciones, ¿Quién es outlier? ")
  cat(is.outlier.rosner)
  cat("\nLos índices de los outliers son: ")
  cat(indices.de.outliers.rosner)
  cat("\nLos valores de los outliers son: ")
  cat(valores.de.outliers.rosner)
  MiPlot_Univariate_Outliers (datos, indices.de.outliers.rosner, "Test de Rosner")
}



###########################################################################
# Conjuntos de datos 
###########################################################################
wine <- read.csv("wine-5an-nn.dat",, comment.char = "@", header = FALSE)
names(wine) <- c("Alcohol", "MalicAcid", "Ash", "AlcalinityOfAsh", "Magnesium", 
                 "TotalPhenols", "flavanoids", "NonflavanoidsPhenols", "Proanthocyanins", 
                 "ColorIntensity", "Hue", "OD280/OD315", "Proline", "Class")
mydata.numeric  = wine[,-14]  
frame.es.outlier <- sapply(1:ncol(mydata.numeric),function(x) 
  vector_es_outlier_IQR(mydata.numeric,x))
indices.de.outliers.en.alguna.columna <- sapply(1:ncol(frame.es.outlier),function(x)
  vector_claves_outliers_IQR(frame.es.outlier,x))
indices.de.outliers.en.alguna.columna
#La columna 3, tiene 4 outliers, por lo que voy a coger ese conjunto de 
#datos, con 3 de las filas eliminada para probar el Test de Grubbs
datos.con.un.outlier = mydata.numeric[-164,3]
datos.con.un.outlier = datos.con.un.outlier[-116]
datos.con.un.outlier = datos.con.un.outlier[-114]
mydata.numeric = datos.con.un.outlier

###########################################################################
# Test de Grubbs
###########################################################################
# datos.con.un.outlier
# Mostramos el histograma de mydata.numeric usando la función hist
# y un gráfico de puntos con la función plot
# Observamos que hay un dato con un valor extremo
hist(mydata.numeric)
plot(mydata.numeric)

# -------------------------------------------------------------------------
# Aplicamos el test de Grubbs sobre datos.con.un.outlier
# Usamos la función grubbs.test (two.sided = TRUE)
# Guardamos el resultado en test.de.Grubbs y vemos el p.value correspondiente
test.de.Grubbs <- grubbs.test(mydata.numeric,two.sided=TRUE)
test.de.Grubbs$p.value
# Este resultado es significativo con los valores de alpha usuales 0.025, 0.01

# -------------------------------------------------------------------------
# El test de Grubbs es significativo por lo que se concluye que hay un ÚNICO outlier
# El valor que toma lo podríamos obtener a través de la función outlier del 
# paquete outliers
# pero éste no nos dice cuál es el índice correspondiente.
# Por lo tanto, calculamos manualmente cuál es el índice de aquel registro
# que más se desvía de la media de la columna correspondiente.
# Tendremos que usar las funciones abs(valor absoluto), mean(media) y order (para ordenar)
# El resultado lo guardamos en las siguientes variables:
# indice.de.outlier.Grubbs
# valor.de.outlier.Grubbs
alpha = 0.025 
if (test.de.Grubbs$p.value < alpha){
  indice.de.outlier.Grubbs = order(abs(mydata.numeric - mean(mydata.numeric)), decreasing = T)[1]
  valor.de.outlier.Grubbs  = mydata.numeric[indice.de.outlier.Grubbs]
}
indice.de.outlier.Grubbs
valor.de.outlier.Grubbs

# -------------------------------------------------------------------------
# Ahora que sabemos el índice del outlier, podemos usar la función MiPlot_Univariate_Outliers
# Esta función muestra un plot similar al que ya habíamos mostrado, pero usa el color rojo 
# para mostrar el outlier
# Los parámetros son: el conjunto de datos, los índices de los outliers (solo uno en este 
# caso) y el título a mostrar
# MiPlot_Univariate_Outliers = function (datos, indices_de_Outliers, titulo)
MiPlot_Univariate_Outliers(mydata.numeric,indice.de.outlier.Grubbs,"Test de Grubbs")

###########################################################################
# El mismo proceso anterior empaquetado en una función 
###########################################################################
# Llamamos a la función MiPlot_resultados_TestGrubbs
# MiPlot_resultados_TestGrubbs = function(datos)
# Esta función realiza todo el proceso de aplicar el test de Grubbs tal y como hemos 
# hecho anteriormente
# También muestra los resultados: para ello, la función llama directamente a 
# MiPlot_Univariate_Outliers
# El parámetro a pasar a la función MiPlot_resultados_TestGrubbs es el conjunto de datos
MiPlot_resultados_TestGrubbs(mydata.numeric)

###########################################################################
# Volvemos a aplicar el mismo proceso con los otros conjuntos de datos
###########################################################################
###########################################################################
# datos.con.dos.outliers.masking
###########################################################################
mydata.numeric = wine[,-13]
indices.de.outliers.en.alguna.columna
#Elijo la columna 9 ya que es la que tiene dos outliers
datos.con.dos.outliers.masking = mydata.numeric[,9]
# Mostramos un gráfico de puntos con la función plot
# Vemos que hay dos outliers
# Aplicamos el test de Grubbs sobre datos.con.dos.outliers.masking
X11()
mydata.numeric = datos.con.dos.outliers.masking
plot(mydata.numeric)
test.de.Grubbs = grubbs.test(mydata.numeric, two.sided = TRUE)
test.de.Grubbs$p.value
MiPlot_resultados_TestGrubbs(mydata.numeric)
# El resultado no es significativo con ninguno de los valores de alpha usuales (<= 0.05)
# Sin embargo, hay dos outliers. 
# La razón es que se ha producido un efecto de "masking"  
# Ningún outlier es detectado por Grubbs :-(

###########################################################################
# Test de Rosner
###########################################################################
# Hay tests para detectar un número exacto de k outliers, pero no son muy útiles
# Mejor usamos un test para detectar un número menor o igual que k outliers (Rosner)
# Aplicamos el Test de Rosner (rosnerTest) con k=4 sobre datos.con.dos.outliers.masking
# Nos dará un aviso ocasionado por tener pocos datos
# Guardamos el resultado en test.de.rosner 
# El test ordena los valores de mayor a menor distancia de la media y lanza el test de hipótesis
# para ver si hay menos de k=4 outliers.
# MiPlot_Univariate_Outliers
# MiPlot_Univariate_Outliers = function (datos, indices_de_Outliers, titulo){
mydata.numeric = datos.con.dos.outliers.masking
test.de.rosner = rosnerTest(mydata.numeric, k = 4)
is.outlier.rosner = test.de.rosner$all.stats$Outlier
is.outlier.rosner
#     Es un vector de 4 boolean. 
#     Nos indica si son considerados outliers los 4 valores que más se alejan de la media
#     En este caso:
#     [1]  FALSE FALSE FALSE FALSE
#     Todos son FALSE => El test indica que no hay outliers :-(
k.mayores.desviaciones.de.la.media = test.de.rosner$all.stats$Obs.Num
k.mayores.desviaciones.de.la.media
indices.de.outliers.en.alguna.columna[[9]]
#     Es un vector con los cuatro índices de los 4 valores que
#     más se alejan de la media
#     En este caso:
#     [1]  93 126 112 131
# Construimos el vector con los índices de los que son outliers 
# y se lo pasamos como parámetro a la función
indices.de.outliers.rosner = k.mayores.desviaciones.de.la.media[is.outlier.rosner]
valores.de.outliers.rosner = mydata.numeric[indices.de.outliers.rosner]
print("Índices de las k-mayores desviaciones de la media")
k.mayores.desviaciones.de.la.media
print("De los k valores fijados, ¿Quién es outlier?")
is.outlier.rosner 
print("Los índices de los outliers son:")
indices.de.outliers.rosner
print("Los valores de los outliers son:")
valores.de.outliers.rosner
MiPlot_Univariate_Outliers (mydata.numeric, indices.de.outliers.rosner, "Test de Rosner")

#######################################################################
# La función
# MiPlot_resultados_TestRosner = function(datos)
# hace directamente las anteriores tareas, es decir, lanza el test y dibuja el plot.
# Lanzamos esta función con el dataset datos.con.dos.outliers.masking 
# y comprobamos que ofrece los resultados vistos anteriormente
MiPlot_resultados_TestRosner(mydata.numeric)
#Con lo que este test ha fallado en este caso ya que se esperaban 2 outliers, y no ha sido así

#######################################################################
# Para ver el comportamiento del Test de Rosner con el conjunto de datos inicial 
# lanzamos la función MiPlot_resultados_TestRosner con k=4 sobre datos.con.un.outlier
# Test de Rosner
# Índices de las k-mayores desviaciones de la media: 11 18 34 51
# De las k mayores desviaciones, ¿Quién es outlier? TRUE FALSE FALSE FALSE
# Los índices de los outliers son: 11
# Los valores de los outliers son: 1.36
# Número de datos: 175
# ¿Quién es outlier?: FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE TRUE FALSE ...
# El test indica que solo hay un outlier :-)
mydata.numeric = datos.con.un.outlier
MiPlot_resultados_TestRosner(mydata.numeric)

#######################################################################
# Lanzamos también el test de Rosner con k=4 sobre datos.con.varios.outliers
# Mostamos el plot de datos.con.varios.outliers
# Aplicamos el Test de Rosner (rosnerTest) con k=4 sobre datos.con.varios.outliers
# [1]  TRUE  TRUE  TRUE FALSE
# [1] 123 158 50
# Indica que hay tres outliers :-)
indices.de.outliers.en.alguna.columna[[5]]
mydata.numeric  = wine[,-14] 
#Selecciono la columna 5, que tiene 7 outliers y elimino 3, dejando 4
datos.con.varios.outliers = mydata.numeric[-c(14,39,50),5]
mydata.numeric = datos.con.varios.outliers
MiPlot_resultados_TestRosner(mydata.numeric)

