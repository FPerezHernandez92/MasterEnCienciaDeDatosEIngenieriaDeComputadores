#FRANCISCO PÉREZ HERNÁNDEZ 20076629K

#Dudas:
  #5.1m(no hay enunciado),5.1n(falta el anterior)

# EJERCICIO 1: LISTS
# Las listas son colecciones de objetos que pueden tener modos diferentes (e.g. numéricos, vectores, arrays..)
# Ejemplo de cómo crear una lista. Ejecuta los comandos y describe que es lo que ocurre
my_list <- list(name="Fred", wife="Mary", no.children=3, child.ages=c(4,7,9)) 
#Creamos una lista con 4 atributos 
attributes(my_list); 
#Obtenemos los atributos
names(my_list) 
#Obtiene los nombres de los atributos
my_list[2]; 
#Nos da el segundo atributo de la lista
my_list[[2]]
#Obtenemos el elemento del segundo atributo
my_list$wife
#Obtenemos el elemento del atributo wife
my_list[[4]][2] 
#Obtenemos del último atributo el segundo elemento
length(my_list[[4]]) 
#Obtenemos el tamaño del último atributo de la lista
my_list$wife <- 1:12 
#En el atributo wife añadimos un vector del 1 al 12
my_list$wife <- NULL
#En el atributo wife lo hacemos inválido
my_list <- c(my_list, list(my_title2=month.name[1:12]))
#A my_list le añadimos una lista con nombre my_title2 con el nombre de los 12 meses del año 
unlist(my_list); 
help(unlist)
#Con unlist obtenemos toda la estructura de my_list
data.frame(unlist(my_list));
aux <- data.frame(unlist(my_list));
is.data.frame(aux)
#Creamos un data.frame con la estructura de my_list
matrix(unlist(my_list))
#Creamos una matriz con la estructura de my_list

#EJERCICIO 2: APPLY()
#Utilizando el siguiente dataset
myMA <- matrix(rnorm(100000), 10000, 10, dimnames=list(1:10000, paste("C", 1:10, sep=""))) 
#2.1 Calcula la media para fila en la matriz myMA
mean_myMA_fil <- apply(myMA,1,mean)
mean_myMA_fil

#2.2 Calcula la media para cada columna en la matriz
mean_myMA_col <- apply(myMA,2,mean)
mean_myMA_col

#2.3 Calcula las filas de la matriz anterior que contienen missing values NA. Antes de calcularlos recuerda (na.rm=T).
which(apply(myMA, 1, anyNA))


#EJERCICIO 3: SAPPLY()
#3.1 Dado el vector d1 
d1=cbind(c(1:5),c(1,5,2,7,14),c(11:15))
d1

#3.1a Sustituye los números 1,2,7 y 14 por su palabra en alemán “eins”,”zwei”,”sieben” y “vierzehn” utilizando sapply().
a_aleman = function(x){
  if (x==1)
    x = "eins"
  else if(x==2)
    x = "zwei"
  else if(x==7)
    x = "sieben"
  else if (x==14)
    x = "vierzehn"
  else
    x
}
sapply(d1,a_aleman)

#3.1b Carga la librería lattice y mira que dimensión tiene el dataset barley y que tipo de objeto es.
library(lattice)
summary(barley)
dim(barley)
class(barley)

#3.1c Averigua cuantos campos componen el objeto
names(barley)
attributes(barley)

#3.1d Obtén con lapply() la longitud de cada uno de los elementos de la lista. Sugerencia: unique()
help(unique)
fix(barley)
list_length <- lapply(barley,FUN = unique)
list_length

#3.1e Obtén el resultado anterior en un vector
help(vapply)
vector_lenght <- as.vector(lapply(barley,FUN=unique))
vector_lenght


#EJERCICIO 4: TABLE()
#4.1 La función  table() cuenta el numero de elementos repetidos en un vector. Es la función más básica de clustering.
#Cuenta el numero de entradas idénticas en la variable  Sepal.Length del dataset iris.
entras_sepal_length <- table(iris$Sepal.Length)
entras_sepal_length
sum(entras_sepal_length)
length(iris$Sepal.Length)


#EJERCICIO 5: EXPLORATORY DATA ANALYSIS
#5.1 hip dataset
#5.1a Descargate el  dataset hip con el siguiente commando 
hip  <-read.table("http://astrostatistics.psu.edu/datasets/HIP_star.dat", header=T,fill=T)

#5.1b Una vez descargado comprueba la dimensión y los nombres de las columnas del dataset. ¿Qué dimensión tiene? 
#¿qué datos alberga?
dim(hip)
names(hip)
summary(hip)
apply(hip,2,class)

#5.1c Muestra por pantalla la columna de la variable RA
hip$RA
hip[3]

#5.1d Calcula las tendencias centrales de todos los datos del dataset (mean, media) utilizando la function apply
apply(hip,2,mean,na.rm=TRUE)
apply(hip,2,median,na.rm=TRUE)

#5.1e Haz lo mismo para las medidas de dispersión mínimo y máximo. ¿Seria posible hacerlo con un único comando?
#¿Que hace la función range()
apply(hip,2,min)
apply(hip,2,max)
summary(hip)
help(range)
#Range devuelve un vector con el mínimo y máximo de los argumentos dados
apply(hip,2,range,na.rm=TRUE)

#5.1f Sin embargo las medidas mas populares de dispersión son la varianza (var()), su desviación standard (sd()) y 
#la desviación absoluta de la mediana o MAD. Calcula estas medidas para los valores de RA
var(hip$RA)
sd(hip$RA)
mad(hip$RA)
median(hip$RA)

#5.1g Imagina que quieres calcular dos de estos valores de una sola vez. ¿Te serviría este código?
f = function(x) c(median(x), mad(x))  
f(hip[,1])
f(hip$RA)
#Si funcionaría

#5.1h ¿Cuál sería el resultado de aplicar apply(hip,2,f)?
apply(hip,2,f)
#Calcular para cada columna el valor de la media y de la desviación absoluta de la mediana

#5.1i Vamos a medir la dispersión de la muestra utilizando el concepto de cuartiles. El percentil 90 es aquel 
#dato que excede en un 10% a todos los demás datos. El cuartil (quantile) es el mismo concento, solo que habla 
#de proporciones en vez de porcentajes. De forma que el percentil 90 es lo mismo que el cuartil 0.90. La 
#mediana “median” de un dataset es el valor más central, en otras palabras exactamente la mitad del dataset excede 
#la media. Calcula el cuartil .10 y .50 para la columna RA del dataset hip. Sugerencia: quantile()
help(quantile)
quantile(hip$RA,probs=c(0.1,0.5))

#5.1j Los cuantiles 0.25 y 0.75 se conocen como el  first quartile y el third quartile, respectivamente. 
#Calcula los cuatro cuartiles para RA con un único comando.
quantile(hip$RA,probs=c(0.1,0.25,0.5,0.75))

#5.1k Otra medida de dispersion es la diferencia entre el primer y el tercer cuartil conocida como rango 
#intercuartil (IQR) Inter Quantile Range. ¿Obtienes ese valor con la función summary()?
summary(hip)
IQR(hip$RA)
#Con summary no se obtiene ese valor

#5.1l Hasta ahora has ignorado la presencia de  valores perdidos NA. La función any() devuelve TRUE si se 
#encuentra al menos un TRUE en el vector que damos como argumento. Su combinación con is.na es muy útil. 
#¿qué obtienes cuando ejecutas el siguiente comando? ¿Cómo lo interpretas?
hasNA = function(x) any(is.na(x)) 
apply(hip,2,hasNA)   
#Podemos saber que en la última columna tenemos valores NA ya que como resultado obtenemos que en la columna
#B.V tenemos un TRUE

#5.1m Prueba a ejecutar el siguiente comando.

#5.1n Como has observado nos devuelve NA para toda la columna, normalmente querríamos poder usar la función 
#sobre el resto de datos que no son NA: Para ello podemos utilizar la función na.omit. ¿Que ocurre cuando lo 
#hacemos?. Usando apply calcula la media para hip y hip1. Intenta calcular la media de forma que solo cambie la 
#de B.V cuando ignores los valores NA.
apply(na.omit(hip),2,mean)

#5.1o Obten una idea aproximada de tus datos mediante la creación de un boxplot del hop dataset
boxplot(hip)
boxplot(hip[-1])

#5.1p Crea un scatterplot que te compare los valores de RA y DE. Representa los puntos con el símbolo ‘.’ Y que 
#estos puntos sean de color rojo si DE excede de 0. Sugerencia ifelse()
plot(hip$RA, hip$DE, main="Scatterplot", xlab="RA ", ylab="DE", pch=20, col=(ifelse (hip$DE>0, "red","black")))

#5.1q Haz un scatterplot de RA y pmRA. ¿Ves algún patrón?
plot(hip$RA, hip$pmRA, main="Scatterplot", xlab="RA ", ylab="pmRA", pch=20, col="black")
#Se puede decir que hay correlación entre las variables de forma lineal

#5.1r En vez de crear los plots por separado para cada par de columnas, hazlos con un solo comando con el 
#scatterplot matrix
pairs(hip)
library(car)
scatterplot.matrix(hip)

#5.1s Para poder acceder a las variables por su nombre usa attach(hip).Vamos a seleccionar las estrellas 
#Hyadas del dataset aplicando los siguientes filtros:
attach(hip)
#5.1s1 RA in the range (50,100) 
new_RA = RA >50 & RA < 100
#5.1s2 DE in the range (0,25) 
new_DE = DE>0 & DE<25
#5.1s3 pmRA in the range (90,130) 
new_pmRA = pmRA>90 & pmRA<130
#5.1s4 pmDE in the range (-60,-10) 
new_pmDE = pmDE>(-60) & pmDE <(-10)
#5.1s5 e_Plx <5 
new_e_Plx = e_Plx<5
#5.1s6 Vmag >4 OR B.V <0.2 (this eliminates 4 red giants) 
new_Vmag_BV = Vmag>4 | B.V<0.2

#5.1t Crea un nuevo dataset con la aplicación de estos filtro. El Nuevo dataset se llama hyades. 
#¿Que dimensiones tiene? Grafica un scatterplot de Vmag vs B.V
new_prueba = new_DE & new_e_Plx & new_pmDE & new_pmRA & new_RA & new_Vmag_BV
hyades <- subset(hip,new_prueba)
names(hip)
dim(hyades)
plot(hyades$Vmag,hyades$B.V,main="Scatterplot Vmag vs B.V",col="blue",pch=20)


#5.2 Ejemplo 2, iris dataset
#5.2a Vamos a utilizar el ejemplo del dataset iris que está incluido en la distribución de R. Este dataset fue 
#creado por Douglas Fisher.  Consta de tres clases y tipos de 3 clases de tipos de flores:
#_setosa_
#_virginica_
#_versicolor_
#Cada una de ellas con cuatro atributos:
#sepal width
#sepal length
#petal width
#petal length
#Inspecciona las primeras filas del dataset y calcula el summary() del mismo con cada atributo del dataset
attach(iris)
iris
fix(iris)
summary(iris)
head(iris)

#5.2b Crea un histograma de petal.width , teniendo en cuenta que el numero de bins es variable fija este 
#a 9. Añádele color y nombres al eje x "Petal Width"y al gráfico dale el nombre de  "Histogram of Petal Width". 
#Crea un histograma para cada variable
histogram(Petal.Width,xlab="Petal Width",main="Hitogram of Petal Width",breaks=9)
histogram(Sepal.Length,xlab="Sepal Length",main="Hitogram of Sepal Length",breaks=9)
histogram(Sepal.Width,xlab="Sepal Width",main="Hitogram of Sepal Width",breaks=9)
histogram(Petal.Length,xlab="Petal Length",main="Hitogram of Petal Length",breaks=9)
histogram(Species,xlab="Species",main="Hitogram of Species")


#5.2c Crea los cuartiles del dataset
quantile(Petal.Length)
quantile(Petal.Width)
quantile(Sepal.Width)
quantile(Sepal.Length)
sapply(iris,function(x) quantile(as.numeric(x)))

#5.2d Representa en un boxplot la variable de ancho de hoja dependiendo del tipo de hoja que tengan
boxplot(Sepal.Width ~ Species,ylab="Sepal Wicth")

#5.2e Crea los cuartiles para cada tipo de iris y represéntalos en un plot como líneas cada una de un color
q<-sapply(iris[1:4], function(x) quantile(x))
plot(q[,1], type="l",col="red", ylab="")
par(new = TRUE)
plot(q[,2], type="l",col="blue", ylab="")
par(new = TRUE)
plot(q[,3], type="l",col="green", ylab="")
par(new = TRUE)
plot(q[,4], type="l",col="purple", ylab="")
legend(1, 2.5, c(names(iris[1]), names(iris[2]), names(iris[3]), 
                 names(iris[4])),col=c("red", "blue", "green", "purple"), lty = 1)

#5.2f Crea los boxplot de la longitud del pétalo en función de la especie de Iris.
boxplot(Petal.Length~Species,ylab="Petal Length")

#5.2g Compara con scatter plots las variables entre sí.
pairs(iris)


#EJERCICIO 6: COMO CREAR SUBGRUPOS DE DATOS EN R
#6.1 Busca información sobre la function cut(). Para ilustrar su uso vamos a utilizar el dataset state.x77. Si 
#no lo tienes instalado instala el paquete R-Datasets. Usa la función head() para ver como son tus datos..
help(cut)
fix(state.x77)
head(state.x77)

#6.2 Extrae la columna Frost y asigna el resultado a la variable frost
frost <- state.x77[,"Frost"]

#6.3 Tu Nuevo objeto es un vector numérico
is.numeric(frost)

#6,4 Ahora intenta agrupar los datos en frost en tres niveles. Para crear bins en tus datos puedes utilizar 
#la función cut(). 
corte <- cut(frost,breaks=3)

#6.5 ¿Que obtienes como nombres de los niveles?
#Se obtiene un factor de tres niveles donde su nombre es el intervalo al que pertenece su valor
frost[corte]

#6.6 En la realidad no existen estados que tengan frost en días negativos. Esto es porque R añade un poco de 
#padding. Prueba a solucionar el problema utilizando el parámetro include.lowest=TRUE en cut()
corte2 <- cut(frost,breaks=3,include.lowest = TRUE)

#6.7 Los nombres de los niveles no son demasiado informativos, especifica nuevos nombres para los niveles
corte <- factor(corte,labels=(c("Uno","Dos","Tres")))

#6.8 Después de este paso has creado un factor que clasifica los estados en bajo, medio y alto según el numero 
#de heladas.
corte <- factor(corte,labels=(c("Bajo","Medio","Alto")))

#6.9 Ahora cuenta el número de estados que  hay en cada uno de los niveles. PISTA: utiliza la función table()
table(corte)


#EJERCICIO 7: COMO ORDENAR DATOS, HACER SELECCIONES CON IF, CALCULAR CONDICIONALES TOTALES, TRANSPONER COLUMAS Y FILAS
#Vamos a volver a utilizar el datasets mtcars. 
#7.1 Ordena este data set de forma ascendente según su valo de hp. PISTA: with()
attach(mtcars)
fix(mtcars)
summary(mtcars)
help(with)
coches <- mtcars[with(mtcars,order(hp)),]
head(mtcars)
head(coches)

#7.2 Calcula la media de la columna mpg. 
mean(coches$mpg)

#7.3 Calcula la media de mpg para aquellos datos cuyo valor de hp sea menor que 150 y por separado para aquellos 
#cuyo valor de hp sea mayor o igual a 150
menor150 = coches$hp < 150
mean(coches$hp[menor150])
mean(coches$hp[!menor150])

#7.4 Busca los valores únicos de la columna cyl de mtcars. PISTA unique()
unique(coches$cyl)

#7.5 Obten los datos de mpg cyl disp hp para “Toyota Corolla"
summary(coches)
coches["Toyota Corolla",c("mpg","cyl","disp","hp")]

#7.6 Crea una nueva variable mpgClass de tipo categórico cuyo valor es “Low“ si el valor de mpg es menor que 
#la media de la columna mpg y “High” si es mayor que la media de mpg. PISTA ifelse(). Combina ese comando 
#con with() para añadir la nueva variable a mtcars
mpgClass <- (ifelse(coches$mpg<(median(coches$mpg)),"Low","High"))
mpgClass
median(coches$mpg)
coches$mpg
mtcars$mpgClass <- mpgClass
summary(mtcars)

#7.7 ¿qué pasa cuando ejecutas este comando? 
with(mtcars, tapply(hp, list(cyl, gear), mean))
#tapply obtiene primero un vector y segundo una lista de dos objetos a los que les calcula la media



