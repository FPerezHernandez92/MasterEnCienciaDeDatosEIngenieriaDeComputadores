#FRANCISCO PÉREZ HERNÁNDEZ 20076629K

#1 MATRICES
#1.1 Ejecuta los siguientes comandos
matrix(data=5, nr=2, nc=2)
matrix(1:6, 2, 3)
matrix(1:6, 2, 3, byrow = TRUE)

#1.2 Crea un vector z con los 30 primeros números y crea con el una matriz m con 3 filas y 10 columnas
e12a = c(1:30)
e12b = matrix(e12a,3,10)

#1.3 Escribe la tercera columna en un vector
e13 = e12b[,3]

#1.4 Crea in R las matrices
e14a = matrix(c(3,-1,21,1),2,2 )
e14b = matrix(c(1,0,4,1,0,-1), 2,3)
e14a
e14b
#Y calcula los efectos de los siguientes comandos
e14a[1,]
e14a[2,]
e14a[,2]
e14b[1,2]
e14b[,2:3]

#1.5 Transforma la matriz m que creástes en el ejercicio anterior en un array multidimensional. Pista: averigua 
#lo que puedas de la función dim().
e12b
help(dim)
e15 = as.array(e12b,dim=30)
(e15)

#1.6 Crea un array x de 5 x 5 y rellénalo con valores del 1 al 25. Investiga la función array(). 
x = array(1:25,dim = c(5,5))
x

#1.7 Escribe el array x en un vector y
e17 = as.vector(x)
e17

#1.8 Dadas las matrices  m1 y m2 usa rbind() y cbind() para crear matrices nuevas. para crear matrices nuevas 
#utilizando estas funciones, llamalas M1 y M2.¿En que se diferncian las matrices creadas?
m1 <- matrix(1,nr=2,nc=2)
m2 <- matrix(2,nr=2,nc=2)
m1
m2
M1 <- rbind(m1,m2)
M1
M2 <- cbind(m1,m2)
M2
#La diferencia viene dada en que parte se añade la segunda matriz

#1.9 El operador para el producto de dos matrices es ‘ %* %’. Por ejemplo, considerando las dos matrices creadas 
#en el ejercicio anterior utilízalo
e19 = M1%*%M2
e19

#1.10 Usa la matriz M1 del ejercicio anterior y aplica la función t(). ¿qué hace esa función?
e110 = t(M1)
M1
e110
#t() hace la traspuesta

#1.11 Ejecuta los siguientes comandos basados en la función diag() sobre las matrices creadas anteriormente m1 y 
#m2. ¿Qué tipo de acciones puedes ejecutar con ella?
m1
m2
diag(m1)
#Obtener la diagonal
diag(rbind(m1,m2)%*%cbind(m1,m2))
#Diagonal de la multiplicación de dos matrices
diag(m1) <- 10
#Cambiar la diagonal de m1 por valores 10
diag(3)
#Crear una matriz de longitud 3 de diagonal
v<-c(10,20,30)
diag(2.1,nr=3,nc=5)
#Crear una matriz 3x5 con diagonal 2.1

#1.12 Crea los siguientes vectores. Los datos se corresponden con las ventas en millones de la trilogía de la 
#guerra de las galaxias. El primer numero corresponde a las ventas en US y el segundo al resto de países.
# Box office Star Wars: In Millions (!)  First element: US, Second element:
# Non-US
new_hope = c(460.998007, 314.4)
empire_strikes = c(290.475067, 247.9)
return_jedi = c(309.306177, 165.8)

#1.12a Construye la matriz star_wars_matrix con  esos vectores
star_wars_matrix = matrix(c(new_hope, empire_strikes, return_jedi),3,2,byrow=TRUE)
#Añádele nombres a las columnas y filas de la matriz según las descripciones dadas anteriormente de los datos
colnames(star_wars_matrix)<-c("Ventas US","Resto Paises")
rownames(star_wars_matrix)<-c("NewHopw","EmpireStrikes","ReturnJedy")
star_wars_matrix

#1.12b Calcula las ganacias  mundiales de cada película y  guardalas en un vector que se llame worldwide_vector
worldwide_vector = c(star_wars_matrix[,1]+star_wars_matrix[,2])
worldwide_vector

#1.12c Añade éste ultimo vector como una columna nueva a la matriz star_wars_matrix y asigna el resultado a 
#all_wars_matrix. Usa para ello la función cbind().
all_wars_matrix = cbind(star_wars_matrix,worldwide_vector)
all_wars_matrix

#1.12d Calcula las ganancias totals en USA y fuera de USA para las tres películas. Puedes usar para ello la 
#función colSums()
help("colSums")
colSums(all_wars_matrix[,1:2])

#1.12e Calcula la media de ganancias para todas las películas fuera de los estados unidos. Asigna esa media la 
#variable non_us_all
non_us_all <- mean(all_wars_matrix[,2])
non_us_all

#1.12f Haz lo mismo pero solo par alas dos primeras películas . Asigna el resultado a la variable non_us_some
non_us_some <- mean(all_wars_matrix[1:2,2])
non_us_some

#1.12g Calcula cuantos visitantes hubo para cada película en cada área geográfica. Ya tienes las ganancias 
#totales en star_wars_matrix. Asume que el precio de las entradas es de cinco euros/dólares (Nota: el numero total 
#de visitantes para cada pelicula dividido por el precio del ticket te da el numero de visitantes)
all_views_wars_matrix = all_wars_matrix/5
all_wars_matrix
all_views_wars_matrix

#1.12h Calcula la media de visitantes en territorio USA y en territorio noUS
star_wars_view = colMeans(all_views_wars_matrix[,1:2])
star_wars_view

#Subsetting matrices y arrays 
#Como hemos visto en teoría la sintaxis para acceder tanto a matrices como a arrays bidimiensionales es la siguiente.
#array[rows, columns] 
#Muchas funciones de R necesitan una matriz como dato de entrada. Si algo no funciona recuerda convertir el objeto 
#a una matriz con la función
#1.13 Crea un array x <- array(c(1:10),dim=c(5,2)). ¿Que información te dan los siguientes comando?
x <- array(c(1:10),dim=c(5,2))
dim(x)
#La dimensión del array
nrow(x)
#Número de filas
ncol(x)
#Número de columnas

#1.14 Crea un array de dimensiones 5 filas y dos columnas y rellénalo con valores del 1-5 y del 5 al 1
i<- array(c(1:5,5:1),dim=c(5,2))
i

#1.15 ¿Qué hace el comando x[i]?. Comprueba que tienes en x antes
x
x[i]
#Buscamos elementos en la matriz x gracias a lo que hay en i

#1.16 ¿y el comando x[i] <- 0?
x[i]<-0
x
#Asignamos 0 a esos valores de la matriz

#1.17 Descárgate el fichero array_datos.txt de SWAD (Practicas/Ficheros) e impórtalo en tu work space de R 
#teniendo en cuenta que es un texto tabulado. Después crea un documento con el mismo pero en formato csv en vez 
#de tab separated
array_datos = read.table("array_datos.txt",sep="\t")
write.csv(array_datos, "array_datos.csv")


#2 ACCESO Y SELECCIÓN DE SECCIONES DE UN DATA FRAMES
#La sintaxis general para acceder a un data frame es 
#my_frame[rows, columns] 
#2.1 Vamos a trabajar con un ejemplo que viene por defecto en la instalación de R USArrests. Este data frame contiene 
#la información para cada estado Americano de las tasas de criminales (por 100.000 habitantes). Los datos de las 
#columnas se refieren a Asesinatos, violaciones y porcentaje de la población que vive en áreas urbanas. 
#Los datos son de 1973. Contesta a las siguientes preguntas sobre los datos
#2.1a Obten las dimensiones del data frame utilizando la función dim()
fix(USArrests)
attach(USArrests)
dim(USArrests)

#2.1b ¿Qué hace la función length cuando se aplica sobre un dataframe?. 
length(USArrests)
#Nos devuelve el número de columnas del dataframe

#2.1c	Obtén el numero  de columnas y el número de filas
ncol(USArrests)
nrow(USArrests)

#2.1d	Obtén el nombre de las filas y las columnas para este data frame
colnames(USArrests)
rownames(USArrests)

#2.1e	échale un vistazo a los datos, por ejemplo a las seis primeras filas
USArrests[1:6,]

#2.1f	Ordena de forma decreciente las filas de nuestro data frame según el porcentaje de población en el área urbana. 
#Para ello investiga la función order() y sus parámetros.
help(order)
USArrests[order(USArrests$UrbanPop,decreasing = TRUE),]

#2.1g	¿Podrías añadir un segundo criterio de orden?, ¿cómo?
#Se podría añadir para que si hay dos valores iguales del primer criterio, que sea este segundo el que haga
#el desempate
USArrests[order(USArrests$UrbanPop,USArrests$Rape,decreasing = TRUE),]

#2.1h	Muestra por pantalla la columna con los datos de asesinato
USArrests[1]

#2.1i	Muestra las tasas de asesinato para el segundo, tercer y cuarto estado 
USArrests$Murder[2:4]

#2.1j	Muestra las primeras cinco filas de todas las columnas
USArrests[1:5,]

#2.1k	Muestra todas las filas para las dos primeras columnas
USArrests[,1:2]

#2.1l Muestra todas las filas de las columnas 1 y 3
USArrests[,c(1,3)]

#2.1m	Muestra solo las primeras cinco filas de las columnas 1 y 2
USArrests[1:5,1:2]

#2.1n	Extrae las filas para el índice Murder
USArrests[1]

#2.1o	Vamos con expresiones un poco mas complicadas: ¿Que estado tiene la menor tasa de asesinatos? 
rownames(USArrests)[which.min(USArrests[,1])]

#2.1p	¿Que estados tienen una tasa inferior al 4%?
rownames(USArrests)[which(USArrests[,1]<4)]

#2.1q	¿Que estados están en el cuartil superior en lo que a población en zonas urbanas se refiere? 
summary(USArrests)
rownames(USArrests)[which(USArrests[,3]>75)]

#2.2 Vamos a trabajar con otro data frame. Descarga el fichero student.txt de la plataforma SWAD, almacena 
#la información en una variable llamada “students”. Ten en cuenta que los datos son tab-delimited y tienen un 
#texto para cada columna. Comprueba que R ha leído correctamente el fichero imprimiendo el objeto en la pantalla
students = read.delim("student.txt")
students

#2.2a Imprime solo los nombres de la columnas
colnames(students)

#2.2b Selecciona sólo a la columna height 
students$height
students[1]

#2.2c ¿Cuantas observaciones hay en cada grupo?. Utiliza la función table(). Este comando se puede utilizar 
#para crear tablas cruzadas (cross-tabulations)
table(students["population"])
table(students["gender"])

#2.2d Crea nuevas variables a partir de los datos que tenemos. Vamos a crear una variable nueva “sym” que contenga 
#M si el genero es masculino y F si el genero es femenino. Busca en la ayuda información sobre la función ifelse(). 
#Crea una segunda variable “colours” cuyo valor será “Blue” si el estudiante es de kuopio y “Red” si es de otro sitio.
sym <- ifelse (students$gender == "male","M","F") 
help(ifelse)
colours <- ifelse(students$population =="kuopio","Blue","Red")

#2.2e Con los datos anteriores de height y shoesize y las nuevas variables crea un nuevo data.frame que se llame 
#students.new
students.new <- data.frame(students$height,students$shoesize,sym,colours)

#2.2f Comprueba que la clase de student.new es un dataframe
is.data.frame(students.new)

#2.2g Crea dos subsets a partir del dataset student. Divídelo dependiendo del sexo. Para ello primero comprueba 
#que estudiantes son hombres (male). Pista: busca información sobre la función which.
which(students$gender == "male")
which(students$gender == "female")

#2.2h Basándote en esa selección dada por which toma solo esas filas del dataset student para generar el subset 
#stundent.male
students.male <- students[which(students$gender=="male"),]
students.male

#2.2i Repite el procedimiento para seleccionar las estudiantes mujeres (females)
students.female <- students[which(students$gender=="female"),]
students.female

#2.2j Utiliza la function write.table() para guarder el contenido de student.new en un archivo.
write.table(students.new,file = "studentesnew.txt")


#3	COMO CREAR UN DATA FRAME DESDE CERO EN R
#3.1 Crea os siguientes vectores:
employee <- c('John Doe','Peter Gynn','Jolie Hope') 
salary <- c(21000, 23400, 26800) 
startdate <- as.Date(c('2010-11-1','2008-3-25','2007-3-14'))
#3.1a	¿tienen los tres vectores la misma clase?¿que clases son?
class(employee)
class(salary)
class(startdate)

#3.1b	Combina los vectores en un data frame que se llame employ.data
employ.data <- data.frame(employee,salary,startdate)

#3.1c	Muestra por pantalla la estructura del nuevo data frame
employ.data
str(employ.data)

#3.1d ¿ves algo extraño cuando miras a la estructura de employ.data? ¿Que?
#Que la clase employee ha pasado de character a factor

#3.1e A pesar de que el vector employee es un vector de tipo carácter, R por defecto lo ha convertido en un factor. 
#Puedes evitar esta transformación automática utilizando la función stringsAsFactors() y declarandola FALSE. 
#Prueba y comprueba la diferencia al llamar a la function str() sobre el nuevo employ.data OJO: este es un 
#grave error así que procura usar siempre la function antes mencionada para forzar vectores y matrices como 
#caracteres cuando sea necesario.
options(stringsAsFactors = FALSE)
employ.data <- data.frame(employee,salary,startdate)
str(employ.data)


#4 COMO PREPARAR DATOS EN R 
#4.1 Preparemos el data frame mtcars() que viene por defecto en R:
#4.1a Selecciona las variables 1,2,9 y 10 de mtcars() y asignalas a un data frame llamado cars. Utiliza 
#estos numerous como indices para extraer las variables.
mtcars
cars <- mtcars[,c(1,2,9,10)]
cars
is.data.frame(cars)

#4.1b Convierte la variable gear en cars en un factor ordenado
str(cars)
cars[,"gear"] <- as.ordered(cars[,"gear"])
str(cars)

#4.1c Transforma la variable am de cars en un factor dónde su valor sea “auto” si su valor original era 1 y 
#“manual” sies era 0. PISTA: mira la función ifelse(). ¿Cómo puedes usar la propiedad de esta función para 
#escribir tu código
help(ifelse)
cars[,"am"] <- ifelse(cars$am == "1", "auto", "manual")
str(cars)
cars[,"am"] <- as.factor(cars[,"am"])
str(cars)

#4.1d Comprueba la estructura de tu set de datos. Describe tu nuevo data frame
str(cars)
summary(cars)

