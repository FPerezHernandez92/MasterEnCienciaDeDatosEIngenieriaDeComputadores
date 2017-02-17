setwd("~/Dropbox/zMaster/zRStudio/Master-en-Ciencia-De-Datos-e-Ingeniería-de-Computadores/2 Mineria de datos, preprocesamiento y clasificacion")
#Librerias:
library(MASS)
library(class)
library(caret)
library(mice)
library(lattice)
library(randomForest)
library(party)

#Leemos el data set original
accidentes.train.original <- read.csv("accidentes-kaggle.csv")
accidentes.test.original <- read.csv("accidentes-kaggle-test.csv")

#Realizamos 

accidentes.train.solo.numericos <- accidentes.train.original[,c(8,9,10,11,12,30)]
accidentes.test.solo.numericos <- accidentes.test.original[,c(8,9,10,11,12)]
set.seed(1234)
ct <- ctree(TIPO_ACCIDENTE ~., accidentes.train.solo.numericos)
# se realiza la prediccion
testPred <- predict(ct, newdata = accidentes.test.solo.numericos)




daaa <- as.matrix(testPred)
write.table(daaa,file="tt.txt",sep=",",quote = F)




##### VALORES PERDIDOS
#Buscamos el porcentaje de valores perdidos por fila
porcentaje.de.valores.perdidos.por.fila.train <- apply(accidentes.train.original, 1, function(x) sum(is.na(x))) / ncol(accidentes.train.original) * 100
filas.train.con.valores.perdidos <- (porcentaje.de.valores.perdidos.por.fila.train > 0)
sum(filas.train.con.valores.perdidos)
porcentaje.de.valores.perdidos.por.columna.train <- apply(accidentes.train.original,2,function(x) sum(is.na(x))) / ncol(accidentes.train.original) * 100
columnas.train.con.valores.perdidios <- (porcentaje.de.valores.perdidos.por.columna.train > 0)
accidentes.train.sin.columnas.na <- accidentes.train.original[,!columnas.train.con.valores.perdidios]
porcentaje.de.valores.perdidos.por.fila.test <- apply(accidentes.test.original, 1, function(x) sum(is.na(x))) / ncol(accidentes.test.original) * 100
filas.test.con.valores.perdidos <- (porcentaje.de.valores.perdidos.por.fila.test > 0)
sum(filas.test.con.valores.perdidos)
accidentes.test.sin.columna.na <- accidentes.test.original[,!columnas.train.con.valores.perdidios[1:29]]
porcentaje.de.valores.perdidos.por.columna.test <- apply(accidentes.test.original, 2, function(x) sum(is.na(x))) / ncol(accidentes.test.original) * 100
(porcentaje.de.valores.perdidos.por.columna.test > 0)
#Tenemos una gran cantidad de filas con valores perdidos, por lo que no podemos eliminarlos

#Imputación de valores perdidos
#Vamos a ver el patrón de aparción de los valores perdidos
datos <- accidentes.train.original
patron <- mice::md.pattern(x=datos)
#Se genera el grafico de distribucion de datos perdidos. Solo se consideran las variables con datos perdidos
variables.con.datos.perdidios <- apply(datos,2,function(x) sum(is.na(x)))
variables.con.datos.perdidos.numero <- (variables.con.datos.perdidios > 0)
columnas.con.valores.perdidos <- c(1:length(variables.con.datos.perdidos.numero))[variables.con.datos.perdidos.numero]
aggr_plot <- VIM::aggr(datos[,columnas.con.valores.perdidos], col=c('blue','red'), numbers=TRUE, 
                       sortVars=TRUE, labels=names(data), cex.axis=.5, 
                       gap=1, ylab=c("Grafico de datos perdidos","Patron"))
#Se muestra informacion sobre algunas de las variables en que aparecen los datos perdidos
variables.valores.perdidos <- accidentes.train.original[,columnas.con.valores.perdidos]
VIM::marginplot(datos[,c(columnas.con.valores.perdidos[1],columnas.con.valores.perdidos[2])])

#Usemos el paquete MICE para imputar valores
filas.completas <- mice::ccn(datos)
filas.incompletas <- mice::icn(datos)
cat ("Datos completos: ", filas.completas, " e imcompletos: ", filas.incompletas, "\n")
#Se realiza la imputación
#Dado que la imputación es un proceso costoso y me ha supuesto mucho tiempo de cálculo, he divido el dataset
accidentes.train.original.p1 <- accidentes.train.original[1:6000,]
accidentes.train.original.p2 <- accidentes.train.original[6001:12000,]
accidentes.train.original.p3 <- accidentes.train.original[12001:18000,]
accidentes.train.original.p4 <- accidentes.train.original[18001:24000,]
accidentes.train.original.p5 <- accidentes.train.original[24001:30002,]
set.seed(1234)
datos.imputados <- mice::mice(accidentes.train.original.p1, m=1, maxit=1, method="pmm")
#Vemos el número de instancias sin datos perdidos y con datos perdidos en la parte ya limpia
filas.completas.despues.mice <- mice::ccn(datos.imputados)
filas.incompletas.despues.mice <- mice::cnn(datos.imputados)
cat ("Datos completos después imputación: ", filas.completas.despues.mice, 
     " e imcompletos después imputación: ", filas.incompletas.despues.mice, "\n")
#Veamos como se distribuyen los datos imputados:
xyplot(datos.imputados, columnas.con.valores.perdidos[1]~columnas.con.valores.perdidos[-1], pch=18,cex=1)


#Selecciono los conjuntos de datos a clasificar
accidentes.train.modelos <- accidentes.train.sin.columnas.na
accidentes.test.modelos <- accidentes.test.sin.columna.na

#LM
set.seed(1234)
ldaFit <- train(train.set,train.set.label,
                method="lda",metric="Accuracy",preProcess=c("center","scale"),
                tuneLength=10,trControl=trainControl(method="cv",number=10))
summary(ldaFit)
dim(accidentes.train.modelos)[2]
train.set <- accidentes.train.modelos[,-(dim(accidentes.train.modelos)[2])]
train.set.label <- accidentes.train.modelos[,dim(accidentes.train.modelos)[2]]
test.set <- accidentes.test.modelos
knn.accidentes <- knn(train=train.set, test=test.set,cl=train.set.label,k=1)

#se aprende el modelo: random forest. Podemos especificar el
# numero de arboles a incluir en el bosque
data("GermanCredit")
modelo <- randomForest::randomForest(Class ~ ., data=GermanCredit, ntree=10)

# se muestra informacion del modelo
print(modelo)

# muestra la importancia de los atributos, teniendo en cuenta
# el modelo construido
randomForest::importance(modelo)

# se muestra informacion sobre los errores para cada una de las
# clases: la linea en negro indica el error medio 
randomForest::plot(modelo)

# se aprende otrol modelo con mas arboles
modelo2 <- randomForest(TIPO_ACCIDENTE ~ ., data=accidentes.train.modelos, ntree=100)

# se muestran los errores para cada etiqueta de la variable clase
plot(modelo2)


posicionClase <- length(names(accidentes.train.modelos))
variableClase <- names(accidentes.train.modelos)[posicionClase]
formulaClase <- as.formula(paste(variableClase,"~.",sep=""))

ct <- ctree(formulaClase, accidentes.train.modelos)

