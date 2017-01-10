##################### C) CLASIFICACIÓN ########################

library(class)
library(kknn)
library(caret)
library(MASS)
rm(list=ls())
australian <- read.csv("australian/australian.dat", comment.char = "@", header = FALSE)
attach(australian)
############################ KNN ##############################

##Examinamos la estructura del dataset
str(australian)
##Vemos el número de variables para cada clase
table(australian$V15)
##Pasamos la V15 a factor
australian$V15 <- factor(australian$V15,levels = c(0,1), label = c(0,1))
##Vemos el porcentaje de cada clase para el dataset
round(prop.table(table(australian$V15))*100,digits=1)
##Normalizamos toda la base de datos
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
australian_n <- as.data.frame(lapply(australian[1:14],normalize))
##Visualizamos variables
plot(australian[,1:14])
plot(australian_n[,1:14],col=australian[,15])
##Calculamos la correlación entre variables
cor(australian[,1:14])
##Vemos que puede haber correlación entre las variables V10 y V9
##Creamos el conjunto de training y de test y sus etiquetas
tamanio_tt <- nrow(australian)*0.8
australian_train <- australian_n[1:tamanio_tt,-15]
australian_test <- australian_n[(tamanio_tt+1):nrow(australian),-15]
australian_train_labels <- australian[1:tamanio_tt,15]
australian_test_labels <- australian[(tamanio_tt+1):nrow(australian),15]
##Entrenamos un modelo de los datos
set.seed(1234)
australian_test_pred <- knn(train=australian_train, test=australian_test,cl=australian_train_labels,k=21)
##Evaluamos el modelo
table(australian_test_pred,australian_test_labels)
mean(australian_test_pred==australian_test_labels)
##Por lo que obtenemos un modelo que explica el 85,5% del problema
set.seed(1234)
australian_test_pred <- knn(train=australian_train, test=australian_test,cl=australian_train_labels,k=7)
##Evaluamos el modelo
table(australian_test_pred,australian_test_labels)
mean(australian_test_pred==australian_test_labels)
##Por lo que obtenemos un modelo que explica el 84,7% del problema

##Con caret podemos también hacer un modelo knn
require(caret)
set.seed(1234)
knnModel <- train(x = australian[1:tamanio_tt,-15], y = australian[1:tamanio_tt,15],method = "knn", preProc = c("center","scale"))
knnModel
##Siendo el mejor modelo k=9
knnPred <- predict(knnModel,newdata = australian[(tamanio_tt+1):nrow(australian),-15])
testData = australian[(tamanio_tt+1):nrow(australian),15]
postResample(pred=knnPred, obs = testData)
##Si aplicamos el conjunto de test sobre el modelo creado tenemos un modelo con el 84,05% de explicación
##Si lo hacemos con los datos normalizados como debería ser
set.seed(1234)
knnFit <- train(australian_train, australian_train_labels, method="knn", metric="Accuracy", tuneGrid = data.frame(.k=1:15))
knnFit
##Si le decimos los valores de k que debe tomar, vemos como con k = 15 conseguimos el mejor modelo
knnPred <- predict(knnFit, newdata = australian_test)
postResample(pred=knnPred, obs = australian_test_labels)
##Consiguiendo un model con el 82,6% de explicación del problema

##Ahora si aplico estas funciones a los conjuntos de train y test facilitados podemos tener:
##Primero creo las dos funciones para hacer todo el proceso de forma automática
aplicar_metodo_fold <- function(train,train_labels,test,test_labels,metodo="knn"){
  if (metodo == "knn"){
    knnFit <- train(train, train_labels, method="knn", metric="Accuracy", tuneGrid = data.frame(.k=1:15))
    knnPred <- predict(knnFit, newdata = test)
    #print(postResample(pred=knnPred, obs = test_labels))
    postResample(pred=knnPred, obs = test_labels)
  }
  else if (metodo == "glm"){
    glmFit <- train(train, train_labels, method="glm", metric="Accuracy",trControl=trainControl(method="cv",number=10))
    glmPredict <- predict(glmFit, newdata = test)
    salida <- confusionMatrix(data = glmPredict, reference = test_labels)
    matriz_confusion <- table(glmPredict,test_labels)
    #print(mean(glmPredict==test_labels))
    mean(glmPredict==test_labels)
  }
  else if (metodo == "lda"){
    ldaFit <- train(train,train_labels,
                    method="lda",metric="Accuracy",preProcess=c("center","scale"),
                    tuneLength=10,trControl=trainControl(method="cv",number=10))
    ldaPredict <- predict(ldaFit, newdata = test)
    salida <- confusionMatrix(data = ldaPredict, reference = test_labels)
    matriz_confusion <- table(ldaPredict,test_labels)
    #print(mean(ldaPredict==test_labels))
    mean(ldaPredict==test_labels)
  }
  else if (metodo == "qda"){
    qdaFit <- train(train,train_labels,
                    method="qda",metric="Accuracy",preProcess=c("center","scale"),
                    tuneLength=10,trControl=trainControl(method="cv",number=10))
    qdaPredict <- predict(qdaFit, newdata = test)
    salida <- confusionMatrix(data = qdaPredict, reference = test_labels)
    matriz_confusion <- table(qdaPredict,test_labels)
    #print(mean(qdaPredict==test_labels))
    mean(qdaPredict==test_labels)
  }
}
run_metodo_fold <- function(i, x,metodo = "knn", tt="test",modelo=0){
  file <- paste(x, "-10-", i, "tra.dat", sep="")
  x_tra <- read.csv(file, comment.char="@")
  In <- length(names(x_tra)) - 1 
  names(x_tra)[1:In] <- paste ("X", 1:In, sep="") 
  names(x_tra)[In+1] <- "Y"
  x_tra <- data.frame(lapply(x_tra, normalize))
  x_tra$Y <- factor(x_tra$Y, levels = c(0, 1),labels = c("0", "1"))
  file <- paste(x, "-10-", i, "tst.dat", sep="")
  x_tst <- read.csv(file, comment.char="@")
  names(x_tst)[1:In] <- paste ("X", 1:In, sep="") 
  names(x_tst)[In+1] <- "Y"
  x_tst <- data.frame(lapply(x_tst, normalize))
  x_tst$Y <- factor(x_tst$Y, levels = c(0, 1), labels = c("0", "1"))
  ultimo = 15
  if (modelo == 1){
    x_tra[,9] = x_tra[,9]*x_tra[,10]
    x_tra[,10]=NULL
    x_tst[,9] = x_tst[,9]*x_tst[,10]
    x_tst[,10]=NULL
    ultimo = 14
  }
  if (modelo == 2){
    x_tra[,9] = x_tra[,9]*x_tra[,10]
    x_tra[,10]=NULL
    x_tst[,9] = x_tst[,9]*x_tst[,10]
    x_tst[,10]=NULL
    x_tra[,5] = x_tra[,5]*x_tra[,6]
    x_tra[,6]=NULL
    x_tst[,5] = x_tst[,5]*x_tst[,6]
    x_tst[,6]=NULL
    ultimo = 13
  }
  if (modelo == 3){
    x_tra[,9] = x_tra[,9]*x_tra[,10]
    x_tra[,10]=NULL
    x_tst[,9] = x_tst[,9]*x_tst[,10]
    x_tst[,10]=NULL
    x_tra[,5] = x_tra[,5]*x_tra[,6]
    x_tra[,6]=NULL
    x_tst[,5] = x_tst[,5]*x_tst[,6]
    x_tst[,6]=NULL
    x_tra[,3]=NULL
    x_tst[,3]=NULL
    x_tra[,1]=NULL
    x_tst[,1]=NULL
    ultimo = 11
  }
  if (modelo == 4){
    x_tra[,12]=NULL
    x_tst[,12]=NULL
    x_tra[,11]=NULL
    x_tst[,11]=NULL
    x_tra[,9] = x_tra[,9]*x_tra[,10]
    x_tra[,10]=NULL
    x_tst[,9] = x_tst[,9]*x_tst[,10]
    x_tst[,10]=NULL
    x_tra[,5] = x_tra[,5]*x_tra[,6]
    x_tra[,6]=NULL
    x_tst[,5] = x_tst[,5]*x_tst[,6]
    x_tst[,6]=NULL
    x_tra[,3]=NULL
    x_tst[,3]=NULL
    x_tra[,2]=NULL
    x_tst[,2]=NULL
    x_tra[,1]=NULL
    x_tst[,1]=NULL
    ultimo = 8
  }
  if (modelo == 5){
    x_tra[,12]=NULL
    x_tst[,12]=NULL
    x_tra[,11]=NULL
    x_tst[,11]=NULL
    x_tra[,9] = x_tra[,9]*x_tra[,10]
    x_tra[,10]=NULL
    x_tst[,9] = x_tst[,9]*x_tst[,10]
    x_tst[,10]=NULL
    x_tra[,8] = x_tra[,8]*x_tra[,9]
    x_tra[,9] = NULL
    x_tst[,8] = x_tst[,8]*x_tst[,9]
    x_tst[,9] = NULL
    x_tra[,5] = x_tra[,5]*x_tra[,6]
    x_tra[,6]=NULL
    x_tst[,5] = x_tst[,5]*x_tst[,6]
    x_tst[,6]=NULL
    x_tra[,3]=NULL
    x_tst[,3]=NULL
    x_tra[,2]=NULL
    x_tst[,2]=NULL
    x_tra[,1]=NULL
    x_tst[,1]=NULL
    ultimo = 7
  }
  if (tt == "test"){
    test_labels <- x_tst[,ultimo]
    test <- x_tst[,-ultimo]
  }
  else {
    test_labels <- x_tra[,ultimo]
    test <- x_tra[,-ultimo]
  }
  train_labels <- x_tra[,ultimo]
  aplicar_metodo_fold(x_tra[,-ultimo],train_labels,test,test_labels,metodo)
}
##Una vez creadas las funciones, realizo los cálculos
set.seed(1234)
nombre <- "australian/australian"
accuracy_train <- data.frame(sapply(1:10, run_metodo_fold, nombre, "knn", "train"))
max_accuracy_train <- max(accuracy_train[1,])
max_accuracy_train #0.8806
mean_accuracy_train <- sum(accuracy_train[1,])/ncol(accuracy_train)
mean_accuracy_train #0.8737

set.seed(1234)
accuracy_test <- data.frame(sapply(1:10, run_metodo_fold, nombre, "knn", "test"))
max_accuracy_test <- max(accuracy_test[1,])
max_accuracy_test #0.9411
mean_accuracy_test <- sum(accuracy_test[1,])/ncol(accuracy_test)
mean_accuracy_test #0.8558

mat_knn_val <- matrix(c(mean_accuracy_train,mean_accuracy_test))
rownames(mat_knn_val) <- c("Train","Test")
colnames(mat_knn_val) <- c("knn")
############################ GLM ##############################
set.seed(1234)
##Vamos a dividir el conjunto en train y test sin normalizar
total_train_glm <- length(australian$V15)*0.8
australian_train <- australian[1:total_train_glm,-15]
australian_train_labels <- as.factor(australian[1:total_train_glm,]$V15)
australian_test <- australian[(total_train_glm+1):length(australian$V15),-15]
australian_test_labels <- as.factor(australian[(total_train_glm+1):length(australian$V15),]$V15)
##Con caret directamente, hacemos el modelo de glm
glmFit <- train(australian_train, australian_train_labels, method="glm",metric="Accuracy",trControl=trainControl(method="cv",number=10))
#glmFit
##Añadimos los elementos de test y vemos el acierto
glmPredict <- predict(glmFit, newdata = australian_test)
confusionMatrix(data = glmPredict, reference = australian_test_labels)
matriz_confusion <- table(glmPredict,australian_test_labels)
accuracy <- mean(glmPredict==australian_test_labels)
accuracy
##Y hemos obtenido un modelo con un 87,68% de explicación del problema

##Realizo los cálculos sobre los conjuntos facilitados
set.seed(1234)
accuracy_train <- data.frame(sapply(1:10, run_metodo_fold, nombre,"glm", "train"))
max_accuracy_train <- max(accuracy_train)
max_accuracy_train #0.8870
mean_accuracy_train <- sum(accuracy_train)/nrow(accuracy_train)
mean_accuracy_train #0.8791

set.seed(1234)
accuracy_test <- data.frame(sapply(1:10, run_metodo_fold, nombre,"glm", "test"))
max_accuracy_test <- max(accuracy_test)
max_accuracy_test #0.8529
mean_accuracy_test <- sum(accuracy_test)/nrow(accuracy_test)
mean_accuracy_test #0.8176

mat_glm_val <- matrix(c(mean_accuracy_train,mean_accuracy_test))
rownames(mat_glm_val) <- c("Train","Test")
colnames(mat_glm_val) <- c("glm")
############################ LDA ##############################
set.seed(1234)
##Directamente realizo el modelo lda con caret:
ldaFit <- train(australian_train,australian_train_labels,
                method="lda",metric="Accuracy",preProcess=c("center","scale"),
                tuneLength=10,trControl=trainControl(method="cv",number=10))
summary(ldaFit)
ldaPredict <- predict(ldaFit, newdata = australian_test)
confusionMatrix(data = ldaPredict, reference = australian_test_labels)
##Obteniendo un accuracy del 0.8768 para este conjunto de train y test

##Realizo los cálculos para las particiones de train y test
set.seed(1234)
accuracy_train <- data.frame(sapply(1:10, run_metodo_fold, nombre, "lda", "train"))
max_accuracy_train <- max(accuracy_train)
max_accuracy_train #0.8661
mean_accuracy_train <- sum(accuracy_train)/nrow(accuracy_train)
mean_accuracy_train #0.8603

set.seed(1234)
accuracy_test <- data.frame(sapply(1:10, run_metodo_fold, nombre, "lda", "test"))
max_accuracy_test <- max(accuracy_test)
max_accuracy_test #0.9117
mean_accuracy_test <- sum(accuracy_test)/nrow(accuracy_test)
mean_accuracy_test #0.8514

mat_lda_val <- matrix(c(mean_accuracy_train,mean_accuracy_test))
rownames(mat_lda_val) <- c("Train","Test")
colnames(mat_lda_val) <- c("lda")
############################ QDA ##############################
set.seed(1234)
qdaFit <- train(australian_train,australian_train_labels,
                method="qda",metric="Accuracy",preProcess=c("center","scale"),
                tuneLength=10,trControl=trainControl(method="cv",number=10))
qdaPredict <- predict(qdaFit, newdata = australian_test)
confusionMatrix(data = qdaPredict, reference = australian_test_labels)
##Obteniendo un accuracy del 0.7826 para este conjunto de train y test

##Realizo los cálculos
set.seed(1234)
nombre <- "australian/australian"
accuracy_train <- data.frame(sapply(1:10, run_metodo_fold, nombre, "qda", "train"))
max_accuracy_train <- max(accuracy_train)
max_accuracy_train #0.8112
mean_accuracy_train <- sum(accuracy_train)/nrow(accuracy_train)
mean_accuracy_train #0.8064

set.seed(1234)
accuracy_test <- data.frame(sapply(1:10, run_metodo_fold, nombre, "qda", "test"))
max_accuracy_test <- max(accuracy_test)
max_accuracy_test #0.8235
mean_accuracy_test <- sum(accuracy_test)/nrow(accuracy_test)
mean_accuracy_test#0.7676

mat_qda_val <- matrix(c(mean_accuracy_train,mean_accuracy_test))
rownames(mat_qda_val) <- c("Train","Test")
colnames(mat_qda_val) <- c("qda")

resultados_clasificacion0 <- cbind(mat_knn_val,mat_glm_val,mat_lda_val,mat_qda_val)
resultados_clasificacion0
################## Probando otros modelos #####################
##Como V10 y V9 vimos que estaban correladas por lo que voy a juntarlas, este es el modelo 1
australian_new =australian
australian_new[,9] = australian_new[,9]*australian_new[,10]
australian_new[,10]=NULL
summary(australian_new)
##Realizo los cálculos
set.seed(1234)
accuracy_train <- data.frame(sapply(1:10, run_metodo_fold, nombre, "knn", "train",1))
mean_accuracy_train <- sum(accuracy_train[1,])/ncol(accuracy_train)
mean_accuracy_train #
set.seed(1234)
accuracy_test <- data.frame(sapply(1:10, run_metodo_fold, nombre, "knn", "test",1))
mean_accuracy_test <- sum(accuracy_test[1,])/ncol(accuracy_test)
mean_accuracy_test #
mat_knn_val <- matrix(c(mean_accuracy_train,mean_accuracy_test))
rownames(mat_knn_val) <- c("Train","Test")
colnames(mat_knn_val) <- c("knn1")
set.seed(1234)
accuracy_train <- data.frame(sapply(1:10, run_metodo_fold, nombre,"glm", "train",1))
mean_accuracy_train <- sum(accuracy_train)/nrow(accuracy_train)
mean_accuracy_train #
set.seed(1234)
accuracy_test <- data.frame(sapply(1:10, run_metodo_fold, nombre,"glm", "test",1))
mean_accuracy_test <- sum(accuracy_test)/nrow(accuracy_test)
mean_accuracy_test #
mat_glm_val <- matrix(c(mean_accuracy_train,mean_accuracy_test))
rownames(mat_glm_val) <- c("Train","Test")
colnames(mat_glm_val) <- c("glm1")
set.seed(1234)
accuracy_train <- data.frame(sapply(1:10, run_metodo_fold, nombre, "lda", "train",1))
mean_accuracy_train <- sum(accuracy_train)/nrow(accuracy_train)
mean_accuracy_train #
set.seed(1234)
accuracy_test <- data.frame(sapply(1:10, run_metodo_fold, nombre, "lda", "test",1))
mean_accuracy_test <- sum(accuracy_test)/nrow(accuracy_test)
mean_accuracy_test #
mat_lda_val <- matrix(c(mean_accuracy_train,mean_accuracy_test))
rownames(mat_lda_val) <- c("Train","Test")
colnames(mat_lda_val) <- c("lda1")
set.seed(1234)
accuracy_train <- data.frame(sapply(1:10, run_metodo_fold, nombre, "qda", "train",1))
mean_accuracy_train <- sum(accuracy_train)/nrow(accuracy_train)
mean_accuracy_train #
set.seed(1234)
accuracy_test <- data.frame(sapply(1:10, run_metodo_fold, nombre, "qda", "test",1))
mean_accuracy_test <- sum(accuracy_test)/nrow(accuracy_test)
mean_accuracy_test#
mat_qda_val <- matrix(c(mean_accuracy_train,mean_accuracy_test))
rownames(mat_qda_val) <- c("Train","Test")
colnames(mat_qda_val) <- c("qda1")

resultados_clasificacion1 <- cbind(mat_knn_val,mat_glm_val,mat_lda_val,mat_qda_val)
resultados_clasificacion <- cbind(resultados_clasificacion0,resultados_clasificacion1)
resultados_clasificacion
##Por lo que podemos observar que las diferencias son mínimas, siendo un poco mejor el modelo con todas las variables

##Seguimos probando modelos:
cor(australian_new[,1:13])
##Podemos ver como las variables V5 y V6 tiene algo de correlación
cor(australian_new[,1:13])[5,6]
##Por lo tanto voy a probar a unirlas, este será el modelo 2
australian_new[,5] = australian_new[,5]*australian_new[,6]
australian_new[,6]=NULL

##Realizo los cálculos
modelo_actual=2
set.seed(1234)
accuracy_train <- data.frame(sapply(1:10, run_metodo_fold, nombre, "knn", "train",modelo_actual))
mean_accuracy_train <- sum(accuracy_train[1,])/ncol(accuracy_train)
mean_accuracy_train #
set.seed(1234)
accuracy_test <- data.frame(sapply(1:10, run_metodo_fold, nombre, "knn", "test",modelo_actual))
mean_accuracy_test <- sum(accuracy_test[1,])/ncol(accuracy_test)
mean_accuracy_test #
mat_knn_val <- matrix(c(mean_accuracy_train,mean_accuracy_test))
rownames(mat_knn_val) <- c("Train","Test")
colnames(mat_knn_val) <- c("knn2")
set.seed(1234)
accuracy_train <- data.frame(sapply(1:10, run_metodo_fold, nombre,"glm", "train",modelo_actual))
mean_accuracy_train <- sum(accuracy_train)/nrow(accuracy_train)
mean_accuracy_train #
set.seed(1234)
accuracy_test <- data.frame(sapply(1:10, run_metodo_fold, nombre,"glm", "test",modelo_actual))
mean_accuracy_test <- sum(accuracy_test)/nrow(accuracy_test)
mean_accuracy_test #
mat_glm_val <- matrix(c(mean_accuracy_train,mean_accuracy_test))
rownames(mat_glm_val) <- c("Train","Test")
colnames(mat_glm_val) <- c("glm2")
set.seed(1234)
accuracy_train <- data.frame(sapply(1:10, run_metodo_fold, nombre, "lda", "train",modelo_actual))
mean_accuracy_train <- sum(accuracy_train)/nrow(accuracy_train)
mean_accuracy_train #
set.seed(1234)
accuracy_test <- data.frame(sapply(1:10, run_metodo_fold, nombre, "lda", "test",modelo_actual))
mean_accuracy_test <- sum(accuracy_test)/nrow(accuracy_test)
mean_accuracy_test #
mat_lda_val <- matrix(c(mean_accuracy_train,mean_accuracy_test))
rownames(mat_lda_val) <- c("Train","Test")
colnames(mat_lda_val) <- c("lda2")
set.seed(1234)
accuracy_train <- data.frame(sapply(1:10, run_metodo_fold, nombre, "qda", "train",modelo_actual))
mean_accuracy_train <- sum(accuracy_train)/nrow(accuracy_train)
mean_accuracy_train #
set.seed(1234)
accuracy_test <- data.frame(sapply(1:10, run_metodo_fold, nombre, "qda", "test",modelo_actual))
mean_accuracy_test <- sum(accuracy_test)/nrow(accuracy_test)
mean_accuracy_test#
mat_qda_val <- matrix(c(mean_accuracy_train,mean_accuracy_test))
rownames(mat_qda_val) <- c("Train","Test")
colnames(mat_qda_val) <- c("qda2")

resultados_clasificacion2 <- cbind(mat_knn_val,mat_glm_val,mat_lda_val,mat_qda_val)
resultados_clasificacion <- cbind(resultados_clasificacion0,resultados_clasificacion1,resultados_clasificacion2)
resultados_clasificacion
##Por lo que podemos observar que las diferencias son mínimas, siendo un poco mejor el modelo con todas las variables

##Si seguimos estudiando como mejorar el modelo podemos hacer pruebas, ya que no tenemos información de que es cada variable:
glmFit = glm(V15~.,data=australian_new,family=binomial)
summary(glmFit)
##Donde podemos ver que V5 y V9, que son las variables que hemos fusionado son muy importantes, por lo que 
##vamos a eliminar algunas de las que su p-value es mayor a 0.5 como V1 y V3. Este es el modelo 3
australian_new$V1=NULL
australian_new$V3=NULL
glmFit = glm(V15~.,data=australian_new,family=binomial)
summary(glmFit)
##Veamos como se comporta:
modelo_actual=3
set.seed(1234)
accuracy_train <- data.frame(sapply(1:10, run_metodo_fold, nombre, "knn", "train",modelo_actual))
mean_accuracy_train <- sum(accuracy_train[1,])/ncol(accuracy_train)
mean_accuracy_train #
set.seed(1234)
accuracy_test <- data.frame(sapply(1:10, run_metodo_fold, nombre, "knn", "test",modelo_actual))
mean_accuracy_test <- sum(accuracy_test[1,])/ncol(accuracy_test)
mean_accuracy_test #
mat_knn_val <- matrix(c(mean_accuracy_train,mean_accuracy_test))
rownames(mat_knn_val) <- c("Train","Test")
colnames(mat_knn_val) <- c("knn3")
set.seed(1234)
accuracy_train <- data.frame(sapply(1:10, run_metodo_fold, nombre,"glm", "train",modelo_actual))
mean_accuracy_train <- sum(accuracy_train)/nrow(accuracy_train)
mean_accuracy_train #
set.seed(1234)
accuracy_test <- data.frame(sapply(1:10, run_metodo_fold, nombre,"glm", "test",modelo_actual))
mean_accuracy_test <- sum(accuracy_test)/nrow(accuracy_test)
mean_accuracy_test #
mat_glm_val <- matrix(c(mean_accuracy_train,mean_accuracy_test))
rownames(mat_glm_val) <- c("Train","Test")
colnames(mat_glm_val) <- c("glm3")
set.seed(1234)
accuracy_train <- data.frame(sapply(1:10, run_metodo_fold, nombre, "lda", "train",modelo_actual))
mean_accuracy_train <- sum(accuracy_train)/nrow(accuracy_train)
mean_accuracy_train #
set.seed(1234)
accuracy_test <- data.frame(sapply(1:10, run_metodo_fold, nombre, "lda", "test",modelo_actual))
mean_accuracy_test <- sum(accuracy_test)/nrow(accuracy_test)
mean_accuracy_test #
mat_lda_val <- matrix(c(mean_accuracy_train,mean_accuracy_test))
rownames(mat_lda_val) <- c("Train","Test")
colnames(mat_lda_val) <- c("lda3")
set.seed(1234)
accuracy_train <- data.frame(sapply(1:10, run_metodo_fold, nombre, "qda", "train",modelo_actual))
mean_accuracy_train <- sum(accuracy_train)/nrow(accuracy_train)
mean_accuracy_train #
set.seed(1234)
accuracy_test <- data.frame(sapply(1:10, run_metodo_fold, nombre, "qda", "test",modelo_actual))
mean_accuracy_test <- sum(accuracy_test)/nrow(accuracy_test)
mean_accuracy_test#
mat_qda_val <- matrix(c(mean_accuracy_train,mean_accuracy_test))
rownames(mat_qda_val) <- c("Train","Test")
colnames(mat_qda_val) <- c("qda3")

resultados_clasificacion3 <- cbind(mat_knn_val,mat_glm_val,mat_lda_val,mat_qda_val)
resultados_clasificacion <- cbind(resultados_clasificacion0,resultados_clasificacion1,resultados_clasificacion2,resultados_clasificacion3)
resultados_clasificacion
##Vemos como los resultados no han mejorado, por lo que vamos a probar otro modelo

glmFit = glm(V15~.,data=australian_new,family=binomial)
summary(glmFit)
##vamos a eliminar algunas de las que su p-value es mayor a 0.2 como V2, V11 y V12
australian_new$V2=NULL
australian_new$V11=NULL
australian_new$V12=NULL
glmFit = glm(V15~.,data=australian_new,family=binomial)
summary(glmFit)
cor(australian_new[,1:7])
##Donde veo como V7, que es la que tiene un p-value mayor, no guarda correlación con ninguna, por lo que también la eliminaré. Modelo 4
australian_new$V7=NULL

##Veamos como se comporta:
modelo_actual=4
set.seed(1234)
accuracy_train <- data.frame(sapply(1:10, run_metodo_fold, nombre, "knn", "train",modelo_actual))
mean_accuracy_train <- sum(accuracy_train[1,])/ncol(accuracy_train)
mean_accuracy_train #
set.seed(1234)
accuracy_test <- data.frame(sapply(1:10, run_metodo_fold, nombre, "knn", "test",modelo_actual))
mean_accuracy_test <- sum(accuracy_test[1,])/ncol(accuracy_test)
mean_accuracy_test #
mat_knn_val <- matrix(c(mean_accuracy_train,mean_accuracy_test))
rownames(mat_knn_val) <- c("Train","Test")
colnames(mat_knn_val) <- c("knn4")
set.seed(1234)
accuracy_train <- data.frame(sapply(1:10, run_metodo_fold, nombre,"glm", "train",modelo_actual))
mean_accuracy_train <- sum(accuracy_train)/nrow(accuracy_train)
mean_accuracy_train #
set.seed(1234)
accuracy_test <- data.frame(sapply(1:10, run_metodo_fold, nombre,"glm", "test",modelo_actual))
mean_accuracy_test <- sum(accuracy_test)/nrow(accuracy_test)
mean_accuracy_test #
mat_glm_val <- matrix(c(mean_accuracy_train,mean_accuracy_test))
rownames(mat_glm_val) <- c("Train","Test")
colnames(mat_glm_val) <- c("glm4")
set.seed(1234)
accuracy_train <- data.frame(sapply(1:10, run_metodo_fold, nombre, "lda", "train",modelo_actual))
mean_accuracy_train <- sum(accuracy_train)/nrow(accuracy_train)
mean_accuracy_train #
set.seed(1234)
accuracy_test <- data.frame(sapply(1:10, run_metodo_fold, nombre, "lda", "test",modelo_actual))
mean_accuracy_test <- sum(accuracy_test)/nrow(accuracy_test)
mean_accuracy_test #
mat_lda_val <- matrix(c(mean_accuracy_train,mean_accuracy_test))
rownames(mat_lda_val) <- c("Train","Test")
colnames(mat_lda_val) <- c("lda4")
set.seed(1234)
accuracy_train <- data.frame(sapply(1:10, run_metodo_fold, nombre, "qda", "train",modelo_actual))
mean_accuracy_train <- sum(accuracy_train)/nrow(accuracy_train)
mean_accuracy_train #
set.seed(1234)
accuracy_test <- data.frame(sapply(1:10, run_metodo_fold, nombre, "qda", "test",modelo_actual))
mean_accuracy_test <- sum(accuracy_test)/nrow(accuracy_test)
mean_accuracy_test#
mat_qda_val <- matrix(c(mean_accuracy_train,mean_accuracy_test))
rownames(mat_qda_val) <- c("Train","Test")
colnames(mat_qda_val) <- c("qda4")

resultados_clasificacion4 <- cbind(mat_knn_val,mat_glm_val,mat_lda_val,mat_qda_val)
resultados_clasificacion <- cbind(resultados_clasificacion0,resultados_clasificacion1,resultados_clasificacion2,resultados_clasificacion3,resultados_clasificacion4)
resultados_clasificacion

##Veamos que tal esta el modelo actual
glmFit = glm(V15~.,data=australian_new,family=binomial)
summary(glmFit)
cor(australian_new[,1:6])
##Viendo las correlaciones, podemos observar la que hay entre la variable V8 y V9 (V9*V10)
cor(australian_new[,1:6])[3,4]
##Por lo que voy a probar un último modelo con ambas variables unidas V8*V9*V10. Modelo 5

##Veamos como se comporta:
modelo_actual=5
set.seed(1234)
accuracy_train <- data.frame(sapply(1:10, run_metodo_fold, nombre, "knn", "train",modelo_actual))
mean_accuracy_train <- sum(accuracy_train[1,])/ncol(accuracy_train)
mean_accuracy_train #
set.seed(1234)
accuracy_test <- data.frame(sapply(1:10, run_metodo_fold, nombre, "knn", "test",modelo_actual))
mean_accuracy_test <- sum(accuracy_test[1,])/ncol(accuracy_test)
mean_accuracy_test #
mat_knn_val <- matrix(c(mean_accuracy_train,mean_accuracy_test))
rownames(mat_knn_val) <- c("Train","Test")
colnames(mat_knn_val) <- c("knn5")
set.seed(1234)
accuracy_train <- data.frame(sapply(1:10, run_metodo_fold, nombre,"glm", "train",modelo_actual))
mean_accuracy_train <- sum(accuracy_train)/nrow(accuracy_train)
mean_accuracy_train #
set.seed(1234)
accuracy_test <- data.frame(sapply(1:10, run_metodo_fold, nombre,"glm", "test",modelo_actual))
mean_accuracy_test <- sum(accuracy_test)/nrow(accuracy_test)
mean_accuracy_test #
mat_glm_val <- matrix(c(mean_accuracy_train,mean_accuracy_test))
rownames(mat_glm_val) <- c("Train","Test")
colnames(mat_glm_val) <- c("glm5")
set.seed(1234)
accuracy_train <- data.frame(sapply(1:10, run_metodo_fold, nombre, "lda", "train",modelo_actual))
mean_accuracy_train <- sum(accuracy_train)/nrow(accuracy_train)
mean_accuracy_train #
set.seed(1234)
accuracy_test <- data.frame(sapply(1:10, run_metodo_fold, nombre, "lda", "test",modelo_actual))
mean_accuracy_test <- sum(accuracy_test)/nrow(accuracy_test)
mean_accuracy_test #
mat_lda_val <- matrix(c(mean_accuracy_train,mean_accuracy_test))
rownames(mat_lda_val) <- c("Train","Test")
colnames(mat_lda_val) <- c("lda5")
set.seed(1234)
accuracy_train <- data.frame(sapply(1:10, run_metodo_fold, nombre, "qda", "train",modelo_actual))
mean_accuracy_train <- sum(accuracy_train)/nrow(accuracy_train)
mean_accuracy_train #
set.seed(1234)
accuracy_test <- data.frame(sapply(1:10, run_metodo_fold, nombre, "qda", "test",modelo_actual))
mean_accuracy_test <- sum(accuracy_test)/nrow(accuracy_test)
mean_accuracy_test#
mat_qda_val <- matrix(c(mean_accuracy_train,mean_accuracy_test))
rownames(mat_qda_val) <- c("Train","Test")
colnames(mat_qda_val) <- c("qda5")

resultados_clasificacion5 <- cbind(mat_knn_val,mat_glm_val,mat_lda_val,mat_qda_val)
resultados_clasificacion <- cbind(resultados_clasificacion0,resultados_clasificacion1,resultados_clasificacion2,resultados_clasificacion3,resultados_clasificacion4,resultados_clasificacion5)
resultados_clasificacion
##Viendo que los resultados de este último modelo son peores

##Hagamos una comparación de nuestros algoritmos para encontrar el mejor modelo
data_result_clasif <- as.data.frame(resultados_clasificacion)
data_result_clasif[which.max(as.numeric(data_result_clasif[2,]))]
mejor_knn_train <- (data_result_clasif[which.max(as.numeric(data_result_clasif[2,]))])[1,]
mejor_knn_test <- (data_result_clasif[which.max(as.numeric(data_result_clasif[2,]))])[2,]
##Donde vemos que el modelo con mayor Accuracy en test es el modelo 4, con knn
data_result_clasif$knn=NULL;data_result_clasif$knn1=NULL;data_result_clasif$knn2=NULL;data_result_clasif$knn3=NULL;data_result_clasif$knn4=NULL;data_result_clasif$knn5=NULL
data_result_clasif[which.max(as.numeric(data_result_clasif[2,]))]
mejor_lda_train <- (data_result_clasif[which.max(as.numeric(data_result_clasif[2,]))])[1,]
mejor_lda_test <- (data_result_clasif[which.max(as.numeric(data_result_clasif[2,]))])[2,]
##El segundo mejor modelo, sin knn, es el modelo 0 para lda
data_result_clasif$lda=NULL;data_result_clasif$lda1=NULL;data_result_clasif$lda2=NULL;data_result_clasif$lda3=NULL;data_result_clasif$lda4=NULL;data_result_clasif$lda5=NULL
data_result_clasif[which.max(as.numeric(data_result_clasif[2,]))]
##Para glm vemos como el mejor modelo es el modelo 4
data_result_clasif$glm=NULL;data_result_clasif$glm1=NULL;data_result_clasif$glm2=NULL;data_result_clasif$glm3=NULL;data_result_clasif$glm4=NULL;data_result_clasif$glm5=NULL
data_result_clasif[which.max(as.numeric(data_result_clasif[2,]))]
mejor_qda_train <- (data_result_clasif[which.max(as.numeric(data_result_clasif[2,]))])[1,]
mejor_qda_test <- (data_result_clasif[which.max(as.numeric(data_result_clasif[2,]))])[2,]
##Por último, para qda, el mejor modelo es el modelo 5
##Por lo que, nuestro mejor modelo ha sido el modelo 4 en el que hacimos la unión de las variables V10 con V9 y V5 con V6, y eliminabamos
##las variables V12, V11, V3, V2 y V1, usando knn
######################### COMPARACIÓN #########################
##leemos la tabla con los errores medios de test
resultados_test <- read.csv("clasif_test_alumos.csv")
tablatst <- cbind(resultados_test[,2:dim(resultados_test)[2]])
colnames(tablatst) <- names(resultados_test)[2:dim(resultados_test)[2]]
rownames(tablatst) <- resultados_test[,1]
resultados_test[2,]

##leemos la tabla con los errores medios de train
resultados_train <- read.csv("clasif_train_alumnos.csv")
tablatra <- cbind(resultados_train[,2:dim(resultados_train)[2]])
colnames(tablatra) <- names(resultados_train)[2:dim(resultados_train)[2]]
rownames(tablatra) <- resultados_train[,1]
resultados_train[2,]

##Añadir los valores resultantes en test
tablatst[2,][1] <- mejor_knn_test
tablatst[2,][2] <- mejor_lda_test
tablatst[2,][3] <- mejor_qda_test
tablatst[2,]

##Añadir los valores resultantes en train
tablatra[2,][1] <- mejor_knn_train
tablatra[2,][2] <- mejor_lda_train
tablatra[2,][3] <- mejor_qda_train
tablatra[2,]

##Comparativa por pares de LDA y QDA para test (Wilcoxon´s test)
##LDA (other) vs QDA (ref)
## + 0.1 porque wilcox R falla para valores == 0 en la tabla
difs <- (tablatst[,2] - tablatst[,3]) / tablatst[,2]
wilc_1_2 <- cbind(ifelse (difs<0, abs(difs)+0.1, 0+0.1), ifelse (difs>0, abs(difs)+0.1, 0+0.1)) 
colnames(wilc_1_2) <- c(colnames(tablatst)[2], colnames(tablatst)[3])
head(wilc_1_2)

##Se aplica el test Wilcoxon para interpretar los resultados
LDAvsQDAtst <- wilcox.test(wilc_1_2[,1], wilc_1_2[,2], alternative = "two.sided", paired=TRUE) 
Rmas <- LDAvsQDAtst$statistic
pvalue <- LDAvsQDAtst$p.value
LDAvsQDAtst <- wilcox.test(wilc_1_2[,2], wilc_1_2[,1], alternative = "two.sided", paired=TRUE) 
Rmenos <- LDAvsQDAtst$statistic
Rmas
Rmenos
pvalue
##Al ser p-valor mayor a 0.05 no existen diferencias significativas entre ambos
##Solo un 21,58% de confianza de que existan diferencias ((1-pvalue)*100)
##Como el p-value es 0.7841, mayor que el nivel de signifiación 0.05, no somos capaces
##a rechazar la hipótesis nula y concluir que no hay suficiente evidencia en los datos.
##Por lo tanto se acepta la hipotesis nula: las dos distribuciones son iguales

##Comparativa por pares de LDA y QDA para train (Wilcoxon´s test)
##LDA (other) vs QDA (ref)
## + 0.1 porque wilcox R falla para valores == 0 en la tabla
difs <- (tablatra[,2] - tablatra[,3]) / tablatra[,2]
wilc_1_2 <- cbind(ifelse (difs<0, abs(difs)+0.1, 0+0.1), ifelse (difs>0, abs(difs)+0.1, 0+0.1)) 
colnames(wilc_1_2) <- c(colnames(tablatra)[2], colnames(tablatra)[3])
head(wilc_1_2)

##Se aplica el test Wilcoxon para interpretar los resultados
LDAvsQDAtst <- wilcox.test(wilc_1_2[,1], wilc_1_2[,2], alternative = "two.sided", paired=TRUE) 
Rmas <- LDAvsQDAtst$statistic
pvalue <- LDAvsQDAtst$p.value
LDAvsQDAtst <- wilcox.test(wilc_1_2[,2], wilc_1_2[,1], alternative = "two.sided", paired=TRUE) 
Rmenos <- LDAvsQDAtst$statistic
Rmas
Rmenos
pvalue
##Al igual que en test, para el train, no existen diferencias significativas al ser
##p-value = 0.1536, mayor a 0.05.


##Comparativas múltiples con Friedman y post-hoc Holm
test_friedman <- friedman.test(as.matrix(tablatst)) 
test_friedman
##p-value = 0.9512, mayor que 0.05 por lo que se acepta la hipótesis nula, no existen
##diferencias significativas entre los 3 algoritmos.

##Aplicamos post-hoc Holm
tam <- dim(tablatst)
groups <- rep(1:tam[2], each=tam[1])
pairwise.wilcox.test(as.matrix(tablatst), groups, p.adjust = "holm", paired = TRUE)
##No existen diferencias significativas como indicaba el test de Friedman

##Comparativas múltiples con Friedman y post-hoc Holm para train
test_friedman <- friedman.test(as.matrix(tablatra)) 
test_friedman
##No existen diferencias significativas entre los algoritmos al ser p-value = 0.522, mayor que 0.05

##Aplicamos post-hoc Holm
tam <- dim(tablatra)
groups <- rep(1:tam[2], each=tam[1])
pairwise.wilcox.test(as.matrix(tablatra), groups, p.adjust = "holm", paired = TRUE)
##No existen diferencias significativas como indicaba el test de Friedman