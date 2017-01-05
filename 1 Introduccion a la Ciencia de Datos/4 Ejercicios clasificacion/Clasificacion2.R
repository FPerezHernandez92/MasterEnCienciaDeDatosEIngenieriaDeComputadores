#FRANCISCO PÉREZ HERNÁNDEZ 20076629K
##### Classification using LDA
## Example: The Stock Market Data ----
set.seed(1234)
library(MASS)
library(ISLR)
library(klaR)
## Linear Discriminant Analysis
lda.fit=lda(Direction~Lag1+Lag2
            ,data=Smarket,
            subset=Year<2005)
lda.fit
plot(lda.fit, type="both")

Smarket.2005=subset(Smarket,Year==2005)
lda.pred=predict(lda.fit,Smarket.2005)
lda.pred$class[1:5] ### arreglado
class(lda.pred)
data.frame(lda.pred)[1:5,]
table(lda.pred$class,Smarket.2005$Direction)
#Aquí podemos ver la tabla comparativa del test
mean(lda.pred$class==Smarket.2005$Direction)
#Una media del 55,95% que no está mal para comenzar
partimat(Direction~Lag1+Lag2, data=Smarket ,method="lda")
#Vemos como es dificil obtener un modelo que se ajuste correctamente

## QDA
qda.fit=qda(Direction~Lag1+Lag2, data=Smarket, subset=Year<2005)
qda.fit
qda.fit$scaling
plot(qda.fit, type="both")##omitir
Smarket.2005=subset(Smarket,Year==2005)
qda.pred=predict(qda.fit,Smarket.2005)
class(qda.pred)
data.frame(qda.pred)[1:5,]
table(qda.pred$class,Smarket.2005$Direction)
#Vemos de nuevo la tabla comparativa del test
mean(qda.pred$class==Smarket.2005$Direction)
#Y ahora obtenemos con QDA un 59,9% sobre el test, mejorando a LDA
partimat(Direction~Lag1+Lag2, data=Smarket ,method="qda")
#Pero se ve que aún es bastante dificil

## LDA
iris.lda<-lda(Species ~ Sepal.Length + Sepal.Width + Petal.Length
              + Petal.Width,  data = iris)
iris.lda
partimat(Species ~ Sepal.Length + Sepal.Width + Petal.Length +
           Petal.Width, data=iris, method="lda")
#Vemos como hemos usado LDA sobre el conjunto de datos IRIS

data(iris)
TrainData <- iris[,1:4]
TrainClasses <- iris[,5]
ldaFit <- train(TrainData, TrainClasses,
                method = "lda",
                preProcess = c("center", "scale"),
                tuneLength = 10,
                trControl = trainControl(method = "cv"))
confusionMatrix(ldaFit)
#Obtenemos un accuracy del 98% lo que es muy bueno el método LDA

qdaFit <- train(TrainData, TrainClasses,
                method = "qda",
                preProcess = c("center", "scale"),
                tuneLength = 10,
                trControl = trainControl(method = "cv"))
confusionMatrix(qdaFit)
#Con QDA obtenemos un 97% el cual no está nada mal pero sigue siendo mejor LDA



#EJERCICIO 1: 
set.seed(1234)
# - Try LDA with all Lag variables
Smarket
trainNumber <- cbind(c(1:1000))
trainData = Smarket[trainNumber,-9]
trainDataLabel = as.factor(Smarket[trainNumber,9])
testData = Smarket[-trainNumber,-9]
testDataLabel = as.factor(Smarket[-trainNumber,9])
lda.fit=lda(Direction~Lag1+Lag2+Lag3+Lag4+Lag5
            ,data=Smarket,train=trainData)
lda.fit
lda.pred=predict(lda.fit,testData)
table(lda.pred$class,testDataLabel)
#Aquí podemos ver la tabla comparativa del test
mean(lda.pred$class==testDataLabel)
confusionMatrix(data=lda.pred$class,reference = testDataLabel)
#Por lo que tenemos un accuracy del 56,4% el cual no es demasiado bueno
#Con caret:
ldaFit <- train(form=Direction~Lag1+Lag2+Lag3+Lag4+Lag5, data=Smarket,
                subset = trainNumber, method="lda",
                metric="Accuracy", 
                preProcess = c("center","scale"),
                tuneLength = 10,
                trControl=trainControl(method ="cv",number=10))

summary(ldaFit)
ldaPredict <- predict(ldaFit, newdata = testData)
confusionMatrix(data = ldaPredict, reference = testDataLabel)
#Obtenemos un 57,6% por lo que ha mejorado un poco
matrizAccuracy <- matrix(c(57.6))
colnames(matrizAccuracy) <- c("LDA")

# - Make a quick comparison between logistic regression and LDA
glmFit <- train(form=Direction~Lag1+Lag2+Lag3+Lag4+Lag5, data=Smarket,
                subset = trainNumber, method="glm",
                metric="Accuracy", 
                preProcess = c("center","scale"),
                tuneLength = 10,
                trControl=trainControl(method ="cv",number=10))

summary(glmFit)
glmPredict <- predict(glmFit, newdata = testData)
confusionMatrix(data = glmPredict, reference = testDataLabel)
#Con este método obtenemos un 57,6% igual que LDA con caret
matrizAccuracyaux <- matrix(c(57.6))
colnames(matrizAccuracyaux) <- c("GLM")
matrizAccuracy <- cbind(matrizAccuracy,matrizAccuracyaux)

# - Try with QDA and compare all three methods. Plot the results
qdaFit <- train(form=Direction~Lag1+Lag2+Lag3+Lag4+Lag5, data=Smarket,
                subset = trainNumber, method="qda",
                metric="Accuracy", 
                preProcess = c("center","scale"),
                tuneLength = 10,
                trControl=trainControl(method ="cv",number=10))
qdaPredict <- predict(qdaFit, newdata = testData)
confusionMatrix(data = qdaPredict, reference = testDataLabel)
#Obtenemos un 54,8% el cual es más malo que LDA y QDA
matrizAccuracyaux <- matrix(c(54.8))
colnames(matrizAccuracyaux) <- c("QDA")
matrizAccuracy <- cbind(matrizAccuracy,matrizAccuracyaux)
#Para visualizar los resultados me he decidido por mostrar los accuracy de los distintos métodos:
barplot(matrizAccuracy,ylim=c(0,100),col="green",main="Accuracy distintos métodos",ylab="Accuracy %")



set.seed(1234)
#EJERCICIO 2: Using only the information in file clasif_train_alumnos.csv:
train_alumnos <- read.csv("clasif_train_alumnos.csv",stringsAsFactors = FALSE)
# - Compare lda and qda using Wilcoxon
help("wilcox.test")
wilcox.test(train_alumnos$out_train_lda, train_alumnos$out_train_qda, paired=TRUE)
#Tenemos un p-valor mayor que 0.05 ya que da 0.17, por lo que no tienen diferencias entre si

# - Perform a multiple comparison using Friedman
help("friedman.test")
dim(train_alumnos)
head(train_alumnos)
m<-matrix(0,20,3)
m[,1] <- train_alumnos$out_train_knn
m[,2] <- train_alumnos$out_train_lda
m[,3] <- train_alumnos$out_train_qda
friedman.test(m)
#Tenemos un p-valor mayor que 0.05 ya que da 0.52, por lo que no tienen diferencias entre si  

# - Using Holm see if there is a winning algorithm (even if Friedman says ther is no chance...)
help("pairwise.wilcox.test")
tam <- dim(m)
groups <- rep(1:tam[2], each=tam[1])
pairwise.wilcox.test(m,groups,p.adjust="holm",paired=TRUE)
#Al no salir ningún p-valor menor que 0.05 no hay ningún resultado significativo 

