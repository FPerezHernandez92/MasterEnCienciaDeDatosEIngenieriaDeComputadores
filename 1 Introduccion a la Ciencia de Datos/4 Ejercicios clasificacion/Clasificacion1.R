### FRANCISCO PÉREZ HERNÁNDEZ 20076629K
## CLASIFICACIÓN 1
set.seed(1234)
########################
# EJEMPLO TRANSPARENCIAS
########################
# import the CSV file
wbcd <- read.csv("wisc_bc_data.csv", stringsAsFactors = FALSE)
# examine the structure of the wbcd data frame
str(wbcd)
# drop the id feature
wbcd <- wbcd[,-1]
# table of diagnosis
table(wbcd$diagnosis)
# recode diagnosis as a factor
wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B", "M"),
                         label = c("Benign", "Malignant"))
# table or proportions with more informative labels
round(prop.table(table(wbcd$diagnosis)) * 100, digits = 1)
# summarize three numeric features
summary(wbcd[c("radius_mean", "area_mean", "smoothness_mean")])
# create normalization function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
# test normalization function - result should be identical
normalize(c(1, 2, 3, 4, 5))
normalize(c(10, 20, 30, 40, 50))
# normalize the wbcd data
wbcd_n <- as.data.frame(lapply(wbcd[,2:31], normalize))
# confirm that normalization worked
summary(wbcd_n$area_mean)
# Visualize data
plot(wbcd[,2:5])
plot(wbcd_n[,1:4], col=wbcd[,1])
# Calculate correlation
cor(wbcd[,2:5])
# create training and test data
wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]
# create labels for training and test data
wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]
## Training a model on the data ----
# load the "class" library
library(class)
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                      cl = wbcd_train_labels, k=21)
## Evaluating model performance ----
table(wbcd_test_pred,wbcd_test_labels)
## Using caret
require(caret)
knnModel <- train(x = wbcd[1:469,-1], y = wbcd[1:469,1], method = "knn", preProc = c("center","scale"))
knnModel
knnFit <- train(wbcd_train, wbcd_train_labels, method="knn", metric="Accuracy", tuneGrid = data.frame(.k=1:15))
knnFit
knnPred <- predict(knnModel, newdata = wbcd_test)
testData = wbcd[470:569,1]
postResample(pred = knnPred, obs = testData)
#Por lo que ya se ha podido ver el funcionamiento y el resultado proporcionado

set.seed(123456)
#EJERCICIO 1: Try with different k choices and do a quick comparison. You can draw a plot to show the results.
#Creo una función con la que calcular un modelo Knn con distintos valores de K y que guarde el valor accuracy
aplicar_knn <- function(k){
  mi_knn <- knn(train = wbcd_train, test = wbcd_test,
                        cl = wbcd_train_labels, k)
  accu <- postResample(pred = mi_knn, obs = testData)[1]
}
#Aplico la funcíon para obtener los accuracy
accuracy <- sapply(1:400,aplicar_knn)
#Primero habia puesto 10 pero al ver que no habia demasiada diferencia he usado 400
accuracy
#Pinto las gráficas para ver los valores de accuracy
plot(x=1:400,y=accuracy)
plot(x=1:100,y=accuracy[1:100],type="l")
plot(x=1:40, y=accuracy[1:40],type="l")
max(accuracy)
plot(x=1:25,y=accuracy[1:25],type="p")
lines(x=1:25,y=accuracy[1:25],type="l",col="blue")
#Vemos como hay 5 valores con accuracy = 0.98 asique tenemos distintos k buenos


######################
#SEGUNDA PARTE DEL PDF
######################
set.seed(1234)
require(ISLR)
names(Smarket)
summary(Smarket)
?Smarket
pairs(Smarket,col=Smarket$Direction)

# Let's see the data...
cor(Smarket) # This won't work, why)
#Porque debe ser variables numéricas
cor(Smarket[,-9]) # Note that Volume has some correlation with Year...
boxplot(Smarket$Volume~Smarket$Year)
# Direction is derive from Today
cor(as.numeric(Smarket$Direction),Smarket$Today)

# Logistic regression
glm.fit <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
               data=Smarket,family=binomial) 
summary(glm.fit)
glm.probs <- predict(glm.fit,type="response")
glm.probs[1:5]
glm.pred <- ifelse(glm.probs>0.5,"Up","Down")
attach(Smarket)
table(glm.pred,Direction)
mean(glm.pred==Direction)

# Make training and test set
train <- Year<2005
glm.fit <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
               data=Smarket,family=binomial, subset=train)
glm.probs <- predict(glm.fit,newdata=Smarket[!train,],type="response")
glm.pred <- ifelse(glm.probs >0.5,"Up","Down")
Direction.2005=Smarket$Direction[!train]
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)

#Overfitting!
#Fit smaller model
glm.fit <- glm(Direction~Lag1+Lag2,
               data=Smarket,family=binomial, subset=train)
glm.probs=predict(glm.fit,newdata=Smarket[!
                                            train,],type="response")
glm.pred <- ifelse(glm.probs >0.5,"Up","Down")
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)

glmFit <- train(train_x, y = train_y,
                method = "glm",
                preProcess = c("center", "scale"),
                tuneLength = 10,
                trControl = trainControl(method = "cv"))
##En el siguiente ejercicio se arregla el problema

#EJERCICIO 2: Using the Smarket dataset: Perform 10 fold-cv with logistic regression
set.seed(1234)
#Divido en train y test
total_train <- length(Smarket$Year)*0.8
Smarket_train <- Smarket[1:total_train,-9]
#He quitado la última ya que es la que vamos a predecir
Smarket_train_labels <- as.factor(Smarket[1:total_train,]$Direction)
Smarket_test <- Smarket[(total_train+1):length(Smarket$Year),-9]
Smarket_test_labels <- as.factor(Smarket[(total_train+1):length(Smarket$Year),]$Direction)

glmFit <- train(Smarket_train, Smarket_train_labels, method="glm",
                metric="Accuracy", 
                trControl=trainControl(method ="cv",number=10))
warnings()
#glm.fit: el alogritmo no converge por lo que hay que darle mas iteraciones
glmFit <- train(Smarket_train, Smarket_train_labels, method="glm",
                metric="Accuracy", 
                trControl=trainControl(method ="cv",number=10),
                control=glm.control(maxit=40))
#He probado de 10 en 10 hasta que con 40 ha funcionado
warnings()
#glm.fit: se suponen que son problemas de preciosión pero no he conseguido arreglarlos
glmFit
#Tenemos un Accuracy del 99,6% por lo que el modelo es muy bueno, o está muy sobreajustado
#Probamos en test:
glmPredict <- predict(glmFit, newdata = Smarket_test)
confusionMatrix(data = glmPredict, reference = Smarket_test_labels)
#Al realizar la matriz de confusión obtenemos un Accuracy del 99,6% para el test, por lo que el modelo es muy bueno o está sobreajustado

