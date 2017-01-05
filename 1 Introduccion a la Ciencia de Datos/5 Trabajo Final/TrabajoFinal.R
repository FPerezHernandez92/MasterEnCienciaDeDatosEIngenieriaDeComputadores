##Nos movemos al directorio de trabajo
setwd("~/Dropbox/zMaster/zRStudio/Master-en-Ciencia-De-Datos-e-Ingeniería-de-Computadores/1 Introduccion a la Ciencia de Datos/5 Trabajo Final")

###############################################################
###############################################################
###############################################################
############### A) DESCRIPCIÓN DEL PROBLEMA ###################
###############################################################
###############################################################

##REGRESIÓN
baseball <- read.csv("baseball/baseball.dat", comment.char = "@", header = FALSE)
names(baseball) <- c("Batting_average", "On-base_percentage", "Runs", "Hits", "Doubles", "Triples", 
                 "HomeRuns", "Runs_batted_in", "Walks", "Strike-Outs", "Stolen_bases", "Errors", 
                 "Free_agency_eligibility", "Free_agent", "Arbitration_eligibility", "Arbitration", "Salary")
is.data.frame(baseball)
##Podemos ver como es un data frame la estructura de datos
ncol(baseball)
nrow(baseball)
##Con 17 columnas y 337 filas
##Tenemos que las variables "Batting_average" y "On-base_percentage" son numéricas, y el resto son int

##APARTADO A1: Media, desviación estandar...
summary(baseball)
##Obtenemos las medias de las variables o con summary o con apply
medias_baseball <- apply(baseball,2,mean)
medias_baseball
##Para la desviación estandar usamos apply
desviacion_estandar_baseball <- apply(baseball,2,sd)
desviacion_estandar_baseball
##Varianza
varianza_baseball <- apply(baseball,2,var)
varianza_baseball
##Desviación absoluta de la mediana
desviacion_absoluta_de_la_mediana_baseball <- apply(baseball,2,mad)
desviacion_absoluta_de_la_mediana_baseball
##Rango intercuartil
rango_intercuartil_baseball <- apply(baseball,2,IQR)
rango_intercuartil_baseball

##APARTADO A2: Gráficos
plot(baseball)
##Directamente con plot, podemos ver las gráficas de cada variable con el resto
par(mfrow=c(3,4))
nombres_baseball = names(baseball)
plot_con_respecto_a_y <- function(x,y){
  plot(baseball[,y]~baseball[,x],xlab=nombres_baseball[x],ylab="Salary")
}
sapply(1:12,plot_con_respecto_a_y,17)
par(mfrow=c(2,2))
sapply(13:16,plot_con_respecto_a_y,17)

##APARTADO A3: Descripción
##Podemos encontrar información en la web de Keel: http://sci2s.ugr.es/keel/dataset.php?cod=76#sub1
correlacion_baseball <- cor(baseball)
correlacion_baseball <- cor(baseball)[,17]
correlacion_baseball[cor(baseball)[,17] > 0.5 | cor(baseball)[,17] < -0.5]



#CLASIFICACIÓN
australian <- read.csv("australian/australian.dat", comment.char = "@", header = FALSE)
is.data.frame(australian)
#Podemos ver como es un data frame la estructura de datos
ncol(australian)
nrow(australian)
#Con 15 columnas y 690 filas
#Tenemos que las variables 2,3 y 7 son numéricas, y el resto son int

#APARTADO A1: Media, desviación estandar...
summary(australian)
#Obtenemos las medias de las variables o con summary o con apply
medias_australian <- apply(australian,2,mean)
medias_australian
#Para la desviación estandar usamos apply
desviacion_estandar_australian <- apply(australian,2,sd)
desviacion_estandar_australian
#Varianza
varianza_australian <- apply(australian,2,var)
varianza_australian
#Desviación absoluta de la mediana
desviacion_absoluta_de_la_mediana_australian <- apply(australian,2,mad)
desviacion_absoluta_de_la_mediana_australian
#Rango intercuartil
rango_intercuartil_australian <- apply(australian,2,IQR)
rango_intercuartil_australian

#APARTADO A2: Gráficos
plot(australian)
#Directamente con plot, podemos ver las graficas de cada variable con el resto
par(mfrow=c(3,3))
nombres_australian = names(australian)
plot_con_respecto_a_y <- function(x,y){
  plot(australian[,y]~australian[,x],xlab=nombres_australian[x],ylab="15")
}
sapply(1:9,plot_con_respecto_a_y,15)
par(mfrow=c(3,2))
sapply(10:14,plot_con_respecto_a_y,15)

plot(australian)
#Directamente con plot, podemos ver las graficas de cada variable con el resto

#APARTADO A3: Descripción
#Podemos encontrar información en la web de Keel: http://sci2s.ugr.es/keel/dataset.php?cod=53#sub1
correlacion_australian <- cor(australian)
correlacion_australian <- cor(australian)[,15]
correlacion_australian[cor(australian)[,15] > 0.5 | cor(australian)[,15] < -0.5]






###############################################################
###############################################################
###############################################################
###################### B) REGRESIÓN ###########################
###############################################################
###############################################################

rm(list=ls())
baseball <- read.csv("baseball/baseball.dat", comment.char = "@", header = FALSE)
names(baseball) <- c("Batting_average", "On-base_percentage", "Runs", "Hits", "Doubles", "Triples", 
                     "HomeRuns", "Runs_batted_in", "Walks", "Strike-Outs", "Stolen_bases", "Errors", 
                     "Free_agency_eligibility", "Free_agent", "Arbitration_eligibility", "Arbitration", "Salary")
attach(baseball)

###############################################################
##################### REGRESIÓN SIMPLE ########################
###############################################################

##Modelo lineal para Runs
fitRuns=lm(Salary~Runs)
#fitRuns
##Modelo lineal para Hits
fitHits=lm(Salary~Hits)
#fitHits
##Modelo lineal para Dobules
fitDoubles=lm(Salary~Doubles)
#fitDoubles
##Modelo lineal para HomeRuns
fitHomeRuns=lm(Salary~HomeRuns)
#fitHomeRuns
##Modelo lineal para Runs_batted_in
fitRunsBattedIn=lm(Salary~Runs_batted_in)
#fitRunsBattedIn

##Creo la matriz para obtener el r cuadrado ajustado
mat_r_squ <- matrix(c(summary(fitRuns)$adj.r.squared,
                             summary(fitHits)$adj.r.squared,
                             summary(fitDoubles)$adj.r.squared,
                             summary(fitHomeRuns)$adj.r.squared,
                             summary(fitRunsBattedIn)$adj.r.squared))
rownames(mat_r_squ) <- c("fitRuns","fitHits","fitDoubles",
                                "fitHomeRuns","fitRunsBattedIn")
colnames(mat_r_squ) <- c("adj.r.squared")

##Creo la matriz para obtener el p valor
mat_p_val <- matrix(c(summary(fitRuns)$coefficients["Runs","Pr(>|t|)"],
                      summary(fitHits)$coefficients["Hits","Pr(>|t|)"],
                      summary(fitDoubles)$coefficients["Doubles","Pr(>|t|)"],
                      summary(fitHomeRuns)$coefficients["HomeRuns","Pr(>|t|)"],
                      summary(fitRunsBattedIn)$coefficients["Runs_batted_in","Pr(>|t|)"]))
colnames(mat_p_val) <- c("p-value")
regresion_simple <- cbind(mat_r_squ,mat_p_val)

##Creo la matriz para obtener el rmse
mat_rmse <- matrix(c(sqrt(sum(fitRuns$residuals^2)/(length(fitRuns$residuals)-2)),
                     sqrt(sum(fitHits$residuals^2)/(length(fitHits$residuals)-2)),
                     sqrt(sum(fitDoubles$residuals^2)/(length(fitDoubles$residuals)-2)),
                     sqrt(sum(fitHomeRuns$residuals^2)/(length(fitHomeRuns$residuals)-2)),
                     sqrt(sum(fitRunsBattedIn$residuals^2)/(length(fitRunsBattedIn$residuals)-2))))
colnames(mat_rmse) <- c("RMSE")
##Uno los matrices y la muestro
regresion_simple <- cbind(regresion_simple,mat_rmse)
regresion_simple

##Creo las gráficas de los modelos realizados
par(mfrow=c(3,2))
plot(Runs,Salary)
abline(fitRuns,col="red")
#confint(fitRuns)
plot(Hits,Salary)
abline(fitHits,col="green")
#confint(fitHits)
plot(Doubles,Salary)
abline(fitDoubles,col="blue")
#confint(fitDoubles)
plot(HomeRuns,Salary)
abline(fitHomeRuns,col="orange")
#confint(fitHomeRuns)
plot(Runs_batted_in,Salary)
abline(fitRunsBattedIn,col="brown")
#confint(fitRunsBattedIn)
par(mfrow=c(1,1))

##Como la siguiente parte es realizar los modelos de
##regresión múltiple, voy a realizar las funciones que se vieron
##para realizar cross-validation pero modificadas para obtener los
##datos de una manera más clara

##Función para realizar cross-validation de los modelos lm
run_lm_fold <- function(i, x,modelo, tt = "test") {
  file <- paste(x, "-5-", i, "tra.dat", sep="")
  x_tra <- read.csv(file, comment.char="@") 
  file <- paste(x, "-5-", i, "tst.dat", sep="")
  x_tst <- read.csv(file, comment.char="@")
  In <- length(names(x_tra)) - 1 
  names(x_tra)[1:In] <- paste ("X", 1:In, sep="") 
  names(x_tra)[In+1] <- "Y"
  names(x_tst)[1:In] <- paste ("X", 1:In, sep="") 
  names(x_tst)[In+1] <- "Y"
  if (tt == "train") {
    test <- x_tra
  } 
  else { 
    test <- x_tst
  }
  fitMulti=lm(modelo,x_tra)
  yprime=predict(fitMulti,test)
  mse <- sum(abs(test$Y-yprime)^2)/length(yprime) ##MSE
  resumen <- summary(fitMulti)
  #Error estandar residual
  a <- 100*(resumen$sigma/(mean(x_tra$Y)))
  a <- format(a,digits=3)
  #Coeficiente de determinacion R2
  b <- resumen$r.squared
  b <- format(b,digits=3)
  #Coeficiente de determinacion R2 ajustado
  c <- resumen$adj.r.squared
  c <- format(c,digits=3)
  #RMSE
  d <- sqrt(sum(fitMulti$residuals^2)/(length(fitMulti$residuals)-2))
  d <- format(d,digits = 3)
  salida <- list("EER" = a, "R2" = b, "R2_Adj" = c, "MSE" = mse, "RMSE" = d) 
  return (salida)
}

##Función para realizar el análisis de cross-validation
Analisis_lm_fold <- function(modelo, nombre){
  lmMSEtrain<-(sapply(1:5,run_lm_fold,nombre,modelo,"train")) 
  lmMSEtest<-(sapply(1:5,run_lm_fold,nombre,modelo,"test"))
  
  #Error Estandar Residual
  a <- mean(as.numeric(lmMSEtest[1,]))
  at <- ifelse(a<10,"Si","No")
  a <- format(a,digits=3)
  # Coeficiente de determinacion R2
  b <- mean(as.numeric(lmMSEtest[2,]))
  bt <- ifelse(b>0.8,"Si", "No")
  # Coeficiente de determinacion R2 ajustado
  c <- mean(as.numeric(lmMSEtest[3,]))
  ct <- ifelse(abs(b-c)<0.01,"Si","No")
  b <- format(b,digits = 3)
  c <- format(c,digits = 3)
  d <- mean(as.numeric(lmMSEtest[5,]))
  salida <- list ( "EER"     = a, "T1" = at,
                   "R2"      = b, "T2" = bt,
                   "R2_Adj"  = c, "T3" = ct,
                   "MSEtrain" = mean(as.numeric(lmMSEtrain[4,])),
                   "MSEtest" = mean(as.numeric(lmMSEtest[4,])),
                   "RMSE" = d) 
  return (salida)
}

nombre <- "baseball/baseball"
#Número de las variables:Runs 3, Hits 4, Doubles 5, HomeRuns 7, Runs_batted_in 8
modelo1 <- Y~X3
modelo2 <- Y~X4
modelo3 <- Y~X5
modelo4 <- Y~X7
modelo5 <- Y~X8
modelo6 <- Y~.
lm1 <- Analisis_lm_fold(modelo1,nombre)
lm2 <- Analisis_lm_fold(modelo2,nombre)
lm3 <- Analisis_lm_fold(modelo3,nombre)
lm4 <- Analisis_lm_fold(modelo4,nombre)
lm5 <- Analisis_lm_fold(modelo5,nombre)
lm6 <- Analisis_lm_fold(modelo6,nombre)
regresion_simple_cv <- data.frame(rbind(model1=lm1,model2=lm2,model3=lm3,model4=lm4,model5=lm5,model6=lm6))
regresion_simple_cv
##Podemos ver como de momento el mejor modelo sería el modelo6, con todas las variables,
##ya que obtenemos un R2 ajustado de 0,68, muy superior al resto. Además el MSE de test medio
##es el más bajo de todos los modelos. Pero para los modelos simples, tenemos que el que mejor
##R2 ajustado sería el modelo5, el cual tiene también el MSEtest más pequeño. 



###############################################################
################### REGRESIÓN MÚLTIPLE ########################
###############################################################

##Vamos a pasar ahora a buscar el mejor modelo de regresión múltiple
fitM1=lm(Salary~.,data=baseball)
summary(fitM1) #R2adj 0.686
##Voy a quitar algunas de las que parece que tienen menos influencia como:
##Doubles, Hits, Triples
fitM2=lm(Salary~. -Doubles-Hits-Triples,data=baseball)
summary(fitM2) #R2adj 0.688 hemos mejorado
modelo7 <- Y~. -X4-X5-X6
lm7 <- Analisis_lm_fold(modelo7,nombre)
#lm7
##Sigo quitando, por ejemplo: Runs, Batting_average
fitM3=lm(Salary~. -Doubles-Hits-Triples-Runs-Batting_average,data=baseball)
summary(fitM3) #R2adj 0.6881
modelo8 <- Y~. -X4-X5-X6-X3-X1
lm8 <- Analisis_lm_fold(modelo8,nombre)
#lm8
##Pruebo a quitar: `On-base_percentage`
fitM4=lm(Salary~. -Doubles-Hits-Triples-Runs-Batting_average-`On-base_percentage`,data=baseball)
summary(fitM4) #R2adj 0.688 
modelo9 <- Y~. -X4-X5-X6-X3-X1-X2
lm9 <- Analisis_lm_fold(modelo9,nombre)
#lm9
##Pruebo a quitar: Errors
fitM5=lm(Salary~. -Doubles-Hits-Triples-Runs-Batting_average-`On-base_percentage`-Errors,data=baseball)
summary(fitM5) #R2adj 0.6876 
modelo10 <- Y~. -X4-X5-X6-X3-X1-X2-X12
lm10 <- Analisis_lm_fold(modelo10,nombre)
#lm10
##Pruebo a quitar: Walks y Arbitration
fitM6=lm(Salary~. -Doubles-Hits-Triples-Runs-Batting_average-`On-base_percentage`-Errors-Walks-Arbitration,data=baseball)
summary(fitM6) #R2adj 0.685
modelo11 <- Y~. -X4-X5-X6-X3-X1-X2-X12-X9-X16
lm11 <- Analisis_lm_fold(modelo11,nombre)
#lm11

##Llegados a este punto las variables que nos quedan que parecen más relevantes son:
##X7:HomeRuns , X8:Runs_batted_in ,X10:`Strike-Outs` ,X11:Stolen_bases ,
##X13:Free_agency_eligibility ,X14:Free_agent ,X15:Arbitration_eligibility 
fitM7=lm(Salary~HomeRuns*Runs_batted_in*`Strike-Outs`*Stolen_bases*Free_agency_eligibility*Free_agent*Arbitration_eligibility,data=baseball)
summary(fitM7) #R2adj 0.7932 el mejor hasta el momento
modelo12 <- Y~X7*X8*X10*X11*X13*X14*X15
lm12 <- Analisis_lm_fold(modelo12,nombre)
#lm12
##Las variables que interfieren (cuando p-value es pequeño) son: Free_agent, Runs_batted_in, 
##Free_agency_eligibility, Stolen_bases, `Strike-Outs`,HomeRuns
##Por lo que voy a probar a eliminar la variable Arbitration_eligibility
fitM8=lm(Salary~HomeRuns*Runs_batted_in*`Strike-Outs`*Stolen_bases*Free_agency_eligibility*Free_agent,data=baseball)
summary(fitM8) #R2adj 0.7195 
modelo13 <- Y~X7*X8*X10*X11*X13*X14
lm13 <- Analisis_lm_fold(modelo13,nombre)
#lm13
#Sigo probando modelos
fitM9=lm(Salary~(HomeRuns^2)*(Runs_batted_in^2)*(`Strike-Outs`^2)*Stolen_bases*Free_agency_eligibility*Free_agent*Arbitration_eligibility,data=baseball)
summary(fitM9) #R2adj 0.7932 igual que el mejor 
modelo14 <- Y~(X7^2)*(X8^2)*(X10^2)*X11*X13*X14*X15
lm14 <- Analisis_lm_fold(modelo14,nombre)
#lm14

fitM10=lm(Salary~(HomeRuns^2)*(Runs_batted_in^2)*(`Strike-Outs`^2)*(Stolen_bases^2)*(Free_agency_eligibility^2)*(Free_agent^2)*(Arbitration_eligibility^2),data=baseball)
summary(fitM10) #R2adj 0.7932 igual que el mejor 
modelo15 <- Y~(X7^2)*(X8^2)*(X10^2)*(X11^2)*(X13^2)*(X14^2)*(X15^2)
lm15 <- Analisis_lm_fold(modelo15,nombre)
#lm15

fitM11=lm(Salary~I(HomeRuns^2)*I(Runs_batted_in^2)*I(`Strike-Outs`^2)*(Stolen_bases^2)*(Free_agency_eligibility^2)*(Free_agent^2)*(Arbitration_eligibility^2),data=baseball)
summary(fitM11) #R2adj 0.7885 
modelo16 <- Y~I(X7^2)*I(X8^2)*I(X10^2)*(X11^2)*(X13^2)*(X14^2)*(X15^2)
lm16 <- Analisis_lm_fold(modelo16,nombre)
#lm16

fitM12=lm(Salary~I(HomeRuns^2)*I(Runs_batted_in^2)*I(`Strike-Outs`^2)*I(Stolen_bases^2)*I(Free_agency_eligibility^2)*I(Free_agent^2)*I(Arbitration_eligibility^2),data=baseball)
summary(fitM12) #R2adj 0.7854
modelo17 <- Y~I(X7^2)*I(X8^2)*I(X10^2)*I(X11^2)*I(X13^2)*I(X14^2)*I(X15^2)
lm17 <- Analisis_lm_fold(modelo17,nombre)
#lm17

fitM13=lm(Salary~I(HomeRuns^2)+I(Runs_batted_in^2)+I(`Strike-Outs`^2)+I(Stolen_bases^2)+I(Free_agency_eligibility^2)+I(Free_agent^2)+I(Arbitration_eligibility^2),data=baseball)
summary(fitM13) #R2adj 0.6914
modelo18 <- Y~I(X7^2)+I(X8^2)+I(X10^2)+I(X11^2)+I(X13^2)+I(X14^2)+(X15^2)
lm18 <- Analisis_lm_fold(modelo18,nombre)
#lm18
##X7:HomeRuns , X8:Runs_batted_in ,X10:`Strike-Outs` ,X11:Stolen_bases ,
##X13:Free_agency_eligibility ,X14:Free_agent ,X15:Arbitration_eligibility 
fitM14=lm(Salary~I(Runs_batted_in*HomeRuns^2)+I(`Strike-Outs`*Stolen_bases^2)+I(Free_agency_eligibility*Free_agent^2)+I(Arbitration_eligibility^2),data=baseball)
summary(fitM14) #R2adj 0.3087
modelo19 <- Y~I(X8*X7^2)+I(X10*X11^2)+I(X13*X14^2)+I(X15^2)
lm19 <- Analisis_lm_fold(modelo19,nombre)
#lm19

fitM15=lm(Salary~I(Runs_batted_in*Free_agency_eligibility^2)+I(Runs_batted_in*Arbitration_eligibility^2)+I(Stolen_bases*Free_agency_eligibility^2)+I(Stolen_bases*Arbitration_eligibility^2)+I(HomeRuns*`Strike-Outs`*Free_agent^2),data=baseball)
summary(fitM15) #R2adj 0.7363
modelo20 <- Y~I(X8*X13^2)+I(X8*X15^2)+I(X11*X13^2)+I(X11*X15^2)+I(X7*X10*X14^2)
lm20 <- Analisis_lm_fold(modelo20,nombre)
#lm20

fitM16=lm(Salary~I(Runs_batted_in*Free_agency_eligibility^2)+I(Runs_batted_in*Arbitration_eligibility^2)+I(Stolen_bases*Free_agency_eligibility^2)+I(Stolen_bases*Arbitration_eligibility^2),data=baseball)
summary(fitM16) #R2adj 0.7363
modelo21 <- Y~I(X8*X13^2)+I(X8*X15^2)+I(X11*X13^2)+I(X11*X15^2)
lm21 <- Analisis_lm_fold(modelo21,nombre)
#lm21

fitM17=lm(Salary~I(Runs_batted_in*Free_agency_eligibility^2)+I(Runs_batted_in*Arbitration_eligibility^2)+I(Stolen_bases*Free_agency_eligibility^2),data=baseball)
summary(fitM17) #R2adj 0.7303
modelo22 <- Y~I(X8*X13^2)+I(X8*X15^2)+I(X11*X13^2)
lm22 <- Analisis_lm_fold(modelo22,nombre)
#lm22

fitM18=lm(Salary~I(Runs_batted_in^2)*Free_agency_eligibility+(`Strike-Outs`+Stolen_bases)*Free_agency_eligibility+I((Runs_batted_in+Stolen_bases)^3)*Arbitration_eligibility,data=baseball)
summary(fitM18) #R2adj 0.7479
modelo23 <- Y~I(X8^2)*X13+(X10+X11)*X13+I((X8+X11)^3)*X15
lm23 <- Analisis_lm_fold(modelo23,nombre)
#lm23

fitM19=lm(Salary~I(Runs_batted_in^2)*Free_agency_eligibility+(`Strike-Outs`+Stolen_bases)*Free_agency_eligibility+I((Runs_batted_in+Stolen_bases)^3)*Arbitration_eligibility+
            (Batting_average+`On-base_percentage`+Runs+Hits+Doubles+Triples+HomeRuns+Walks+Errors)*Free_agency_eligibility,data=baseball)
summary(fitM19) #R2adj 0.7549
modelo24 <- Y~I(X8^2)*X13+(X10+X11)*X13+I((X8+X11)^3)*X15+(X1+X2+X3+X4+X5+X6+X7+X9+X12)*X13
lm24 <- Analisis_lm_fold(modelo24,nombre)
#lm24

fitM20=lm(Salary~I(Runs_batted_in^2)*Free_agency_eligibility+(`Strike-Outs`+Stolen_bases)*Free_agency_eligibility+I((Runs_batted_in+Stolen_bases)^3)*Arbitration_eligibility+
            (Batting_average+`On-base_percentage`+Runs+Hits+Doubles+Triples+HomeRuns+Walks+Errors)*Free_agency_eligibility+
            (Batting_average+`On-base_percentage`+Runs+Hits+Doubles+Triples+HomeRuns+Walks+Errors)*Arbitration_eligibility+
            (Batting_average+`On-base_percentage`+Runs+Hits+Doubles+Triples+HomeRuns+Walks+Errors)*Free_agent,data=baseball)
summary(fitM20) #R2adj 0.7704
modelo25 <- Y~I(X8^2)*X13+(X10+X11)*X13+I((X8+X11)^3)*X15+(X1+X2+X3+X4+X5+X6+X7+X9+X12)*X13+(X1+X2+X3+X4+X5+X6+X7+X9+X12)*X15+(X1+X2+X3+X4+X5+X6+X7+X9+X12)*X14
lm25 <- Analisis_lm_fold(modelo25,nombre)
#lm25

fitM21=lm(Salary~I(Runs_batted_in^2)*Free_agency_eligibility+(`Strike-Outs`+Stolen_bases)*Free_agency_eligibility+I((Runs_batted_in+Stolen_bases)^3)*Arbitration_eligibility+
            (Batting_average+`On-base_percentage`+Runs+Hits+Doubles+Triples+HomeRuns+Walks+Errors)*Free_agency_eligibility+
            (Batting_average+`On-base_percentage`+Runs+Hits+Doubles+Triples+HomeRuns+Walks+Errors)*Arbitration_eligibility+
            (Batting_average+`On-base_percentage`+Runs+Hits+Doubles+Triples+HomeRuns+Walks+Errors)*Free_agent+
            (Batting_average+`On-base_percentage`+Runs+Hits+Doubles+Triples+HomeRuns+Walks+Errors)*Arbitration,data=baseball)
summary(fitM21) #R2adj 0.7795
modelo26 <- Y~I(X8^2)*X13+(X10+X11)*X13+I((X8+X11)^3)*X15+(X1+X2+X3+X4+X5+X6+X7+X9+X12)*X13+(X1+X2+X3+X4+X5+X6+X7+X9+X12)*X15+(X1+X2+X3+X4+X5+X6+X7+X9+X12)*X14+(X1+X2+X3+X4+X5+X6+X7+X9+X12)*X16
lm26 <- Analisis_lm_fold(modelo26,nombre)
#lm26

dflm <- data.frame(rbind(model1=lm1,model2=lm2,model3=lm3,model4=lm4,model5=lm5,model6=lm6,
                       model7=lm7,model8=lm8,model9=lm9,model10=lm10,model11=lm11,model12=lm12,model13=lm13
                       ,model14=lm14,model15=lm15,model16=lm16,model17=lm17,model18=lm18,model19=lm19,model20=lm20
                       ,model21=lm21,model22=lm22,model23=lm23,model24=lm24,model25=lm25,model26=lm26))
dflm
dflm[which.min(as.numeric(dflm[,8])),]
##El modelo que ha obtenido un menor MSE en test ha sido el modelo 20
##Vemos que su R2 de 0,747 y un R2adj de 0,742 por lo que se explica un 74% del problema
dflm[which.min(as.numeric(dflm[,7])),]
##El modelo 16 tiene el menor MSE en train
##Este modelo tiene muy buen R2

dflm[which.max(as.numeric(dflm[,7])),]
##El modelo con un mayor MSE en training es el modelo 19
dflm[which.max(as.numeric(dflm[,8])),]
##El modelo 17 obtiene el mayor MSE en test, por lo que indica sobre-ajuste del modelo. 
dflm[which.max(as.numeric(dflm[,3])),]
##El modelo 16 ha obtenido el mayor R2
dflm[which.max(as.numeric(dflm[,5])),]
##Siendo el mismo que ha obtenido el R2adj mayor. 

dflm[c(16,20),]
##Por lo tanto tenemos para elegir entre dos modelos el 16 o el 20.
##Como mejor modelo para esta parte eligiría el modelo 20 al tener un menor MSEtest.

modelo20


###############################################################
################### KNN PARA REGRESIÓN ########################
###############################################################
set.seed(1234)
library(kknn)
##Si realizamos un análisis como el de las transparencias, antes de aplicar knn-fold:
##Modelo para Runs
fitRunsknn=kknn(Salary~Runs,baseball,baseball)
##Modelo para Hits
fitHitsknn=kknn(Salary~Hits,baseball,baseball)
##Modelo para Dobules
fitDoublesknn=kknn(Salary~Doubles,baseball,baseball)
##Modelo para HomeRuns
fitHomeRunsknn=kknn(Salary~HomeRuns,baseball,baseball)
##Modelo para Runs_batted_in
fitRunsBattedInknn=kknn(Salary~Runs_batted_in,baseball,baseball)

par(mfrow=c(3,2))
plot(Salary~Runs)
points(Runs,fitRunsknn$fitted.values,col="red",pch=20)
plot(Salary~Hits)
points(Hits,fitHitsknn$fitted.values,col="green",pch=20)
plot(Salary~Doubles)
points(Doubles,fitDoublesknn$fitted.values,col="blue",pch=20)
plot(Salary~HomeRuns)
points(HomeRuns,fitHomeRunsknn$fitted.values,col="orange",pch=20)
plot(Salary~Runs_batted_in)
points(Runs_batted_in,fitRunsBattedInknn$fitted.values,col="brown",pch=20)
par(mfrow=c(1,1))

yprime=fitRunsknn$fitted.values
rmse_runs <- sqrt(sum((Salary-yprime)^2)/length(yprime))
yprime=fitHitsknn$fitted.values
rmse_hits <- sqrt(sum((Salary-yprime)^2)/length(yprime))
yprime=fitDoublesknn$fitted.values
rmse_doubles <- sqrt(sum((Salary-yprime)^2)/length(yprime))
yprime=fitHomeRunsknn$fitted.values
rmse_home_runs <- sqrt(sum((Salary-yprime)^2)/length(yprime))
yprime=fitRunsBattedInknn$fitted.values
rmse_runs_batted_in <- sqrt(sum((Salary-yprime)^2)/length(yprime))


mat_rmse_knn <- matrix(c(rmse_runs,rmse_hits,rmse_doubles,rmse_home_runs,rmse_runs_batted_in))
colnames(mat_rmse_knn) <- c("RMSEknn")
##Uno los matrices y la muestro
regresion_simple_knn <- cbind(regresion_simple,mat_rmse_knn)
regresion_simple_knn
#Viendo que el RMSEknn es menor que el de lm

##Función para realizar cross-validation de los modelos knn
run_knn_fold <- function(i, x,modelo, tt = "test") {
  file <- paste(x, "-5-", i, "tra.dat", sep="")
  x_tra <- read.csv(file, comment.char="@") 
  file <- paste(x, "-5-", i, "tst.dat", sep="")
  x_tst <- read.csv(file, comment.char="@")
  In <- length(names(x_tra)) - 1 
  names(x_tra)[1:In] <- paste ("X", 1:In, sep="") 
  names(x_tra)[In+1] <- "Y"
  names(x_tst)[1:In] <- paste ("X", 1:In, sep="") 
  names(x_tst)[In+1] <- "Y"
  if (tt == "train") {
    test <- x_tra
  } 
  else { 
    test <- x_tst
  }
  fitMulti=kknn(modelo,x_tra,test)
  yprime=fitMulti$fitted.values
  rmse <- sqrt(sum((test$Y-yprime)^2)/length(yprime))
  mse <- sum(abs(test$Y-yprime)^2)/length(yprime) ##MSE
  salida <- list("MSE"=mse, "RMSE"=rmse)
  return (salida)
}

##Función para realizar el análisis de cross-validation
Analisis_knn_fold <- function(modelo, nombre){
  knnMSEtrain<-(sapply(1:5,run_knn_fold,nombre,modelo,"train")) 
  knnMSEtest<-(sapply(1:5,run_knn_fold,nombre,modelo,"test"))
  a <- mean(as.numeric(knnMSEtest[2,]))
  salida <- list ( "MSEtrain" = mean(as.numeric(knnMSEtrain[1,])),
                   "MSEtest" = mean(as.numeric(knnMSEtest[1,])),
                   "RMSE"=a) 
  return (salida)
}
set.seed(1234)
knn1 <- Analisis_knn_fold(modelo1,nombre)
knn2 <- Analisis_knn_fold(modelo2,nombre)
knn3 <- Analisis_knn_fold(modelo3,nombre)
knn4 <- Analisis_knn_fold(modelo4,nombre)
knn5 <- Analisis_knn_fold(modelo5,nombre)
knn6 <- Analisis_knn_fold(modelo6,nombre)
knn7 <- Analisis_knn_fold(modelo7,nombre)
knn8 <- Analisis_knn_fold(modelo8,nombre)
knn9 <- Analisis_knn_fold(modelo9,nombre)
knn10 <- Analisis_knn_fold(modelo10,nombre)
knn11 <- Analisis_knn_fold(modelo11,nombre)
knn12 <- Analisis_knn_fold(modelo12,nombre)
knn13 <- Analisis_knn_fold(modelo13,nombre)
knn14 <- Analisis_knn_fold(modelo14,nombre)
knn15 <- Analisis_knn_fold(modelo15,nombre)
knn16 <- Analisis_knn_fold(modelo16,nombre)
knn17 <- Analisis_knn_fold(modelo17,nombre)
knn18 <- Analisis_knn_fold(modelo18,nombre)
knn19 <- Analisis_knn_fold(modelo19,nombre)
knn20 <- Analisis_knn_fold(modelo20,nombre)
knn21 <- Analisis_knn_fold(modelo21,nombre)
knn22 <- Analisis_knn_fold(modelo22,nombre)
knn23 <- Analisis_knn_fold(modelo23,nombre)
knn24 <- Analisis_knn_fold(modelo24,nombre)
knn25 <- Analisis_knn_fold(modelo25,nombre)
knn26 <- Analisis_knn_fold(modelo26,nombre)

dfknn <- data.frame(rbind(model1=knn1,model2=knn2,model3=knn3,model4=knn4,model5=knn5,model6=knn6,
                       model7=knn7,model8=knn8,model9=knn9,model10=knn10,model11=knn11,model12=knn12,model13=knn13
                       ,model14=knn14,model15=knn15,model16=knn16,model17=knn17,model18=knn18,model19=knn19,model20=knn20
                       ,model21=knn21,model22=knn22,model23=knn23,model24=knn24,model25=knn25,model26=knn26))
dfknn
##Buscamos el que menor MSE en test tenga:
dfknn[which.min(as.numeric(dfknn[,2])),]
##Que es el modelo 22
##El que menor MSE en train tiene es:
dfknn[which.min(as.numeric(dfknn[,1])),]
##El modelo 25
##El que mayor MSE en test presenta es:
dfknn[which.max(as.numeric(dfknn[,2])),]
##El modelo 3
##Y el que mayor MSE en train tiene es:
dfknn[which.max(as.numeric(dfknn[,1])),]
##También el modelo 3

dfknn[c(22,25),]
##Por lo tanto tenemos que los modelos 22 y 25 presentan MSE bajos para test o train,
##y como no coincide con ninguno de los mejores modelo que dieron en lm, para esta parte
##creo que el mejormodelo sería el modelo 22 al tener un MSE de test menor
modelo22

##Si queremos quedarnos con el mejor modelo de entre knn y lm tenemos que:
dflm[20,]
dfknn[22,]
##Si comparamos por MSEtest tenemos que:
dflm[20,8]
dfknn[22,2]
##El modelo 20 tiene un menor valor de MSEtest
##Si observamos el de train:
dflm[20,7]
dfknn[22,1]
##El modelo 22 tiene un menor valor de MSEtrain

##Para la comparativa final me decantaré final me quedaré con el modelo 20 por tener
##un valor de MSEtest más pequeño


###############################################################
############### COMPARACIÓN DE ALGORITMOS #####################
###############################################################

##Comparativa general entre distintos algoritmos
##leemos la tabla con los errores medios de test
resultados <- read.csv("regr_test_alumnos.csv")
tablatst <- cbind(resultados[,2:dim(resultados)[2]]) 
colnames(tablatst) <- names(resultados)[2:dim(resultados)[2]] 
rownames(tablatst) <- resultados[,1]
##leemos la tabla con los errores medios de test
resultados <- read.csv("regr_train_alumnos.csv")
tablatra <- cbind(resultados[,2:dim(resultados)[2]]) 
colnames(tablatra) <- names(resultados)[2:dim(resultados)[2]] 
rownames(tablatra) <- resultados[,1]

tablatst["baseball",1] <- as.numeric(dflm[20,8])
tablatst["baseball",2] <- as.numeric(dfknn[20,2])
tablatst["baseball",]

tablatra["baseball",1] <- as.numeric(dflm[20,7])
tablatra["baseball",2] <- as.numeric(dfknn[20,1])
tablatra["baseball",]

##Comparativa por pares de LM y KNN para test (Wilcoxon´s test)
##lm (other) vs knn (ref)
## + 0.1 porque wilcox R falla para valores == 0 en la tabla
difs <- (tablatst[,1] - tablatst[,2]) / tablatst[,1]
wilc_1_2 <- cbind(ifelse (difs<0, abs(difs)+0.1, 0+0.1), ifelse (difs>0, abs(difs)+0.1, 0+0.1)) 
colnames(wilc_1_2) <- c(colnames(tablatst)[1], colnames(tablatst)[2])
head(wilc_1_2)

##Se aplica el test Wilcoxon para interpretar los resultados
LMvsKNNtst <- wilcox.test(wilc_1_2[,1], wilc_1_2[,2], alternative = "two.sided", paired=TRUE) 
Rmas <- LMvsKNNtst$statistic
pvalue <- LMvsKNNtst$p.value
LMvsKNNtst <- wilcox.test(wilc_1_2[,2], wilc_1_2[,1], alternative = "two.sided", paired=TRUE) 
Rmenos <- LMvsKNNtst$statistic
Rmas
Rmenos
pvalue
##Al ser p-valor mayor a 0.05 no existen diferencias significativas entre ambos
##Solo un 13,49% de confianza de que existan diferencias ((1-pvalue)*100)
##Como el p-value es 0.865, mayor que el nivel de signifiación 0.05, no somos capaces
##a rechazar la hipótesis nula y concluir que no hay suficiente evidencia en los datos.
##Por lo tanto se acepta la hipotesis nula: las dos distribuciones son iguales

##Comparativa por pares de LM y KNN para train (Wilcoxon´s test)
##lm (other) vs knn (ref)
## + 0.1 porque wilcox R falla para valores == 0 en la tabla
difs <- (tablatra[,1] - tablatra[,2]) / tablatra[,1]
wilc_1_2 <- cbind(ifelse (difs<0, abs(difs)+0.1, 0+0.1), ifelse (difs>0, abs(difs)+0.1, 0+0.1)) 
colnames(wilc_1_2) <- c(colnames(tablatra)[1], colnames(tablatra)[2])
head(wilc_1_2)

##Se aplica el test Wilcoxon para interpretar los resultados
LMvsKNNtst <- wilcox.test(wilc_1_2[,1], wilc_1_2[,2], alternative = "two.sided", paired=TRUE) 
Rmas <- LMvsKNNtst$statistic
pvalue <- LMvsKNNtst$p.value
LMvsKNNtst <- wilcox.test(wilc_1_2[,2], wilc_1_2[,1], alternative = "two.sided", paired=TRUE) 
Rmenos <- LMvsKNNtst$statistic
Rmas
Rmenos
pvalue
##Por lo contrario, los reusltados obtenidos en el training el test de Wilcoxon muestra
##un p-value menor que el nivel de significación 0.05, por lo que el p-value es significativo.
##Entonces rechazamos la hipotesis nula y decimos que las distribuciones son distintas. 
##Esta interpretación tiene menor peso que la realizada anteriormente para test, ya que en el 
##training los resultados son con datos ya conocidos por lo que se puede decir que hay 
##sobre-ajuste para cada resultado de las bases de datos y que sean distintas las distribuciones
##entre los ajustes lm y knn.


##Comparativas múltiples con Friedman y post-hoc Holm
test_friedman <- friedman.test(as.matrix(tablatst)) 
test_friedman
##Existen diferencias significativas al menos entre un par de algoritmos ya que p-value es menor a 0.05

##Aplicamos post-hoc Holm
tam <- dim(tablatst)
groups <- rep(1:tam[2], each=tam[1])
pairwise.wilcox.test(as.matrix(tablatst), groups, p.adjust = "holm", paired = TRUE)
##Existen diferencias significativas a favor de M5´
##(3vs1 0.081 y 3vs2, con aprox. 90% confianza)
##lm vs knn, pueden ser considerados equivalentes

##Comparativas múltiples con Friedman y post-hoc Holm para train
test_friedman <- friedman.test(as.matrix(tablatra)) 
test_friedman
##Existen diferencias significativas al menos entre un par de algoritmos ya que p-value es menor a 0.05

##Aplicamos post-hoc Holm
tam <- dim(tablatra)
groups <- rep(1:tam[2], each=tam[1])
pairwise.wilcox.test(as.matrix(tablatra), groups, p.adjust = "holm", paired = TRUE)
##Existen diferencias significativas a favor en todos los casos, lo que sugiere que cada ajuste se ha
##adaptado a la base de datos, por lo que puede significar un sobre-ajuste.




###############################################################
###############################################################
###############################################################
##################### C) CLASIFICACIÓN ########################
###############################################################
###############################################################

rm(list=ls())
australian <- read.csv("australian/australian.dat", comment.char = "@", header = FALSE)
attach(australian)
##8 y 15
###############################################################
############################ KNN ##############################
###############################################################

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
##Creamos el conjunto de training y de test y sus etiquetas
tamanio_tt <- nrow(australian)*0.8
australian_train <- australian_n[1:tamanio_tt,]
australian_test <- australian_n[(tamanio_tt+1):nrow(australian),]
australian_train_labels <- australian[1:tamanio_tt,15]
australian_test_labels <- australian[(tamanio_tt+1):nrow(australian),15]
##Entrenamos un modelo de los datos
library(class)
library(kknn)
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
set.seed(1234)
knnFit <- train(australian_train, australian_train_labels, method="knn", metric="Accuracy", tuneGrid = data.frame(.k=1:15))
knnFit
##Si le decimos los valores de k que debe tomar, vemos como con k = 15 conseguimos el mejor modelo

knnPred <- predict(knnModel,newdata = australian_test)
testData = australian[(tamanio_tt+1):nrow(australian),15]
postResample(pred=knnPred, obs = testData)
##Si aplicamos el conjunto de test sobre el modelo creado tenemos un modelo con el 57,9% de explicación




set.seed(1234)
##Voy a crear una función para aplicar knn a los modelos, para ver con que valor de k se obtienen mejores resultados
aplicar_knn <- function(k){
  mi_knn <- knn(train = australian_train, test = australian_test, cl = australian_train_labels, k)
  accu <- postResample(pred = mi_knn, obs = testData)[1]
}
set.seed(1234)
accuracy <- sapply(1:450,aplicar_knn)
#Pinto las gráficas para ver los valores de accuracy
plot(x=1:450,y=accuracy)
plot(x=1:150,y=accuracy[1:150],type="l")
plot(x=100:150,y=accuracy[100:150],type="l")
plot(x=100:120,y=accuracy[100:120],type="l")
max(accuracy)
plot(x=105:115,y=accuracy[105:115],type="p")
lines(x=105:115,y=accuracy[105:115],type="l",col="blue")
#Vemos como hay 2 valores con accuracy = 0.884058 asique tenemos distintos k buenos




####HASTA AQUI BIEN
library(caret)
library(class)
aplicar_knn_fold<-function(train, test, train_labels, test_labels, K)
{
  accuracy<-rep(0,K)
  for(i in 1:K)
  {
    fitMulti <- knn(train, test,cl = train_labels, k=i)
    accuracy[i] <- postResample(pred = fitMulti, obs = test_labels)[1]
  }
  list(accuracy=accuracy)
}

run_knn_fold <- function(i, x, K, tt = "test") {
  file <- paste(x, "-10-", i, "tra.dat", sep="")
  x_tra <- read.csv(file, comment.char="@")
  In <- length(names(x_tra)) - 1 
  names(x_tra)[1:In] <- paste ("X", 1:In, sep="") 
  names(x_tra)[In+1] <- "Y"
  x_tra <- data.frame(lapply(x_tra, normalize))
  x_tra$Y <- factor(x_tra$Y, levels = c(0, 1),labels = c("0", "1"))
  if(tt == "test"){
    file <- paste(x, "-10-", i, "tst.dat", sep="")
    x_tst <- read.csv(file, comment.char="@")
    names(x_tst)[1:In] <- paste ("X", 1:In, sep="") 
    names(x_tst)[In+1] <- "Y"
    x_tst <- data.frame(lapply(x_tst, normalize))
    x_tst$Y <- factor(x_tst$Y, levels = c(0, 1), labels = c("0", "1"))
  }
  if (tt == "train") {
    test_labels <- x_tra[,15]
    test <- x_tra
  }
  else {
    test_labels <- x_tst[,15]
    test <- x_tst
  }
  train_labels <- x_tra[,15]
  aplicar_knn_fold(x_tra[1:14], test[1:14], train_labels, test_labels, K)
}

mean_dataFrame <- function(dataFrame)
{
  n <- length(dataFrame[1]$accuracy)
  m <- rep(0, n)
  for(i in 1:n)
  {
    m[i] <- mean(as.numeric(dataFrame[i,]))
  }
  return(m)	
}

set.seed(1234)
nombre <- "australian/australian"
accuracy_train <- data.frame(sapply(1:10, run_knn_fold, nombre, 10, "train"))
mean_accuracy_train <- mean_dataFrame(accuracy_train)
num.max <- which(mean_accuracy_train==(max(mean_accuracy_train)))
plot(y=mean_accuracy_train, x=c(1:10), xlab="K", ylab="Accuracy", type = "l", 
     lwd=5, col="blue", main="Valores de K y Accuracy para Knn en train")
points(x=num.max, y=mean_accuracy_train[num.max], pch = 19, col="red")
name.max<-"K=" 
name.max<-paste(name.max, num.max, sep = "")
text(x=num.max, y=mean_accuracy_train[num.max], labels=name.max, cex= 1, pos=1)

set.seed(1234)
accuracy_test <- data.frame(sapply(1:10, run_knn_fold, nombre, 10, "test"))
mean_accuracy_test <- mean_dataFrame(accuracy_test)
num.max <- which(mean_accuracy_test==(max(mean_accuracy_test)))
plot(y=mean_accuracy_test, x=c(1:10), xlab="K", ylab="Accuracy", type = "l", 
     lwd=5, col="blue", main="Valores de K y Accuracy para Knn en test")
points(x=num.max, y=mean_accuracy_test[num.max], pch = 19, col="red")
name.max<-"K=" 
name.max<-paste(name.max, num.max, sep = "")
text(x=num.max, y=mean_accuracy_test[num.max], labels=name.max, cex= 1, pos=1)



