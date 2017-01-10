############### A) DESCRIPCIÓN DEL PROBLEMA ###################

###REGRESIÓN
baseball <- read.csv("baseball/baseball.dat", comment.char = "@", header = FALSE)
names(baseball) <- c("Batting_average", "On-base_percentage", "Runs", "Hits", "Doubles", "Triples", 
                     "HomeRuns", "Runs_batted_in", "Walks", "Strike-Outs", "Stolen_bases", "Errors", 
                     "Free_agency_eligibility", "Free_agent", "Arbitration_eligibility", "Arbitration", "Salary")
is.data.frame(baseball)
#Podemos ver como es un data frame la estructura de datos
ncol(baseball)
nrow(baseball)
#Con 17 columnas y 337 filas
#Tenemos que las variables "Batting_average" y "On-base_percentage" son numéricas, y el resto son int

##APARTADO A1: Media, desviación estandar...
summary(baseball)
#Obtenemos las medias de las variables o con summary o con apply
medias_baseball <- apply(baseball,2,mean)
medias_baseball
#Para la desviación estandar usamos apply
desviacion_estandar_baseball <- apply(baseball,2,sd)
desviacion_estandar_baseball
#Varianza
varianza_baseball <- apply(baseball,2,var)
varianza_baseball
#Desviación absoluta de la mediana
desviacion_absoluta_de_la_mediana_baseball <- apply(baseball,2,mad)
desviacion_absoluta_de_la_mediana_baseball
#Rango intercuartil
rango_intercuartil_baseball <- apply(baseball,2,IQR)
rango_intercuartil_baseball

##APARTADO A2: Gráficos
plot(baseball)
#Directamente con plot, podemos ver las gráficas de cada variable con el resto
par(mfrow=c(3,4))
nombres_baseball = names(baseball)
plot_con_respecto_a_y <- function(x,y){
  plot(baseball[,y]~baseball[,x],xlab=nombres_baseball[x],ylab="Salary")
}
sapply(1:12,plot_con_respecto_a_y,17)
par(mfrow=c(2,2))
sapply(13:16,plot_con_respecto_a_y,17)

##APARTADO A3: Descripción
#Podemos encontrar información en la web de Keel: http://sci2s.ugr.es/keel/dataset.php?cod=76#sub1
correlacion_baseball <- cor(baseball)
correlacion_baseball <- cor(baseball)[,17]
correlacion_baseball[cor(baseball)[,17] > 0.5 | cor(baseball)[,17] < -0.5]



###CLASIFICACIÓN
australian <- read.csv("australian/australian.dat", comment.char = "@", header = FALSE)
is.data.frame(australian)
#Podemos ver como es un data frame la estructura de datos
ncol(australian)
nrow(australian)
#Con 15 columnas y 690 filas
#Tenemos que las variables 2,3 y 7 son numéricas, y el resto son int

##APARTADO A1: Media, desviación estandar...
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

##APARTADO A2: Gráficos
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

##APARTADO A3: Descripción
#Podemos encontrar información en la web de Keel: http://sci2s.ugr.es/keel/dataset.php?cod=53#sub1
correlacion_australian <- cor(australian)
correlacion_australian <- cor(australian)[,15]
correlacion_australian[cor(australian)[,15] > 0.5 | cor(australian)[,15] < -0.5]
