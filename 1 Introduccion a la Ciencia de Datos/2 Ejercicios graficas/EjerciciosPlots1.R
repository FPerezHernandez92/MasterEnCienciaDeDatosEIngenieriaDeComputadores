#FRANCISCO PÉREZ HERNÁNDEZ 20076629K

#EJERCICIO 1: Plot distance against stretch
stretch <- c(46,54,48,50,44,42,52)
distance <- c(148,182,173,166,109,141,166)

plot(distance~stretch, main="Distance vs Stretch", xlab = "Stretch", ylab = "Distance", pch = 10, col ="blue")


#EJERCICIO 2:The table on the right have ten observations, taken during the years 1970-79, are on October snow 
#cover for Eurasia (snow cover is in millions of square kilometers).
#2.1 Plot snow.cover versus year.
year <- (1970:1979)
snow.cover <-c(6.5,12.0,14.9,10.0,10.7,7.9,21.9,12.5,14.5,9.2)

help(plot)
plot(snow.cover~year, main="Snow.cover vs Year",xlab="Year",ylab="Snow Cover", col="red",type="p",pch =20)
lines(snow.cover~year,col="red")

#2.2 Plot a histogram of the snow.cover values
hist(snow.cover,main="Histograma de Snow Cover",col="lightblue",xlab="Snow Cover")


#EJERCICIO 3: Given the data in NY.xls
#3.1 Convert ºF to ºC and in to mm.
library("readxl")
NY <- read_excel("NY.xls", na="NR")
#ºF to ºC
NY[,2:8]=((NY[,2:8]-32)*(5/9))
#in to mm
NY[,9:12]=NY[,9:12]*(1/0.039370) 

#3.2 Plot Year vs. Warmest Minimum Temperature
plot(NY[,4]~NY[,1],main="Year vs Warmest Minimum Temperature",ylab="Warmest Minimum Temperatur C",xlab="Year",type="l",col="green")
head(NY)

#3.3 Plot Year vs. Warmest Minimum Temperature and Coldest Minimum Temperature
maximo = max(NY[,4],NY[,5])
minimo = min(NY[,4],NY[,5])
plot(NY[,4]~NY[,1],ylim = c(minimo,maximo),type="l",col="orange",main="Warmest and Coldest minimum temperature by Year", xlab="Year", ylab="Temperature")
lines(NY[,5]~NY[,1],type="l",col="blue")
legend(x = "topleft" ,legend = "Warmest", col="orange",lty=1,cex=0.7)
legend(x="bottomleft",legend = "Coldest", col="blue", lty=1,cex=0.7)

