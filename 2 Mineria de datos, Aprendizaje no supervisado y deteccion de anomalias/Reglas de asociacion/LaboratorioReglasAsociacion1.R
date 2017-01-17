
library(arules)
data()
#Cargamos la BD en nuestra zona de trabajo
data("AdultUCI")
#Consultamos sus dimensiones
dim(AdultUCI)
#Vemos las 2 primeras filas para ver los atributos y sus tipos
AdultUCI[1:2,]

#De los 6 atributos continuos:
#2 los eliminamos porque aporan información redundante: fnlwgt y education-num
AdultUCI[["fnlwgt"]] = NULL
AdultUCI[["education-num"]] = NULL
#Los 4 restantes los dividimos en intervalos
AdultUCI[[ "age"]] = ordered( cut ( AdultUCI[[ "age"]], c(0,25,45,65,100) ) ,labels = c ("Young", "Middle-aged", "Senior", "Old"))
AdultUCI[[ "hours-per-week"]] = ordered( cut ( AdultUCI[[ "hours-per-week"]], c(0,25,40,60,168) ) , labels = c("Part-time", "Full-time", "Over-time", "Workaholic"))
AdultUCI[[ "capital-gain"]] = ordered( cut ( AdultUCI[[ "capital-gain"]], c(-Inf,0,median(AdultUCI[[ "capital-gain"]][AdultUCI[[ "capital-gain"]]>0]), Inf) ) , labels = c("None", "Low", "High"))
AdultUCI[[ "capital-loss"]] = ordered( cut ( AdultUCI[[ "capital-loss"]], c(-Inf,0, median(AdultUCI[[ "capital-loss"]][AdultUCI[[ "capital-loss"]]>0]), Inf) ) , labels = c("None", "Low", "High"))
#Convertimos el data.frame en un conjunto de transacciones con la función as
Adult <- as(AdultUCI,"transactions")
Adult

#Vemos el resumen de la BD
summary(Adult)

#Representar gráficamente la distribución de los items en las transacciones. Como en 
#Adult cada transacción tienen un valor cada atributo/variable, usamos para probarlo la 
#BD Epub (15729 transacciones y 936 items)
data(Epub)
summary(Epub)
image(Epub)

#Para ver gráficamente que items son los más importantes:
#donde el mínimo soporte será 0.1 y reducimos el tamaño de los títulos
itemFrequencyPlot(Adult, support = 0.1, cex.names=0.8)
#Usamos apriori para extraer los itemsets frecuentes con minsop 0.1. 
iAdult <- apriori(Adult, parameter = list(support = 0.1, target="frequent"))
#Orenamos por el valor de soporte
iAdult <- sort(iAdult, by="support") 
#Inspeccionamos los 10 primeros
inspect(head(iAdult, n=10))
#Podemos consultar el tamaño de los itemsets frecuentes
size(iAdult)
#Representamos con un diagrama de barras
barplot(table(size(iAdult)), xlab="itemset size", ylab="count")
#Inspeccionamos los itemsets frecuentes de tamaño 1
inspect(iAdult[size(iAdult)==1])

#Sacamos un vector lógico indicando que itemsets es máximal
imaxAdult <- iAdult[is.maximal(iAdult)]
#Mostramos los 6 primeros ordenados por su valor de soporte
inspect(head(sort(imaxAdult, by="support")))
#Sacamos un vector lógico indicando que itemsets es cerrado
icloAdult <- iAdult[is.closed(iAdult)]
#Mostramos los 6 primeros ordenados por su valor de soporte
inspect(head(sort(icloAdult, by="support")))

#Podemos pintar un gráfico de barras para ver la cantidad de itemsets frecuentes, cerrados
#y maximales que se han generado
barplot( c(frequent=length(iAdult), closed=length(icloAdult), maximal=length(imaxAdult)), ylab="count", xlab="itemsets")

#Usamos apriori para extraer las reglas con mínimo soporte 0.1 y confianza 0.8
#con una longitud minima de 2
rules <- apriori(Adult, parameter = list(support = 0.1, confidence = 0.8, minlen = 2))
#Obtenemos información resumida del conjunto
summary(rules)
#Podemos ver las reglas (lhs es el antecedente y rhs el consecuente de la regla) y sus 
#valores para las medidas soporte, confianza y lift
inspect(head(rules))
#También podemos ver solo los valores de las medidas de calidad 
quality(head(rules))

#Podemos ordenar las reglas por el campo que más nos interese
rulesSorted = sort(rules, by="confidence")
inspect(head(rulesSorted)) 
#Seleccionar un subconjunto de reglas que cumplan una condición. Por ejemplo, seleccionamos 
#las reglas que tenga lift > 1.2 y que en el consecuente de la regla tengan el 
#itemset income=small
rulesRaceWhite <- subset(rules, subset = lhs %in% "race=White" & lift > 1.2) 
inspect(head(rulesRaceWhite))
#Eliminar las reglas redundantes
subsetMatrix <- is.subset(rulesSorted, rulesSorted)
subsetMatrix[lower.tri(subsetMatrix, diag=T)] <- NA
redundant <- colSums(subsetMatrix, na.rm=T) >= 1
rulesPruned <- rulesSorted[!redundant] # remove redundant rules 
inspect(head(rulesPruned))

#También podemos calcular para itemsets o para reglas otras medidas de calidad 
mInteres <- interestMeasure(rulesPruned, measure=c("hyperConfidence", "leverage" ,"phi", "gini"), transactions=Adult)
#Podemos calcular estas medidas para nuestras reglas podadas y añadirselas a la sección 
#quality para que los valores de las medidas nuevas salgan también cuando inspeccionamos 
#las reglas:
quality(rulesPruned) <- cbind(quality(rulesPruned), mInteres)
inspect(head(sort(rulesPruned, by="phi")))

library(arulesViz)
#Utilizar la función plot para representar las reglas en función de las medidas de calidad
plot(rulesPruned)
#Podemos modificar el tipo de gráfico generado cambiando el parámetro método de la función 
#plot. Además, se puede modificar el gráfico cambiando los parámetros del tipo de gráfico
??plot # consultar las distintas opciones para la función plot
plot(rulesPruned[1:6], method="graph", control=list(type="items"))
#Podemos visualizar las reglas como una matriz agrupada. Los antecedentes en las columnas son 
#agrupados usando clustering. En modo interactivo podemos hacer zoom del nodo que queramos 
#estudiar y acceder a las reglas que lo componen para inspeccionarlas.
try: plot(rulesPruned, method="grouped", interactive=TRUE)
plot(rulesPruned[1:6], , method="paracoord", control=list(reorder=TRUE))
#Las podemos guardar en texto plano usando la función write. En este ejemplo las guardamos 
#en un fichero llamado data.csv, usamos como separador “,” y no le ponemos ningún nombre 
#a las columnas
write(rulesPruned, file="reglas.csv", sep = ",", col.names=NA)
#También las podemos guardar en formato PMML
library(pmml)
write.PMML(rulesPruned,file="reglas.pmml")
#Así podemos volver a leerlas
reglasPMML = read.PMML("reglas.pmml")
