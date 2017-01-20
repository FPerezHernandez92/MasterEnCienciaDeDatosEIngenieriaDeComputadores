library(mlbench)
#Cargamos la BD en nuestra zona de trabajo
data(Zoo)
#Consultamos sus dimensiones
dim(Zoo)
#Vemos las 2 primeras filas para ver los atributos y sus tipos
Zoo[1:2,]
summary(Zoo)
summary(Zoo$legs)
Zoo[["feathers"]] = as.factor(Zoo[["feathers"]])
#De el único atributo continuo lo dividimos en intervalos
Zoo[[ "legs"]] = ordered( cut ( Zoo[[ "legs"]], c(-Inf,0,Inf) ) ,labels = c ("no_legs", "has_legs"))
#Convertimos el data.frame en un conjunto de transacciones con la función as
ZooTrans <- as(Zoo,"transactions")
ZooTrans

#Vemos el resumen de la BD
summary(ZooTrans)

#Representar gráficamente la distribución de los items en las transacciones (101 transacciones y 24 items)
image(ZooTrans)

#Para ver gráficamente que items son los más importantes:
#donde el mínimo soporte será 0.1 y reducimos el tamaño de los títulos
itemFrequencyPlot(ZooTrans, support = 0.1, cex.names=0.8)
#Usamos apriori para extraer los itemsets frecuentes con minsop 0.1. 
iZoo <- apriori(ZooTrans, parameter = list(support = 0.1, target="frequent"))
#Orenamos por el valor de soporte
iZoo <- sort(iZoo, by="support") 
#Inspeccionamos los 10 primeros
inspect(head(iZoo, n=10))
#Podemos consultar el tamaño de los itemsets frecuentes
size(iZoo)
#Representamos con un diagrama de barras
barplot(table(size(iZoo)), xlab="itemset size", ylab="count")
#Inspeccionamos los itemsets frecuentes de tamaño 1
inspect(iZoo[size(iZoo)==1])

#Sacamos un vector lógico indicando que itemsets es máximal
imaxZoo <- iZoo[is.maximal(iZoo)]
#Mostramos los 6 primeros ordenados por su valor de soporte
inspect(head(sort(imaxZoo, by="support")))
#Sacamos un vector lógico indicando que itemsets es cerrado
icloZoo <- iZoo[is.closed(iZoo)]
#Mostramos los 6 primeros ordenados por su valor de soporte
inspect(head(sort(icloZoo, by="support")))

#Podemos pintar un gráfico de barras para ver la cantidad de itemsets frecuentes, cerrados
#y maximales que se han generado
barplot( c(frequent=length(iZoo), closed=length(icloZoo), maximal=length(imaxZoo)), ylab="count", xlab="itemsets")

#Usamos apriori para extraer las reglas con mínimo soporte 0.1 y confianza 0.8
#con una longitud minima de 2
rules <- apriori(ZooTrans, parameter = list(support = 0.1, confidence = 0.8, minlen = 2))
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
Zoo$feathers
rulesNoFeathers <- subset(rules, subset = lhs %in% "feathers=FALSE" & lift > 1.2) 
inspect(head(rulesNoFeathers))
#Eliminar las reglas redundantes
subsetMatrix <- is.subset(rulesSorted, rulesSorted)
subsetMatrix[lower.tri(subsetMatrix, diag=T)] <- NA
redundant <- colSums(subsetMatrix, na.rm=T) >= 1
rulesPruned <- rulesSorted[!redundant] # remove redundant rules 
inspect(head(rulesPruned))

#También podemos calcular para itemsets o para reglas otras medidas de calidad 
mInteres <- interestMeasure(rulesPruned, measure=c("hyperConfidence", "leverage" ,"phi", "gini"), transactions=ZooTrans)
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
plot(rulesPruned[1:2], , method="paracoord", control=list(reorder=TRUE))
#Las podemos guardar en texto plano usando la función write. En este ejemplo las guardamos 
#en un fichero llamado data.csv, usamos como separador “,” y no le ponemos ningún nombre 
#a las columnas
write(rulesPruned, file="reglas.csv", sep = ",", col.names=NA)
#También las podemos guardar en formato PMML
library(pmml)
write.PMML(rulesPruned,file="reglas.pmml")
#Así podemos volver a leerlas
reglasPMML = read.PMML("reglas.pmml")
