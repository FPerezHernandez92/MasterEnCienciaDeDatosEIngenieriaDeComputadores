##DUDAS
# -En la primera parte hago bien el análisis de las reglas?
# -En la segunda vez debería eliminar reglas? como?
# -En MOPNAR hago bien el análisis de las reglas?

setwd("~/Dropbox/zMaster/zRStudio/Master-en-Ciencia-De-Datos-e-Ingeniería-de-Computadores/2 Mineria de datos, Aprendizaje no supervisado y deteccion de anomalias/Reglas de asociacion")
library(mlbench)
#Cargamos la BD en nuestra zona de trabajo
data(Zoo)
#Consultamos sus dimensiones
dim(Zoo)
#Vemos las 2 primeras filas para ver los atributos y sus tipos
Zoo[1:2,]
summary(Zoo)
#Vemos que variables tenemos, donde tenemos: pelo, plumas, huevos, leche, aereo, acuatico,
#depredador, dentado, vertebrado, respira, venenoso, aletas, piernas, cola, domestico, 
#"catsize", clase(mamifero,pajaro,reptil,pez,amfibio,insecto,molusco)
summary(Zoo$legs)
#Convertimos las variables booleanas a factor
Zoo[["hair"]] = as.factor(Zoo[["hair"]])
Zoo[["feathers"]] = as.factor(Zoo[["feathers"]])
Zoo[["eggs"]] = as.factor(Zoo[["eggs"]])
Zoo[["milk"]] = as.factor(Zoo[["milk"]])
Zoo[["airborne"]] = as.factor(Zoo[["airborne"]])
Zoo[["aquatic"]] = as.factor(Zoo[["aquatic"]])
Zoo[["predator"]] = as.factor(Zoo[["predator"]])
Zoo[["toothed"]] = as.factor(Zoo[["toothed"]])
Zoo[["backbone"]] = as.factor(Zoo[["backbone"]])
Zoo[["breathes"]] = as.factor(Zoo[["breathes"]])
Zoo[["venomous"]] = as.factor(Zoo[["venomous"]])
Zoo[["fins"]] = as.factor(Zoo[["fins"]])
Zoo[["tail"]] = as.factor(Zoo[["tail"]])
Zoo[["domestic"]] = as.factor(Zoo[["domestic"]])
Zoo[["catsize"]] = as.factor(Zoo[["catsize"]])
#De el único atributo continuo lo dividimos en intervalos
Zoo[[ "legs"]] = ordered( cut ( Zoo[[ "legs"]], c(-Inf,0,Inf) ) ,
                          labels = c ("no_legs", "has_legs"))
#Convertimos el data.frame en un conjunto de transacciones con la función as
ZooTrans <- as(Zoo,"transactions")
ZooTrans
#Por lo que tenemos un conjunto de transacciones de 101 transaciones y 39 items

#Vemos el resumen de la BD de transacciones
summary(ZooTrans)

#Representar gráficamente la distribución de los items en las transacciones
image(ZooTrans)

#Para ver gráficamente que items son los más importantes:
#donde el mínimo soporte será 0.4 y reducimos el tamaño de los títulos
itemFrequencyPlot(ZooTrans, support = 0.4, cex.names=0.8)
#Usamos apriori para extraer los itemsets frecuentes con minsop 0.4. 
iZoo <- apriori(ZooTrans, parameter = list(support = 0.4, target="frequent"))
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
barplot( c(frequent=length(iZoo), closed=length(icloZoo), maximal=length(imaxZoo)), 
         ylab="count", xlab="itemsets")

#Usamos apriori para extraer las reglas con mínimo soporte 0.4 y confianza 0.8
#con una longitud minima de 2
rules <- apriori(ZooTrans, parameter = list(support = 0.4, confidence = 0.8, minlen = 2))
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
#itemset feathers=FALSE
rulesNoFeathers <- subset(rules, subset = lhs %in% "feathers=FALSE" & lift > 1.2) 
inspect(head(rulesNoFeathers))
#Eliminar las reglas redundantes
subsetMatrix <- is.subset(rulesSorted, rulesSorted)
subsetMatrix[lower.tri(subsetMatrix, diag=T)] <- NA
redundant <- colSums(subsetMatrix, na.rm=T) >= 1
rulesPruned <- rulesSorted[!redundant] # remove redundant rules 
inspect(head(rulesPruned))

#También podemos calcular para itemsets o para reglas otras medidas de calidad 
mInteres <- interestMeasure(rulesPruned, measure=c("hyperConfidence", 
                    "leverage" ,"phi", "gini"), transactions=ZooTrans)
#Podemos calcular estas medidas para nuestras reglas podadas y añadirselas a la sección 
#quality para que los valores de las medidas nuevas salgan también cuando inspeccionamos 
#las reglas:
quality(rulesPruned) <- cbind(quality(rulesPruned), mInteres)
inspect(head(sort(rulesPruned, by="phi")))

library(arulesViz)
#Utilizar la función plot para representar las reglas en función de las medidas de calidad
plot(rulesPruned)
#Podemos modificar el tipo de gráfico generado cambiando el parámetro método de la función 
#plot
plot(rulesPruned[1:6], method="graph", control=list(type="items"))
#Podemos visualizar las reglas como una matriz agrupada. Los antecedentes en las columnas son 
#agrupados usando clustering. En modo interactivo podemos hacer zoom del nodo que queramos 
#estudiar y acceder a las reglas que lo componen para inspeccionarlas.

#try: plot(rulesPruned, method="grouped", interactive=TRUE)

#Vamos a analizar algunas reglas por soporte
inspect((sort(rulesPruned, by="support"))[1:20])
#Como si no es domestico no es venenoso, vamos a ver que animales son domesticos y venenosos
vector.es.domestico.y.venenoso <- (Zoo[["domestic"]]==TRUE & Zoo[["venomous"]]==TRUE)
valores.es.domestico.y.venenoso <- (Zoo[vector.es.domestico.y.venenoso,])
valores.es.domestico.y.venenoso
#Por lo que podemos ver que la abeja es domestica y venenosa
#Ahora vamos a ver la regla de que si respira, no tiene aletas
vector.respira.y.con.aletas <- (Zoo[["breathes"]]==TRUE & Zoo[["fins"]]==TRUE)
valores.respira.y.con.aletas <- (Zoo[vector.respira.y.con.aletas,])
valores.respira.y.con.aletas
#Podemos ver que los animales que respiran y que tienen aletas son los delfines,
#una marsopa, la foca y el león marino

#Vamos a analizar algunas reglas por confianza
inspect((sort(rulesPruned, by="confidence"))[1:20])
#Si no es depredador y respira, no tiene aletas
vector.no.depredador.respira.sin.aletas <- (Zoo[["predator"]]==FALSE & Zoo[["breathes"]]==TRUE & Zoo[["fins"]]==FALSE)
valores.no.depredador.respira.sin.aletas <- (Zoo[vector.no.depredador.respira.sin.aletas,])
valores.no.depredador.respira.sin.aletas


##############################
######## ITEMS NEGADOS #######
##############################
summary(Zoo)
#La única variable que podría negar en esta base de datos podría ser type
es.mammal <- (Zoo[["type"]]=="mammal")
es.bird <- (Zoo[["type"]]=="bird")
es.reptile <- (Zoo[["type"]]=="reptile")
es.fish <- (Zoo[["type"]]=="fish")
es.amphibian <- (Zoo[["type"]]=="amphibian")
es.insect <- (Zoo[["type"]]=="insect")
es.mollusch.et.al <- (Zoo[["type"]]=="mollusc.et.al")
Zoo$es.mammal <- as.factor(es.mammal)
Zoo$es.bird <- as.factor(es.bird)
Zoo$es.reptile <- as.factor(es.reptile)
Zoo$es.fish <- as.factor(es.fish)
Zoo$es.amphibian <- as.factor(es.amphibian)
Zoo$es.insect <- as.factor(es.insect)
Zoo$es.mollusch.et.al <- as.factor(es.mollusch.et.al)
#Elimino la antigua variable type
Zoo$type=NULL

#Volvemos a hacer el análisis que se hizo anteriormente
#Convertimos el data.frame en un conjunto de transacciones con la función as
ZooTrans <- as(Zoo,"transactions")
ZooTrans
#Por lo que tenemos un conjunto de transacciones de 101 transaciones y 46 items
#donde antes teniamos 39 items

#Vemos el resumen de la BD de transacciones
summary(ZooTrans)

#Representar gráficamente la distribución de los items en las transacciones
image(ZooTrans)

#Para ver gráficamente que items son los más importantes:
#donde el mínimo soporte será 0.6 y reducimos el tamaño de los títulos
itemFrequencyPlot(ZooTrans, support = 0.6, cex.names=0.8)
#Usamos apriori para extraer los itemsets frecuentes con minsop 0.6. 
iZoo <- apriori(ZooTrans, parameter = list(support = 0.6, target="frequent"))
#He aumentado el soporte al tener un mayor número de reglas
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
barplot( c(frequent=length(iZoo), closed=length(icloZoo), maximal=length(imaxZoo)), 
         ylab="count", xlab="itemsets")

#Usamos apriori para extraer las reglas con mínimo soporte 0.6 y confianza 0.8
#con una longitud minima de 2
rules <- apriori(ZooTrans, parameter = list(support = 0.6, confidence = 0.8, minlen = 2))
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
#itemset feathers=FALSE
rulesNoFeathers <- subset(rules, subset = lhs %in% "feathers=FALSE" & lift > 1.2) 
inspect(head(rulesNoFeathers))
#Eliminar las reglas redundantes
subsetMatrix <- is.subset(rulesSorted, rulesSorted)
subsetMatrix[lower.tri(subsetMatrix, diag=T)] <- NA
redundant <- colSums(subsetMatrix, na.rm=T) >= 1
rulesPruned <- rulesSorted[!redundant] # remove redundant rules 
inspect(head(rulesPruned))

#También podemos calcular para itemsets o para reglas otras medidas de calidad 
mInteres <- interestMeasure(rulesPruned, measure=c("hyperConfidence", 
                                                   "leverage" ,"phi", "gini"), transactions=ZooTrans)
#Podemos calcular estas medidas para nuestras reglas podadas y añadirselas a la sección 
#quality para que los valores de las medidas nuevas salgan también cuando inspeccionamos 
#las reglas:
quality(rulesPruned) <- cbind(quality(rulesPruned), mInteres)
inspect(head(sort(rulesPruned, by="phi")))

library(arulesViz)
#Utilizar la función plot para representar las reglas en función de las medidas de calidad
plot(rulesPruned)
#Podemos modificar el tipo de gráfico generado cambiando el parámetro método de la función 
#plot
plot(rulesPruned[1:6], method="graph", control=list(type="items"))
#Podemos visualizar las reglas como una matriz agrupada. Los antecedentes en las columnas son 
#agrupados usando clustering. En modo interactivo podemos hacer zoom del nodo que queramos 
#estudiar y acceder a las reglas que lo componen para inspeccionarlas.

#try: plot(rulesPruned, method="grouped", interactive=TRUE)

#Vamos a analizar algunas reglas
inspect((sort(rulesPruned, by="support"))[1:20])
##ELIMINAMOS LA PRIMERA REGLA POR EJEMPLO??



###############################
####### PAQUETE RKEEL #########
###############################
library(RKEEL)
#Creamos MOPNAR para el dataset Zoo
data(Zoo)
mopnar_zoo <- MOPNAR_A(Zoo)
#Ejecutamos MOPNAR
mopnar_zoo$run()
#Ordenamos la salida por confianza
mopnar_zoo$sortBy("confidence")
#Mostramos las reglas
rulesZoo <- mopnar_zoo$showRules()

rulesZoo[1:10,]
#Si no es pájaro, no tiene plumas, por lo que vamos a ver los pájaros sin plumas
vector.pajaro.sin.plumas <- (Zoo[["type"]]=="bird" & Zoo[["feathers"]]=="FALSE")
valores.pajaro.sin.plumas <- (Zoo[vector.pajaro.sin.plumas,])
valores.pajaro.sin.plumas
#Por lo que podemos ver que no hay ninguno
