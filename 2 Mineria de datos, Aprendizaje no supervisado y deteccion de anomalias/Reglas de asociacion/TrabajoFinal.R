#Francisco Pérez Hernández
setwd("~/Dropbox/zMaster/zRStudio/Master-en-Ciencia-De-Datos-e-Ingeniería-de-Computadores/2 Mineria de datos, Aprendizaje no supervisado y deteccion de anomalias/Reglas de asociacion")
library(arules)
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
copRulesPruned <- rulesPruned
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

####################################
######## ANÁLISIS DE REGLAS ########
####################################

#Vamos a analizar algunas reglas por soporte
inspect((sort(copRulesPruned, by="support"))[1:10])
#Vemos como esta regla, la 1, tiene un lift menor que 1, por lo que no se espera que sea de utilidad
#Veamos la confianza del consecuente, no es venenoso
inspect(iZoo[size(iZoo)==1])
#Por lo que vamos a compararlas:
inspect((sort(copRulesPruned, by="support"))[1])
inspect(iZoo[size(iZoo)==1][1])
#Vemos como la confianza de la regla y el soporte del consecuente son iguales, y al tener el lift
#menor que 1, tenemos una regla que no nos aporta nada. Por lo que ya tenemos un criterio para
#descartar reglas de utilidad.

#Seguimos analizando, ordenando por lift
inspect((sort(copRulesPruned, by="lift"))[1:10])
#La regla 9 puede ser interesante, analicemos su consecuente
inspect(iZoo[size(iZoo)==1])
#Las comparamos:
inspect((sort(copRulesPruned, by="lift"))[9])
inspect(iZoo[size(iZoo)==1][12])
#Tenemos un lift alto, un soporte en el consecuente del 0.59, y pasamos a una confianza de la regla del 0.98, por lo que 
#esta regla sería interesante. por lo que si pone huevos, no pone leche
#Veamos que animales podrían ser estos:
vector.pone.huevos.no.da.leche <- (Zoo[["eggs"]]==TRUE & Zoo[["milk"]]==FALSE)
valores.pone.huevos.no.da.leche <- (Zoo[vector.pone.huevos.no.da.leche,])
dim(valores.pone.huevos.no.da.leche)
head(valores.pone.huevos.no.da.leche)
#Donde tenemos 58 animales que cumplen estas condiciones, como puede ser el pollo

#Seguimos analizando, ordenando por confidence
inspect((sort(copRulesPruned, by="confidence"))[1:10])
#Las 5 primeras reglas pueden ser interesantes, analicemos su consecuente
inspect(iZoo[size(iZoo)==1])
#Las comparamos:
inspect((sort(copRulesPruned, by="lift"))[1:5])
inspect(iZoo[size(iZoo)==1][c(22,14,13,12)])
#Tenemos un lift alto, un soporte en el consecuente de entre el 0.40 y 0.59, y pasamos a una confianza en la regla del 0.97 y 1, por lo que 
#estas reglas serán interesantes. 



##############################################
######## ANÁLISIS DE REGLAS EN GRUPOS ########
##############################################
data(Zoo)
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
                          labels = c ("FALSE", "TRUE"))
#Convertimos el data.frame en un conjunto de transacciones con la función as
ZooTrans <- as(Zoo,"transactions")

#Lo primero, vamos a realizar de nuevo apriori, pero cambiando los parámetros:
iZoo <- apriori(ZooTrans, parameter = list(support = 0.1, target="frequent", maxlen=2))
rules <- apriori(ZooTrans, parameter = list(support = 0.1, confidence = 0.1, minlen = 2, maxlen=3))
#De forma que queremos reglas que tengan longitud de 2 o 3, con un soporte del 0.4 y confianza del 0.6
#Ordenamos las reglas por lift
sortRules <- (sort(rules,by ="lift"))
#Eliminamos las reglas redundantes 
subsetMatrix <- is.subset(sortRules, sortRules)
subsetMatrix[lower.tri(subsetMatrix, diag=T)] <- NA
redundant <- colSums(subsetMatrix, na.rm=T) >= 1
rulesPruned <- sortRules[!redundant] # remove redundant rules 
#Vemos las primeras reglas ya "arregladas", con tamaño 2 para comenzar el análisis
inspect(head(rulesPruned[size(rulesPruned)==2]))
#De esta forma voy a analizar la regla 1:
consecuentePositivo <- subset(rulesPruned,subset = rhs %in% "fins=TRUE")
inspect(consecuentePositivo[size(consecuentePositivo)==2])
inspect(iZoo[size(iZoo)==1][3])
#Voy a buscar reglas que tenga el consecuente de esta regla negado:
consecuenteNegado <- subset(rulesPruned,subset = rhs %in% "fins=FALSE" )
inspect(consecuenteNegado)
inspect(consecuentePositivo[size(consecuentePositivo)==2])
consecuenteNegadoYRegla1 <- subset(consecuenteNegado, subset = lhs %in% "type=fish")
inspect(consecuenteNegadoYRegla1)
consecuenteNegadoYRegla2 <- subset(consecuenteNegado, subset = lhs %in% "breathes=FALSE")
inspect(consecuenteNegadoYRegla2)
consecuenteNegadoYRegla3 <- subset(consecuenteNegado, subset = lhs %in% "hair=FALSE")
inspect(consecuenteNegadoYRegla3[size(consecuenteNegadoYRegla3)==3])
inspect(iZoo[size(iZoo)==1][32])

###FUNCIÓN CREADA
MiMetodo = function(fsoporte,fconfianza,fminlen,fmaxlen,limitelif,limiteconfianza,limitesoporte,tamasize){
  #Inicializamos la variable de salida
  misrules=NULL
  #Realizamos a priori
  rules <- apriori(ZooTrans, parameter = list(support = fsoporte, confidence = fconfianza, minlen = fminlen, maxlen=fmaxlen))
  #Si queremos eliminar reglas por los valores de lift, confianza o soporte se podan
  rulesLimiteLift <- subset(rules, lift > limitelif)
  rulesLimiteConfianza <- subset(rulesLimiteLift, confidence <= limiteconfianza)
  rulesLimiteSoporte <- subset(rulesLimiteConfianza, support < limitesoporte)
  #Eliminamos las reglas redundantes
  sortRules <- sort(rulesLimiteSoporte, by = "lift")
  subsetMatrix <- is.subset(sortRules, sortRules)
  subsetMatrix[lower.tri(subsetMatrix, diag=T)] <- NA
  redundant <- colSums(subsetMatrix, na.rm=T) >= 1
  rulesPruned <- sortRules[!redundant]
  #Realizamos el inspect en función del tamaño deseado
  salidaInspect <- inspect((rulesPruned[size(rulesPruned)==tamasize]))
  numeroReglas <- dim(salidaInspect)[1]
  #Divido la salida del inspect en reglas con type en el consecuente y reglas sin type:
  #Nota: las reglas con type, no las he analizado, realizaría otra función
  salidaInspectType=NULL;salidaInspectNoType=NULL
  for (i in 1:(dim(salidaInspect)[1])){
    Cconsecuente <- salidaInspect[i,3];Cconsecuente <- as.character(Cconsecuente);tamachar <- nchar(Cconsecuente) -1;
    Cconsecuente <- substr(Cconsecuente,start=2,stop=5)
    if (Cconsecuente=="type"){
      salidaInspectType = rbind(salidaInspectType,salidaInspect[i,])
    } else {
      salidaInspectNoType = rbind(salidaInspectNoType,salidaInspect[i,])
    }
  }
  #Me quedo con las reglas que no tienen type en el consecuente
  salidaInspect <- salidaInspectNoType
  tamaRecortado <- dim(salidaInspect)[1]
  for (h in 1:tamaRecortado){
    #Sacar consecuente
    Cconsecuente <- salidaInspect[h,3];Cconsecuente <- as.character(Cconsecuente);tamachar <- nchar(Cconsecuente) -1;
    Cconsecuente <- substr(Cconsecuente,start=2,stop=tamachar)
    consecuentePositivo <- subset(rulesPruned, subset = rhs %in% Cconsecuente)
    tamaconse <- nchar(Cconsecuente)
    #Niego el consecuente
    CconsecuenteNegado=NULL
    if ((substr(Cconsecuente,start=(tamaconse-4),stop = tamaconse))==FALSE){
      CconsecuenteNegado <- substr(Cconsecuente,start=1,stop=(tamaconse-5))
      CconsecuenteNegado <- paste(CconsecuenteNegado,"TRUE",sep = "")
    } else if ((substr(Cconsecuente,start=(tamaconse-3),stop = tamaconse))==TRUE){
      CconsecuenteNegado <- substr(Cconsecuente,start=1,stop=(tamaconse-4))
      CconsecuenteNegado <- paste(CconsecuenteNegado,"FALSE",sep = "")
    }
    consecuenteNegado <- subset(rulesPruned,subset = rhs %in% CconsecuenteNegado )
    print("Número de reglas con consecuente negado");length(consecuenteNegado)
    inspect(consecuenteNegado)
    inspectPositivas <- inspect(consecuentePositivo[size(consecuentePositivo)==2])
    
    #Busco reglas con el consecuente negado y que tengan el antecedente de las reglas con consecuente positivo
    for ( j in 1:((dim(inspectPositivas)[1]))){
      misrulesNuevas=NULL
      Cantecedente <- inspectPositivas[j,1];Cantecedente <- as.character(Cantecedente);tamachar <- nchar(Cantecedente) -1;
      Cantecedente <- substr(Cantecedente,start=2,stop=tamachar)
      consecuenteNegadoYRegla <- subset(consecuenteNegado, subset = lhs %in% Cantecedente)
      misrulesNuevas = inspect(consecuenteNegadoYRegla[size(consecuenteNegadoYRegla)>2])
      misrulesNuevas = rbind(misrulesNuevas,misrulesNuevas)
      if(is.null(misrulesNuevas)){
        
      } else if ((dim(misrulesNuevas)[1])>0){
        misrules = rbind(misrules,inspectPositivas[j,])
        misrules = rbind(misrules,misrulesNuevas)
      }
    }
    if (is.null(misrules)){
      
    } else {
      #Elimino las reglas con lift menor de 1.2
      for (g in (dim(misrules)[1]):1){
        if (misrules[g,6]<1.2){
          misrules = misrules[-g,]
        }
      }
    }
  }
  return(misrules)
}

#Doy valores a los parámetros y ejecuto: NOTA: LA EJECUCIÓN PUEDE TARDAR VARIOS MINÚTOS
fsoporte = 0.1;fconfianza = 0.1;fminlen = 2;fmaxlen = 3;limitelif = 0;limiteconfianza=1;limitesoporte=1;tamasize=2
rules1 <- MiMetodo(fsoporte,fconfianza,fminlen,fmaxlen,limitelif,limiteconfianza,limitesoporte,tamasize)
inspect(iZoo[size(iZoo)==1][c(16,21)])
rules1[125:127,]
inspect(iZoo[size(iZoo)==1][c(13,23)])
rules1[1726:1727,]



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

rulesZoo[1:9,c(1,2,3,5,7,8,9)]
#Donde podemos ver reglas que estas 9 primeras reglas, pasan de tener un soporte
#en el consecuente de entre 0.17 a 0.88 a tener confianza 1, por lo que serán
#buenas reglas, aunque habrá algunas triviales. 

