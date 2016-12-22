#FRANCISCO PÉREZ HERNÁNDEZ 20076629K

####### INTRODUCCIÓN A R ######
# EJERCICIO 1: R Interactivo
#1.1 Crea una secuencia de números impares
e11a = seq(1,30, by =2)

#1.2 Crea números del 1 al 30
e12a = c(1:30)

#1.3 Busca en la ayuda que hace la función seq(). Describe que hace. Utilízala para crear números del 1 al 30 con un 
#incremento de 0.5. ¿Qué otros parámetros te ofrece la función seq()? Utilízalos en un ejemplo.
#Para saber que hace seq:
help("seq")
#Genera secuencias regulares
e13a = seq(1,30, by = 0.5)
#Sus parametros son, from,to,by,length.out,along.with,...
e13b= seq(1,30,length.out=3)
e13c =seq_along(along.with = c(323,2))

#1.4 Crea una secuencia de números indicando el principio y la longitud de la secuencia de números
e14a = seq(1, length.out = 4)

#1.5 Crea letras minúsculas, mayúsculas, nombre de los meses del año y nombre de los meses del año abreviado
e15a = letters
e15b = LETTERS
e15c = month.name
e15d = month.abb

#1.6 Investiga la función rep(). Repite un vector del 1 al 8 cinco veces.
help(rep)
e16a = rep(c(1:8),5)

#1.7 Haz lo mismo con las primeras ocho letras del abecedario en mayúsculas
e17a = rep(LETTERS[1:8],5)


# EJERCICIO 2: Vectores
#2.1 Crea los siguientes vectores:
#2.1a un vector del 1 al 20:
e21a = c(1:20)
#2.1b un vector del 20 al 1:
e21b = c(20:1)
#2.1c Utilizando el comando c() crea un vector que tenga el siguiente patrón 1,2,3,4,5…20,19,18,17….1
e21c = c(e21a,e21b[2:length(e21b)])

#2.2 Genera una secuencia de números del 1 al 30 utilizando el operador : y asígnalo al vector x. El vector resultante 
#x tiene 30 elementos. Recuerda que el operador ‘:’ tiene prioridad sobre otros operadores aritméticos en una expresión. 
e22 = c(1:30)

#2.3 Genera un vector x que contenga números del 1 al 9. Utilizando el operador ‘:’ .  y utilizando otras opciones.  
#PISTA: seq()
e23a = c(1:9)
e23b = seq(1:9)

#2.4 Genera un vector x que contenga 9 números comprendidos entre 1 y 5
e24 = seq(1,5,length.out = 9)

#2.5 Busca que hace la función sequence(). ¿Cual es la diferencia con la función seq()
help(sequence)
help(seq)
sequence(c(3,2,4))
seq(c(3,2,4))
#Con sequence se crean secuencias hasta los valores indicados en el vector mientras que con seq se crearia 
#un vector con tantos elementos como haya en un vector

#2.6 Crea un vector numérico utilizando la función c()
e26 = as.numeric(c(2:45))

#2.7 Accede al segundo elemento del vector
e26[2]

#2.8 Crea un vector numérico “z” que contenga del 1 al 10. Cambia el modo del vector a carácter. 
e28a = as.numeric(c(1:10))
e28b = as.character(e28a)

#2.9 Ahora cambia el vector z a numérico de nuevo
e29 = as.numeric(e28b)

#2.10 Busca en la ayuda que hace la función scan(). Utilízala para leer un fichero cualquiera y asigna la lectura a 
#un vector “z”.
#La función scan() permite leer datos y cargarlos en R.
help(scan)
write(c(1:10), file="data")
e210 = scan(file = "data")

#2.11 Crea un vector x con 100 elementos, selecciona de ese vector una muestra al azar de tamaño 5. Busca que hace la 
#función sample().
e211a = c(1:100)
e211b = sample(e211a,5)
help(sample)

#2.12 Genera un vector de tipo con 100 números entre el 1 y el 4 de forma random. Para ello mira en la ayuda la 
#función runif(). Obliga a que el vector resultante se ade tipo integer. Ordena el vector por tamaño usando la 
#función sort(). ¿Qué devuelve la función sort?. SI quisieras invertir el orden de los elementos del vector que 
#función utilizarías. Utiliza la función order() sobre x. ¿Cuál es la diferencia con la función sort()?
help(runif)
e212a = as.integer(runif(100,1,4))
e212b = sort(e212a)
help(sort)
#sort devuelve un vector ordeando ascendentemente o descendentemente.
e212c = sort(e212b, decreasing = TRUE)
e212d = order(e212c)
help(order)
#order devuelve los índices ordenados del vector

#2.13 Crea un vector x que contenga los números ordenados del 1 al 10 de forma consecutiva. Investiga la función 
#rep(). Una vez comprobado que funciona, elimina las entradas repetidas del vector, para ello consulta la función 
#unique().
e213a = c(1:10)
help(rep)
e213b = rep(e213a,3)
help(unique)
e213c = unique(e213b)

#2.14 Crea un vector cualquiera. Devuelve de ese vector una muestra cinco números seleccionada al azar. Usa la 
#función sample(), prueba a determinar los valores que quieres extraer con y sin remplazo.
e214a = c(1:10)
e214b = sample(e214a,5)
e214c = sample(e214a,5,replace = TRUE)

#2.15 Comprueba que objetos tienes ahora en tu espacio de trabajo. Prueba con la función ls()
ls()


# EJERCICIO 3: Explora el indexado de Vectores

#3.1 Ejecuta los siguientes comandos y comprueba su resultado 
x <- 1:10
names(x) <- letters[x]
x[1:3]
x[c(1,10)]
x[c(-1,-2)]
x[ x > 5]
x[c("a","d")]
x[]
x <- 1:10; y <- c(x[1:5],99,x[6:10]); y

#3.2 Crea un vector con números del 1 al 100 y extrae los valores del 2 al 23.
e32a = c(1:100)
e32b = e32a[2:23]

#3.3 Del mismo vector x extrae ahora todos los valores menos del 2:23
e33 = e32a[-2:-23]

#3.4 Cambia el número en la posición 5 por el valor 99 
e34 = e33
e34[5]= 99

#3.5 Crea un vector lógico del vector letters, (e.g. comprobando si existe c en el vector letters).
e35 = (letters=="c")

#3.6 ¿Qué devuelve el siguiente comando? which(rep(letters,2)=="c") 
which(rep(letters,2)=="c")
#La posición donde está la letra c

#3.7 ¿Qué devuelve el siguiente comando? match(c("c","g"), letters) 
match(c("c","g"),letters)
#la posicón donde están las letras

#3.8 Crea un vector x de elementos -5 -1, 0, 1, . . . , 6. Escribe un código en R del tipo x[ 'something' ], para 
#extraer
e38 = c(-5,-1:6)
#3.8a elementos de x menores que  0,
e38a = e38[e38<0]
#3.8b elementos de x menores o igual que 0,
e38b = e38[e38<=0]
#3.8c elementos of x mayor o igual que 3,
e38c = e38[e38>=3]
#3.8d elementos de x menor que 0 o mayor que  4 
e38d = e38[e38<0 | e38>4]
#3.8e elementos de x mayor que 0 y menor que 4
e38e = e38[e38>0 & e38<4]
#3.8f elementos de x distintos de 0
e38f = e38[e38!=0]

#3.9 El código is.na se usa para identificar valores ausentes (NA). Crea el vector x<- c(1,2,NA) y averigua que pasa 
#cuando escribes is.na(x). Prueba con x[ x!=NA ] ¿obtienes con este comando los missing values de x?. ¿cuál es tu 
#explicación?
x<-c(1,2,NA)
is.na(x)
x[x!=NA]
#Solo se puede sacar el NA con is.na

#3.10 Determine what objects are in the current workspace(objects())
objects()


# EJERCICIO 4: Búsqueda de valores idénticos y distintos en Vectores
#4.1 Haz la intersección de dos vectores month.name[1:4] y  month.name[3:7] usando la función intersect().
e41a = month.name[1:4]
e41b = month.name[3:7]
e41c = intersect(e41a,e41b)

#4.2 Recupera los valores idénticos entre dos vectores usando %in%. Esta función devuelve un vector lógico de los 
#elementos idénticos. Utiliza esa función para poder extraer ese subset del vector original.
e42 = e41a%in%e41b

#4.3 Si x=month.name[1:4] e y= month.name[3:7] recupera los valores únicos en el primer vector. Para ello investiga 
#la función diff().¿Puedes usarlo con caracteres?. Busca una alternativa.
x=month.name[1:4]
y= month.name[3:7]
help(diff)
setdiff(x,y)
#Al no poder usarlo con carácteres he usado setdiff como alternativa.

#4.4 Une dos vectores sin duplicar las entradas repetidas. Investiga la función unión().
help(union)
e44 = union(e41a,e41b)

#4.5 Recupera las entradas duplicadas de x
e45 = intersect(e41a,e41b)

