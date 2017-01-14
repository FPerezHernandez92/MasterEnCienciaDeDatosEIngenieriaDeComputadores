
# se indica que se requiere el paquete Hmisc. En caso de no
# estar disponible habra que instalarlo
require(Hmisc)

# igual con el paquete fBasics
require(fBasics)

# se carga la definicion de funciones de lectura de datos
# (aunque si se ha trabajado antes con el archivo su definicion
# ya deberia estar en el entorno)
source("lecturaDatos.R")

# se comienza leyendo el conjunto de datos en forma csv
datos <- lecturaDatos("./data/","datos.csv")

# se determina la dimension del conjunto de datos: numero de
# filas y de columnas: de instancias y de variables
instancias <- nrow(datos)
variables <- ncol(datos) 

# se muestra la cabecera del conjunto de datos
head(datos)

# se muestra la lista de nombres de variables del conjunto de datos
names(datos)

# muestra informacion de resumen sobre el conjunto de datos, sobre
# todas sus variables
summary(datos)

# otra funcion interesante es str, que muestra informacion sobre las variables
# de una forma mas compacta, y permite determinar si una variable es discreta,
# continua de forma sencilla
str(datos)

# tambien es posible visualizar los datos en forma de tabla (e incluso editarlos)
fix(datos)

# se muestra informacion sobre la primera variable. Observad que
# la funcion entiende que el indice no se refiere a una fila sino
# a una columna
describe(datos[1])

# se muestra informacion apra la variable class (categorica)
describe(datos[51])

# informacion detallada sobre la distribucion de valores de
# la variable separation
basicStats(datos[1])
