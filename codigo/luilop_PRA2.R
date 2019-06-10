
#######################################################################
# PRACTICA 2 - TIPOLOGIA DE LOS DATOS
# Luis Antón López SObrado - luilop
#######################################################################


# Cargamos los paquetes R que vamos a usar
library(ggplot2)
library(dplyr)
library(chron)

##### 1.  Carga de datos

# Cargamos el juego de datos
datos <- read.csv('athletes.csv',stringsAsFactors = FALSE, header = TRUE)


# Nombres de los atributos
colnames(datos) <- c("id","name","nacionality","sex","birthDate","height","weight","sport","gold","silver","bronze")

filas=dim(datos)[1]

# Verificamos la estructura de los datos cargados
str(datos, vec.len = 2, strict.width = "no", width = 30)


##### 2 y 3.  Transformación y limpieza de datos

# A primera vista podemos ver que los atributos id, name, height y weight, no influyen en el analisis, y por tanto, las podemos eliminar.
datos[["id"]]=NULL
datos[["name"]]=NULL
datos[["height"]]=NULL
datos[["weight"]]=NULL


# Inicialmente a partir de la fecha de nacimiento calculamos las edades de cada unos de los cliente. Obtenmos el año de la fecha almacenada, la convertimos a numerico y restamos el año actual.
options(chron.year.expand = 
          function (y, cut.off = 16, century = c(1900, 2000), ...) {
            chron:::year.expand(y, cut.off = cut.off, century = century, ...)
          }
)

datos$fechaAux <- as.Date(chron(format(as.Date(datos$birthDate, "%m/%d/%Y"), "%m/%d/%y")))
datos$edad <- 2016 - (as.numeric(format(datos$fechaAux, format="%Y")))
datos[["fechaAux"]]=NULL
datos[["birthDate"]]=NULL


# Sumamos todas las medallas obtenidas y las alamacenamos en una varibale auxiliar.
datos$medallasAux <- ((as.numeric(datos$gold)) + (as.numeric(datos$silver)) + (as.numeric(datos$bronze)))


# Si observamos la variable medallasAux, podemos crear un nuevo atributo llamado medallas realizando un proceso de discretización sobre la variable medallasAux donde crearemos 2 categorías en las que dividiremos las observaciones.
datos$medallas[datos$medallasAux > 0] <- "SI"
datos$medallas[datos$medallasAux == 0] <- "NO"

# Eliminamos la variables auxiliares
datos[["medallasAux"]]=NULL
datos[["gold"]]=NULL
datos[["silver"]]=NULL
datos[["bronze"]]=NULL

# Verificamos la estructura de los datos cargados
str(datos, vec.len = 2, strict.width = "no", width = 30)

# Analizamos cada una de las variables de tipo character.
table(datos$nacionality)
table(datos$edad)
table(datos$sex)
table(datos$sport)
table(datos$medallas)

#Comprobamos los valores nulos
colSums(is.na(datos))


# eliminamos todos los valores vacíos
datos = na.omit(datos)
colSums(is.na(datos))

# Una vez que terminadas las modificaciones, convertimos las siguientes variables de continuas en categóricas (discretización). Este proceso se realiza para tener una mejor comprensión de cada uno de los elementos, además de por motivos de almacenamiento. 
datos$nacionality = factor(datos$nacionality)
datos$sex =factor(datos$sex)
datos$sport = factor(datos$sport)
datos$medallas = factor(datos$medallas)

# Verificamos la estructura de los datos cargados
str(datos, vec.len = 2, strict.width = "no", width = 30)
summary(datos)

##### 4.1. Análisis - Comprobación de la normalidad y homogeneidad de la varianza

t.test(datos$edad~datos$sex)
var.test(datos$edad~datos$sex)


# Comprobacion homegenidad de la varianza
res <- bartlett.test(edad ~ sex, data = datos)
res


resf <- fligner.test(edad ~ sex, data = datos)
resf

ggplot(data = datos, aes(x = datos$sex, y = datos$edad, colour = datos$sex)) +
  geom_boxplot() +
  geom_point() +
  theme_bw() +
  theme(legend.position = "none")


##### 4.2. Análisis - pruebas estadisticas


mean(datos$edad)
median(datos$edad)
sd(datos$edad)
var(datos$edad)
quantile(datos$edad,c(0.25,0.5,0.75)) 
summary(datos)


##### 5. Representación de los resultados a partir de tablas y gráficas


totalAtletas <- filas
totalAtletas
# Visualización de los datos
#ggplot(data=datos[1:filas,],aes(x=sex,fill=medallas))+geom_bar()
#Pintamos la gráfica resultante
ggplot(datos, aes(x = sex)) + geom_bar(width=0.5)  + theme(text = element_text(size=10),axis.text.x = element_text(angle=60, hjust=1))




# Verificamos la estructura de los datos cargados
summary(datos$edad)
hist(datos$edad)

#Matriz de porcentages de frecuencia entre las variables "workclass" y "hoursWeek":
datosSexoSport<-table(datos$sex,datos$sport)
for (i in 1:dim(datosSexoSport)[1]){
  datosSexoSport[i,]<-datosSexoSport[i,]/sum(datosSexoSport[i,])*100
}
datosSexoSport


# Visualizamos la relación entre las variables 
theme_set(theme_gray(base_size = 10))
ggplot(datos, aes(x=sport,  group=sex)) + geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + geom_text(aes( label = scales::percent(..prop..), y= ..prop.. ), stat= "count", vjust = -.5) + facet_grid(~sex) + scale_y_continuous(labels = scales::percent) + theme(text = element_text(size=10),axis.text.x = element_text(angle=90, hjust=1))


# Verificamos la estructura de los datos cargados
table(datos$sport)

tabla<-table(datos$sex,datos$sport) 
tabla 


reglin<-lm(datos$sport~datos$sex)
reglin
plot(datos$sport,datos$sex)
abline(reglin,col=2)

tabla2<-table(datos$sex,datos$edad) 
tabla2



tabla3<-table(datos$medallas,datos$nacionality) 
tabla3

## Visualizamos la relación entre las variables 
ggplot(data = datos,aes(x=sport,fill=medallas))+geom_bar(position="fill")+facet_wrap(~sex) + theme(text = element_text(size=10),axis.text.x = element_text(angle=90, hjust=1))

