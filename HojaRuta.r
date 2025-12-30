#####################################
#9no Coloquio Uruguayo de Matemática#
#####################################

#Introducción a R
#Mathias Bourel

#Este script es para ejecutar directamente las lineas 
#de código de las transparencias del material Introducción a R
#del curso de Modelización Estadística

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# lista de todos los paquetes disponibles
library()
# cargar paquetes 
library(MASS) #para hacer análisis discriminante, regresiones, multidimensional
library(rpart) #para hacer arboles de clasificación y regresión
library(e1071) #para Support Vector Machines
library(ks) #estimación de densidad por núcleo
# documentación de paquetes
library(help = rpart)
# descarga e instalación de paquetes desde CRAN 
install.packages (...)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#########
#Importación y exportación de bases de datos#
#############################################
help(mvrnorm)
?lm

base=read.csv("Desgargas/spam.csv",sep=",",header=T)

miejemplo <- data.frame(cbind(matrix(round(runif(24)*100,0),6,4)),CAT=rep(c('A','B'),3))
attr(miejemplo,'names') <- paste('Var',1:5,sep='')
row.names(miejemplo) <- c('I1','I2','I3','I4','I5','I6')
miejemplo <- cbind(miejemplo,suma13=colSums(miejemplo[,1:3]),logica2=miejemplo[,2]>50)
ej2 <- miejemplo[-1,-ncol(miejemplo)]
summary(ej2)
write.table(miejemplo,file='miejemplo.txt',sep='\t')


################################################
#Funcion de densidad, de distribución,cuantil,##
#y generacion de datos normales                #
################################################

x=seq(-5,5,.01)
u=seq(0,1,.01)

par(mfrow=c(2,2))
plot(x,dnorm(x,0,1),type='l', col='blue',main='densidad gaussiana')
plot(x,pnorm(x,0,1),type='l', col='red',main='distribución gaussiana')
plot(u,qnorm(u,0,1),type='l', col='green',main='función de cuantiles gaussiana')
plot(rnorm(n=50,0,1), main='muestra aleatoria gaussiana')


############################################################
#Simulación de una distribución discreta con soporte finito#
############################################################
muestra=sample(x=c(1:6),size=100,replace=T)
plot(table(muestra))
barplot(table(muestra))
summary(muestra)
cumsum(table(muestra))
cumsum(proportions(table(muestra)))


######################
#Escribir una función#
######################
cuentas = function(x, y){
  a = x+2*y
  b=x-y
  c=x/y
  final=c(mean(a),sd(a),mean(b),sd(b),mean(c),sd(c))
  salida=list(x=x,y=y,resultados=result)
  return(salida)}
x <- runif(50,0,1)
y <- rnorm(50,0,3)
cuentas(x,y)


############################
#Serie estadística continua#
############################
x=sqrt(islands)
summary(x)
par(mfrow=c(1,3))
barplot(x,main='Diagrama en barra')
pie(x,main='Reparticion')
plot(density(x))


############
#Histograma#
############
par(mfrow=c(1,4))
hist(x)
hist(x,breaks=10)
hist(x,breaks=20)
hist(x,breaks=c(4 *0:5,30, 45, 50 ,70 , 100,140))
hist(x,breaks='Scott')

########################
#Estimacion de densidad#
########################

par(mfrow=c(1,2))
x=rnorm (5000)
y=density(x)
plot(y)
hist(x,proba=T)
lines(y,col='blue')
z=seq(min(x),max(x),0.01)
lines(z, dnorm(z,0,1), lwd=2,lty =3,col='red')

##########################################
#Estimacion kde de una mezcla de normales#
##########################################
w=rbinom(50,1,1/4)
datos = w*rnorm(50,5) +(1-w) * rnorm(50,-5)
plot(density(datos),
     main = "Densidades superpuestas",
     xlab = "x",
     ylab = "Densidad")
lines(density(datos, kernel = "triangular"),
      col = "red")
lines(density(datos, kernel = "rectangular"),
      col = "blue")


legend("topright",
       legend = c("Kernel = gaussian (default)","Kernel = triangular","Kernel = rectangular"),
       col = c("black", "red","blue"),
       lty = 1,
       bty = "n")

par(mfrow=c(2,2))

for(i in c(0.1,1,2,3)){plot(density(datos,bw=i),
                            main = paste("bandwith=",i),
                            xlab = "x",
                            ylab = "Densidad")
  
}


################################################
# Normal bivariada + estimación de densidad(2D)#
################################################
library(MASS)
set.seed(123)
# 1. Parámetros
n <- 300
mu <- c(0, 0)
Sigma <- matrix(c(1, 0.8,
                  0.8, 2),
                nrow = 2, byrow = TRUE)
# 2. Generar datos
datos <- mvrnorm(n = n, mu = mu, Sigma = Sigma)
x <- datos[,1]
y <- datos[,2]

# 3. Gráfico de los datos
plot(x, y,
     pch = 16, cex = 0.5,
     xlab = "X", ylab = "Y",
     main = "Datos normales bivariados")

# 4. Estimación de densidad kernel normal (2D)
dens <- kde2d(x, y, n = 100)

# 5. Contornos de la densidad + puntos
contour(dens,
        xlab = "X",
        ylab = "Y",
        main = "Estimación de densidad kernel normal (2D)")
points(x, y, pch = 16, cex = 0.4)

# 6. Superficie 3D de la densidad
persp(dens,
      theta = 30, phi = 30,
      expand = 0.5,
      xlab = "X",
      ylab = "Y",
      zlab = "Densidad",
      main = "Densidad kernel normal (2D)")

###########################################
#Comparacion de dos variables cualitativas#
###########################################

# Base de datos
n <- 120

datos <- data.frame(
  TipoVivienda = sample(
    c("Casa", "Apartamento", "Monoambiente"),
    size = n,
    replace = TRUE,
    prob = c(0.45, 0.40, 0.15)
  ),
  Zona = sample(
    c("Urbana", "Suburbana", "Rural"),
    size = n,
    replace = TRUE,
    prob = c(0.5, 0.3, 0.2)
  )
)


# Convertir a factores
datos$TipoVivienda <- factor(datos$TipoVivienda)
datos$Zona <- factor(datos$Zona)

# Tabla de contingencia
tabla <- table(datos$TipoVivienda, datos$Zona)
print(tabla)

# Proporciones
print(prop.table(tabla))            # proporciones totales
print(prop.table(tabla, margin=1))  # por fila
print(prop.table(tabla, margin=2))  # por columna

# Gráfico de barras agrupadas
par(mfrow=c(1,3))
colores <- rainbow(nrow(tabla))

barplot(tabla,
        beside = TRUE,
        legend = FALSE,
        col=colores,
        xlab = "Zona",
        ylab = "Frecuencia",
        main = "Tipo de vivienda según la zona")
legend("topleft",
       legend = rownames(tabla),
       fill = colores,   # clave para que aparezcan los colores
       cex = 0.8,
       bty = "n")
# Gráfico de barras apiladas
barplot(tabla,
        beside = FALSE,
        col = colores,
        legend = FALSE,
        xlab = "Zona",
        ylab = "Frecuencia",
        main = "Distribución conjunta")

legend("topleft",
       legend = rownames(tabla),
       fill = colores,   # clave para que aparezcan los colores
       cex = 0.8,
       bty = "n")
# Mosaic plot
mosaicplot(tabla,
           main = "Mosaic plot: Tipo de vivienda y zona",
           xlab = "Tipo de vivienda",
           ylab = "Zona",
           color = TRUE)

# Gráfico con ggplot2
library(ggplot2)

ggplot(datos, aes(x = Zona, fill = TipoVivienda)) +
  geom_bar(position = "dodge") +
  labs(title = "Tipo de vivienda según la zona",
       y = "Frecuencia") +
  theme_minimal()

#############################################
#Independencia de dos variables cualitativas#
#############################################

# Crear la tabla de contingencia
tabla <- matrix(
  c(138, 83, 64,
    64, 67, 84),
  nrow = 2,
  byrow = TRUE
)

# Nombrar filas y columnas
rownames(tabla) <- c("Democrata", "Republicano")
colnames(tabla) <- c("A favor", "Indiferente", "En contra")

# Ver la tabla
tabla
# Test chi-cuadrado de independencia
test_chi <- chisq.test(tabla)
test_chi
test_chi$expected
if(test_chi$p.value < 0.05){
  "Se rechaza H0: hay asociación entre preferencia política y postura frente al plebiscito."
} else {
  "No se rechaza H0: no se detecta asociación."
}



