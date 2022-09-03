#1. ------------------- Grupo 7 -------------------
#Integrantes:
#            - NICOLÁS ANDRÉS GABRIELLI ESPINOZA
#            - ISRAEL ALONSO GUZMÁN OYARZÚN 
#            - JOAQUÍN ALONSO MORALES FUENTEALBA 
#            - NICOLAS MARCELO VALDES HERRERA 

# Librerias útiles
library(dplyr)
library(ggpubr)
library(tidyr)

#Se cargan los datos desde el csv; Datos casen 2017
poblacion <- read.csv2(file = "C:/Users/Nico/Desktop/Universidad/IME/IME-EP-1-2-3/EP3/EP03 Datos Casen 2017.csv",
                   encoding = "UTF-8",
                   sep = ";")


#Calcular tamaño de la población + medidas estadísticas
tamaño <- nrow(poblacion)
ingreso <- as.numeric(poblacion[["ytot"]])
poda <- 0.2
q20 <- quantile(ingreso, poda)
q80 <- quantile(ingreso, 1 - poda)
ingreso.podado <- ingreso[ingreso > q20 & ingreso < q80]
tamaño.podado <- length(ingreso.podado)
media.ingreso <- mean(ingreso.podado)
sd.ingreso <- sqrt (sum((ingreso.podado - media.ingreso)^2) / tamaño.podado )

#2. ----------- Definir semilla general 5000 datos con una distribución aproximadamente normal -----------
set.seed(984348)
ingreso.normal <- rnorm(5000, mean = media.ingreso, sd = sd.ingreso)

#3. ----------- Distribución Z -----------

#3.1 Medidas estadísticas

media_z <- mean(ingreso.normal)
desv_std_z <- sd(ingreso.normal)

#3.2 Arreglo valores distribución 

#se construye una lista con los valores de la semilla inicial tomando
#en cuenta la media y desviación estandar  de los datos distribuidos normalmente

dist_z<-list() #arreglo de valores de distribución
for (x in ingreso.normal) {
  dist_z<-c(dist_z,(media_z-x)/desv_std_z)
  
}

#3.3 Graficar

df_z<-data.frame("Ingresos estandarizado"=unlist(dist_z))
g1<-gghistogram(df_z,x="Ingresos.estandarizado",
                bins=100,
                xlab="Ingresos estandarizado",
                ylab="Frecuencia",
                color="blue",
                fill="blue",
                add="mean",
                title="Distribución normal")
print(g1)

#4. ----------- Distribución chi cuadrado -----------

#4.1  Distribucón chi cuadrado con más de 3 grados de libertad.
k <- 4
chiq_4 <- list() #arreglo de valores de distribución
for(i in 1:5000){
  ran<-sample(dist_z,size = k) #muestra aleatoria de k elementos
  total<-0
  for (j in ran){
    total <- total+ j*j #suma cuadrados
  }
  
  chiq_4<-c(chiq_4,total)
}

#4.1.1 Gráfico

df_chiq_4<-data.frame("Ingresos"=unlist(chiq_4))
g2<-gghistogram(df_chiq_4,x="Ingresos",
                bins=100,
                xlab="Ingresos",
                ylab="Frecuencia",
                color="blue",
                fill="blue",
                add="mean",
                title="Distribución chi-cuadrado, 4 grados de libertad")
print(g2)
#4.2  Distribucón chi cuadrado con menos de 15 grados de libertad.

k <- 14
chiq_14 <- list() #arreglo de valores de distribución
for(i in 1:5000){
  ran<-sample(dist_z,size = k)#muestra aleatoria de k elementos
  total<-0
  for (j in ran){
    total <- total+ j*j #suma cuadrados
  }
  
  chiq_14<-c(chiq_14,total)
}

#4.2.1 Gráfico
df_chiq_14<-data.frame("Ingresos"=unlist(chiq_14))
g3<-gghistogram(df_chiq_14,x="Ingresos",
                bins=100,
                xlab="Ingresos",
                ylab="Frecuencia",
                color="blue",
                fill="blue",
                add="mean",
                title="Distribución chi-cuadrado, 14 grados de libertad")
print(g3)

#5. ----------- Distribución F -----------

#5.1 arreglo de valores de distribución
f<-list()
for (i in 1:5000){
  x1<-chiq_4[[i]]/4 #k = 4
  x2<-chiq_14[[i]]/14 # k = 14
  f<-c(f,x1/x2)
}

#5.2 Gráfico
df_f<-data.frame("Ingresos"=unlist(f))
g4<-gghistogram(df_f,x="Ingresos",
                bins=100,
                xlab="Ingresos",
                ylab="Frecuencia",
                color="blue",
                fill="blue",
                add="mean",
                title="Distribución F")
print(g4)

#.----------------------------------------------------------------
#.7 ---------Definir semilla y número de repeticiones ----------
#.----------------------------------------------------------------

set.seed(12345)
n.repeticiones <- 20

ensayo <- function(x)
  ifelse(sample(poblacion[["sexo"]], 1) == "Mujer", 1, 0)

veinte.repeticiones <- sapply(1:n.repeticiones, ensayo)

#Se calcula p; la probabilidad de que una mujer sea escogida de la muestra
cantidad.mujeres<-sum(veinte.repeticiones)
p<-cantidad.mujeres/n.repeticiones

#8. ----------- Distribución binomial -----------

#8.1 arreglo de distribución binormal
binomil<-list()
for(i in 1:n.repeticiones){
  binomil<-c(binomil,choose(n.repeticiones,i)*p^i*(1-p)^(n.repeticiones-i))
}

#8.2 Gráfico
df_bin <- data.frame("K"=c(1:n.repeticiones),"Probabilidad"=unlist(binomil))
g5<-ggbarplot(df_bin,x="K",y="Probabilidad",fill="Blue")

print(g5)

#9. ----------- Distribución geométrica -----------

#9.1 arreglo de distribución geométrica
geo<-list()
for (i in 1:n.repeticiones){
  geo<-c(geo, (1-p)^(i-1)*p)
}

#9.2 Gráfico
df_geo <- data.frame("K"=c(1:n.repeticiones),"Probabilidad"=unlist(geo))
g6<-ggbarplot(df_geo,x="K",y="Probabilidad",fill="Blue")

print(g6)

#10. ----------- Distribución binomial negativa -----------

#10.1 arreglo de distribución binomial negativa
bin.neg<-list()
for (i in 1:n.repeticiones){
  pr<-choose(n.repeticiones-1,i-1)*p^(i)*(1-p)^(n.repeticiones-i)
  bin.neg<-c(bin.neg,pr)
}

#10.2 Gráfico
df_bin.neg <- data.frame("K"=c(1:n.repeticiones),"Probabilidad"=unlist(bin.neg))
g7<-ggbarplot(df_bin.neg,x="K",y="Probabilidad",fill="Blue")

print(g7)