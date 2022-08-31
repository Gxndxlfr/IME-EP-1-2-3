# ------------------- Grupo 7 -------------------
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
poblacion <- read.csv2(file = "C:/Users/israe/OneDrive/Escritorio/universidad/8vo semestre/IME/EP/IME-EP-1-2-3/EP2/EP02 Datos Casen 2017.csv",
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

#-----------Definir semilla general 5000 datos con una distribución aproximadamente normal ------------
set.seed(984348)
ingreso.normal <- rnorm(5000, mean = media.ingreso, sd = sd.ingreso)

#1. ----------- Distribución Z -----------

#1.1 Medidas estadísticas

media_z <- mean(ingreso.normal)
desv_std_z <- sd(ingreso.normal)

#1.2 Arreglo valores distribución 

#se construye una lista con los valores de la semilla inicial tomando
#en cuenta la media y desviación estandar  de los datos distribuidos normalmente

dist_z<-list() #arreglo de valores de distribución
for (x in ingreso.normal) {
  dist_z<-c(dist_z,(media_z-x)/desv_std_z)
  
}

#1.3 Graficar

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


#2. ----------- Distribución chi cuadrado -----------

#2.1  Distribucón chi cuadrado con más de 3 grados de libertad.
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

#2.1.1 Gráfico

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
#2.2  Distribucón chi cuadrado con menos de 15 grados de libertad.

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

#2.2.1 Gráfico
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

#3. ----------- Distribución F -----------

#3.1 arreglo de valores de distribución
f<-list()
for (i in 1:5000){
  x1<-chiq_4[[i]]/4 #k = 4
  x2<-chiq_14[[i]]/14 # k = 14
  f<-c(f,x1/x2)
}

#3.2 Gráfico
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

