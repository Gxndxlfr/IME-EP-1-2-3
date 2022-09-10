# ------------------- Grupo 6 -------------------
#Integrantes:
#            - NICOLÁS ANDRÉS GABRIELLI ESPINOZA
#            - ISRAEL ALONSO GUZMÁN OYARZÚN 
#            - JOAQUÍN ALONSO MORALES FUENTEALBA 
#            - NICOLAS MARCELO VALDES HERRERA 

#Caso de analisis
# Se sabe que una máquina que envasa detergentes industriales llena bidones con un volumen de producto que
# sigue una distribución normal con desviación estándar de 1 litro.
# Usando una muestra aleatoria de 100 botellas, el ingeniero a cargo de la planta requiere determinar si 
# la máquina está llenando los bidones con una media de 10 litros.

#Librerias utiles
library(dplyr)
library(ggpubr)
library(pwr)

# 1. Si el ingeniero está seguro de que el verdadero volumen medio no puede ser superior a 10 litros y piensa
# rechazar la hipótesis nula cuando la muestra presente una media menor a 9,5 litros, ¿cuál es la probabilidad
# de que cometa un error de tipo I?

#--Hipotesis
#H0: media_1 = 10
#HA: media_1 < 10

n <- 100 #Tamaño muestra

#Valor nulo
media_1 <- 10

#Valor minimo para el intervalo de confianza
min_1 <- 9.5

#Desviación estandar (error estandar)
sd_1 <- 1/sqrt(100)


alfa <- pnorm(min_1, mean = media_1, sd = sd_1, lower.tail = TRUE)

cat("La probabilidad de cometer un error de tipo 1 es de un ",alfa*100,"%")

#   2. Si el verdadero volumen medio de los bidones fuera de 9,7 litros, ¿cuál sería la probabilidad de que el
# ingeniero, que obviamente no conoce este dato, cometa un error de tipo II?


#--Hipotesis
#H0: media = 10
#HA: media != 10

valor_nulo <- media_1
valor_real <- 9.7

#en el caso de que la hipotesis nula es verdadera
x <- seq(valor_nulo -5*sd_1, valor_nulo + 5*sd_1, 0.01)
y <- dnorm(x,mean=valor_nulo,sd=sd_1)
g <- ggplot(data=data.frame(x,y),aes(x))

#---Grafico
g <- g + stat_function(fun=dnorm,args=list(mean=valor_nulo,sd=sd_1),colour="red",size=1)
g <- g + ylab("")
g <- g + scale_y_continuous(breaks=NULL)
g <- g + scale_x_continuous(name="Volumen medio")

#área de rechazo

valor_critico <- qnorm(alfa,mean=valor_nulo,sd=sd_1,lower.tail = TRUE)

inf_critico <- valor_nulo - valor_critico

sup_critico <- valor_nulo + valor_critico


g <- g + geom_area(data=subset(data.frame(x,y),x >= valor_critico),
                   aes(y = y),
                   colour="red",
                   fill="red",
                   alpha=0.5)

g <- g + stat_function(fun=dnorm,args=list(mean=valor_real,sd=sd_1),colour="blue",size=1)
x1 <- seq(valor_nulo -5*sd_1, valor_nulo + 5*sd_1, 0.01)
y1 <- dnorm(x,mean=valor_real,sd=sd_1)

g<-g+geom_area(data=subset(data.frame(x1,y1),
                           x1 >= valor_critico),
               aes(x=x1,y=y1),
               colour="blue",
               fill="blue",
               alpha=0.5)
print(g)

p <- 1 - power.t.test(n=100,
                      delta=0.2,
                      sd=1,
                      sig.level=alfa,
                      power=NULL,
                      type = "one.sample",
                      alternative="one.sided")$power

cat("La probabilidad de cometer un error de tipo 2 es de un ",p*100,"%")

#   3. Como no se conoce el verdadero volumen medio, genere un gráfico del poder estadístico con las
# condiciones anteriores, pero suponiendo que el verdadero volumen medio podría variar de 9,3 a 10 litros.


# 4. Considerando un volumen medio de 10 litros, ¿cuántos bidones deberían revisarse para conseguir un poder
# estadístico de 0,8 y un nivel de significación de 0,05?


#   5. ¿Y si el ingeniero fuese muy exigente y quisiera reducir la probabilidad de cometer un error de tipo I a un 1%
# solamente?