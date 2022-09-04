# ------------------- Grupo 6 -------------------
#Integrantes:
#            - NICOLÁS ANDRÉS GABRIELLI ESPINOZA
#            - ISRAEL ALONSO GUZMÁN OYARZÚN 
#            - JOAQUÍN ALONSO MORALES FUENTEALBA 
#            - NICOLAS MARCELO VALDES HERRERA 

#Librerias utiles
library(TeachingDemos)
library(dplyr)
library(ggpubr)

#Se cargan los datos desde el csv
datos <- read.csv2(file = "C:/Users/Nico/Desktop/Universidad/IME/IME-EP-1-2-3/EP04/EP04 datos.csv",
                       encoding = "UTF-8",
                       sep = ";")
# 1. El Comité Olímpico cree que el mejor tiempo medio de los atletas orientales antes de ingresar al programa
# de entrenamiento es de 19,75 segundos. ¿Soportan los datos esta afirmación?

  
#-----Hipotesis-----
# H0: media = 19,75
# HA: media != 19,75

#Nivel de significancia
alfa<-0.05

#Filtrar datos por raza "oriental"
atletas_orientales<-datos%>%filter(Raza=="Oriental")

#Filtrar por variable Previo
atletas_orientales_previo <- atletas_orientales[["Previo"]]

#Testing de normalidad
testNorm <- shapiro.test(atletas_orientales_previo)

print(testNorm)

#De la prueba de shapiro se concluye que los valores se distribuyen de manera cercana a la normal.

#como la información que contienen las filas son de cada atleta, los valores son independientes y
#contemos con menos de 30 atletas.

#Gracias a esto damos paso a la prueba T para evaluar la hipotesis

#Estadísticos a utilizar
media_1 <- mean(atletas_orientales_previo)
dv_1 <- sd(atletas_orientales_previo) 


#Prueba t
test_t_1<-t.test(atletas_orientales_previo,mu=19.75,alternative = "two.sided",
                 stdev = dv_1,conf.level = 1-alfa)
print(test_t_1) 


#Conclusión pregunta 1

#Los resultados de la prueba t permite concluir que se rechaza la  hipotesis nula, con un alfa = 0.05.
#Por lo tanto, el mejor tiempo medio de los atletas orientales antes de ingresar al programa de 
#entrenamiento no es 19,75.


#   2. ¿Sugieren los datos que la mejor marca de los atletas negros se reduce en menos de 4,17 segundos tras el
# entrenamiento?

#-----Hipotesis-----
# H0: posterior-previo = -4,17
# HA: posterior-previo > -4,17

# Se extraen los atletas negros
atletas_negros <- datos%>%filter(Raza=="Negra")

# Se obtienen la muestra con la diferencia
dif_negros<-atletas_negros$Posterior-atletas_negros$Previo

# Se verifica utilizando el test de shapiro si la distribución es normal
normalidad<-shapiro.test(dif_blanco)
print(normalidad)
# Como la distribución es cercana a lo normal, con tamaño menor a 30 e independiente
# se utiliza la prueba t de student

# Nivel de significancia
alfa<-0.05

# Prueba T de Student
prueba_2<-t.test(dif_negros, y=NULL, alternative = "greater",
                 mu = -4.17, conf.level = 1-alfa)
print(prueba_2)

# Se falla al rechazar la hipotesis nula pues p-value = 0.3027 > 0.05, por lo tanto
# se puede concluir con 95% de confianza que en promedio la mejor marca de los atletas negros se reduce 
# en 4,17 segundos tras el entrenamiento.

#   3. ¿Es posible afirmar que, en promedio, los atletas negros superan a los orientales por más de 1,76 segundos
# después del entrenamiento?

#-----Hipotesis-----
# H0: media_orientales-media_negros = 1.76
# HA: media_orientales-media_negros > 1.76

# Obtengamos las muestras con las que trabajaremos.
datos_negros <- data %>% filter(Raza == 'Negra')
datos_negros <- datos_negros$Posterior

datos_orientales <- data %>% filter(Raza == 'Oriental')
datos_orientales <- datos_orientales$Posterior

# Las muestras son pequeñas (<30 observaciones).

# Se verifica si la distribución es cercana a lo normal.
n.negros <- shapiro.test(datos_negros)
n.orientales <- shapiro.test(datos_orientales)

print(n.negros)
print(n.orientales)

# Se aprecia que las distribuciones son cercanas a lo normal (Pues ambas tienen un p-value muy superior a 0.05) 
# y se infiere que las muestras son independientes.

# Nivel de significancia
alfa<-0.05

# Se utiliza el test t de Student.
prueba_3 <- t.test(y=datos_negros,
                   x=datos_orientales,
                   paired=FALSE,
                   alternative= "greater",
                   mu=1.76,
                   conf.level=1-alfa
)
print(prueba_3)

# Como el p-value = 0.00255, se rechaza la hipótesis nula con un nivel de significancia del 0.05. 
# Por ende es posible afirmar en un 95% que en promedio los atletas negros superan a los orientales por más de 1,76 segundos después 
# del entrenamiento.
  