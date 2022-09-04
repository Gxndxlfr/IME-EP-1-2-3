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
datos <- read.csv2(file = "C:/Users/israe/OneDrive/Escritorio/universidad/8vo semestre/IME/EP/IME-EP-1-2-3/EP04/EP04 datos.csv",
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
#   3. ¿Es posible afirmar que, en promedio, los atletas negros superan a los orientales por más de 1,76 segundos
# después del entrenamiento?


  