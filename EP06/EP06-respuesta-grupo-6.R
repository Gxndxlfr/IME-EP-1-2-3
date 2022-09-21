# ------------------- Grupo 6 -------------------
#Integrantes:
#            - NICOLÁS ANDRÉS GABRIELLI ESPINOZA
#            - ISRAEL ALONSO GUZMÁN OYARZÚN 
#            - JOAQUÍN ALONSO MORALES FUENTEALBA 
#            - NICOLAS MARCELO VALDES HERRERA 

#Caso de analisis
# Los siguientes datos se basan en un artículo publicado por Hart & Perlis (2019) (JAMA Internal Medicine, 179(9),
# 1285-1287) acerca de la proporción de mujeres autoras de artículos científicos en el área médica. La tabla
# muestra la cantidad de autoras y autores para diferentes especialidades.


#Librerias utiles
library(dplyr)
library(pwr)
library(Hmisc)

#Lectura de datos
data<-read.csv2(file = "C:/Users/israe/OneDrive/Escritorio/universidad/8vo semestre/IME/EP/IME-EP-1-2-3/EP06/datos.csv",
                encoding = "latin1",
                sep = ";")

# 1. Estudios previos habían determinado que la proporción de autoras en la especialidad de oncología era de
# 32%. ¿Respaldan estos datos tal estimación?

#--Hipotesis
#H0: p = 0.32
#HA: p != 0.32

#Valor nulo
p_nulo<-0.32

#Definir un nivel de confianza alfa
alfa<-0.05

#Calcular la probabilidad de exito
oncologas<-as.numeric(data%>%filter(Especialidad=="Oncología")%>%select(Mujer))
oncologos<-as.numeric(data%>%filter(Especialidad=="Oncología")%>%select(Hombre))
cantidad_mujeres<-as.numeric(sum(data$Mujer))

cantidad_oncologos<-oncologas+oncologos


#Comprobar prueba
prueba1<-prop.test(oncologas,
                   n=cantidad_oncologos,
                   p=0.32,
                   alternative = "two.sided",
                   conf.level = 1-alfa)

print(prueba1)

#Existe suficiente evidencia para rechazar la hipotesis alternativa en favor a 
# la hipotesis nula con un nivel de confianza de alfa=0.05




# 2. Según estos datos, ¿es igual la proporción de autoras en las áreas de oncología y dermatología?



# 3. Suponiendo que la diferencia en la proporción de autoras en la especialidad de psiquiatría y la de obstetricia
# es de 0,18. ¿A cuántos autores (hombres y mujeres) deberíamos monitorear para obtener un intervalo de
# confianza del 99% y poder estadístico de 90%, si se intenta mantener aproximadamente la misma
# proporción de gente estudiada en cada caso?
  
  
  
  