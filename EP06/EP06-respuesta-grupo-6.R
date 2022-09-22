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
data<-read.csv2(file = "C:/Users/Nico/Desktop/Universidad/IME/IME-EP-1-2-3/EP06/datos.csv",
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

#--Hipotesis
#H0: p1 - p2 = 0
#HA: p1 - p2 != 0

#Nivel de confianza
alfa<-0.05

oncologas<- as.numeric(data %>% filter(Especialidad == "Oncología")%>%select(Mujer))
oncologos<- as.numeric(data %>% filter(Especialidad == "Oncología")%>%select(Hombre))

dermatologas<-as.numeric(data %>% filter(Especialidad == "Dermatología") %>%select(Mujer))
dermatologos<-as.numeric(data %>% filter(Especialidad == "Dermatología") %>%select(Hombre))

total_oncologos<-oncologas+oncologos

total_dermatologos<-dermatologas+dermatologos
n<-c(total_oncologos,total_dermatologos)

exitos<-c(oncologas,dermatologas)

#Se hace la prueba
prueba2<-prop.test(exitos,
                   n=n,
                   alternative = "two.sided",
                   conf.level=1-alfa)

print(prueba2)

# El p value es de un 0.6468, el cual es mayor al nivel de confianza, por lo que se falla al rechazar la hipótesis nula. Por lo tanto 
# se puede concluir con un 95% de confianza que la proporción de autoras en las áreas de oncología y dermatología son iguales.

# 3. Suponiendo que la diferencia en la proporción de autoras en la especialidad de psiquiatría y la de obstetricia
# es de 0,18. ¿A cuántos autores (hombres y mujeres) deberíamos monitorear para obtener un intervalo de
# confianza del 99% y poder estadístico de 90%, si se intenta mantener aproximadamente la misma
# proporción de gente estudiada en cada caso?

alfa <- 0.01
poder <- 0.9

psiquiatria_mujeres<-as.numeric(data %>% filter(Especialidad == "Psiquiatría")%>%select(Mujer))
psiquiatria_hombres<-as.numeric(data %>% filter(Especialidad == "Psiquiatría")%>%select(Hombre))

total_psiquiatria<-psiquiatria_mujeres+psiquiatria_hombres

# Proporción Mujeres Psiquiatría
p1<-psiquiatria_mujeres/total_psiquiatria

obstetricia_mujeres<-as.numeric(data %>% filter(Especialidad == "Obstetricia")%>%select(Mujer))
obstetricia_hombres<-as.numeric(data %>% filter(Especialidad == "Obstetricia")%>%select(Hombre))

total_obstetricia<-obstetricia_mujeres+obstetricia_hombres

# Proporción Mujeres Obstetricia
p2<-psiquiatria_mujeres/total_obstetricia

# Proporciones para que nos de la diferencia:
p1nueva<-0.42   # Proporción nueva para psiquiatria (Mujeres)
p2nueva<-0.24   # Proporción nueva para obstetricia (Mujeres)

fraccion <- total_psiquiatria / (total_psiquiatria + total_obstetricia)

resultado <- bsamsize(p1 = p1nueva,
                      p2 = p2nueva,
                      fraction = fraccion,
                      alpha = alfa, power = poder)

psiquiatria_mujeres_nuevo <- ceiling(resultado[1])
obstetricia_mujeres_nuevo <- ceiling(resultado[2])

cat("Se deben monitorear", psiquiatria_mujeres_nuevo, "autores en psiquiatria y", obstetricia_mujeres_nuevo, "en obstetricia")
