# ------------------- Grupo 7 -------------------
#Integrantes:
#            - NICOLÁS ANDRÉS GABRIELLI ESPINOZA
#            - ISRAEL ALONSO GUZMÁN OYARZÚN 
#            - JOAQUÍN ALONSO MORALES FUENTEALBA 
#            - NICOLAS MARCELO VALDES HERRERA 

# Librerias útiles
library(dplyr)
library(ggpubr)
# Cargar Datos
datos <- read.csv2(file = "C:/Users/Nico/Desktop/Universidad/IME/IME-EP-1-2-3/EP2/EP02 Datos Casen 2017.csv",
                   encoding = "UTF-8",
                   sep = ";")
datos$ytot <- as.numeric(datos$ytot)
#Filtrar Ingresos por sexo

ingresos <- datos%>%select(sexo,region,comuna,ing.comuna,ytot)

ingresos_hombres <- ingresos%>%filter(sexo=="Hombre")
ingresos_mujeres <- ingresos%>%filter(sexo=="Mujer")

resumen_ingresos_mujeres <- group_by(ingresos_mujeres, comuna)%>%summarise( count = n() , mean(ytot) , median ( ytot ) ,
                                                           sd( ytot ) , sd(ytot)/median(ytot), mean(ing.comuna))
resumen_ingresos_hombres <- group_by(ingresos_hombres, comuna)%>%summarise( count = n() , mean(ytot) , median ( ytot ) ,
                                                                            sd( ytot ) , sd(ytot)/median(ytot), mean(ing.comuna))
names(resumen_ingresos_mujeres)[3] <- "Moda"
names(resumen_ingresos_mujeres)[4] <- "Media"
names(resumen_ingresos_mujeres)[5] <- "Desvest"
names(resumen_ingresos_mujeres)[6] <- "Coef Var"
names(resumen_ingresos_mujeres)[7] <- "ing.comuna"

media_ingresos_hombres <- mean(ingresos_hombres$ytot)
media_ingresos_mujeres <-mean(ingresos_mujeres$ytot)

mediana_hombres <- median(ingresos_hombres$ytot)
mediana_mujeres <- median(ingresos_mujeres$ytot)

desviacion_hombres <- sd(ingresos_hombres$ytot)
desviaion_mujeres <- sd(ingresos_mujeres$ytot)

coef_var_hombres <- desviacion_hombres/media_ingresos_hombres
coef_var_mujeres <- desviaion_mujeres/media_ingresos_mujeres

