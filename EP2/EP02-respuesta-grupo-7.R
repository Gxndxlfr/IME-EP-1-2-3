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
                                                           sd( ytot ) , sd(ytot)/median(ytot), mean(ing.comuna), sum(ytot))
resumen_ingresos_hombres <- group_by(ingresos_hombres, comuna)%>%summarise( count = n() , mean(ytot) , median ( ytot ) ,
                                                                            sd( ytot ) , sd(ytot)/median(ytot), mean(ing.comuna), sum(ytot))
names(resumen_ingresos_mujeres)[3] <- "Media"
names(resumen_ingresos_mujeres)[4] <- "Mediana"
names(resumen_ingresos_mujeres)[5] <- "Desvest"
names(resumen_ingresos_mujeres)[6] <- "Coef Var"
names(resumen_ingresos_mujeres)[7] <- "ing.comuna"
names(resumen_ingresos_mujeres)[8] <- "Suma"

names(resumen_ingresos_hombres)[3] <- "Media"
names(resumen_ingresos_hombres)[4] <- "Mediana"
names(resumen_ingresos_hombres)[5] <- "Desvest"
names(resumen_ingresos_hombres)[6] <- "Coef Var"
names(resumen_ingresos_hombres)[7] <- "ing.comuna"
names(resumen_ingresos_hombres)[8] <- "Suma"


g <- ggplot (resumen_ingresos_mujeres, aes ( fill = resumen_ingresos_mujeres$`comuna`, x =  resumen_ingresos_mujeres$`ing.comuna`, y =  resumen_ingresos_mujeres$`Media`) )
g <- g + geom_bar(position = "dodge", stat = "identity")
g <- g + labs(y = "Media") + labs(x = "Ranking") + labs(fill = "Comunas")
g <- g + theme_pubr()

print(g)