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
datos <- read.csv2(file = "C:/Users/Ekayn/Desktop/codigos/IME-EP-1-2-3/EP2/EP02 Datos Casen 2017.csv",
                   encoding = "UTF-8",
                   sep = ";")
datos$ytot <- as.numeric(datos$ytot)

<<<<<<< Updated upstream
=======
#Crear una matriz que contenga solo Region, Comuna, ing.ingresos e ytot
#Filtrar Ingresos por sexo
ingresos <- datos%>%select(sexo,region,comuna,ing.comuna,ytot)
>>>>>>> Stashed changes

# Filtrar Ingresos por sexo
ingresos <- datos%>%select(sexo,region,comuna,ing.comuna,ytot)
ingresos_mujeres <- ingresos%>%filter(sexo=="Mujer")

# Se filtran los ingresos de las mujeres por comuna y se hace un summarise
resumen_ingresos_mujeres <- group_by(ingresos_mujeres, comuna)%>%summarise( count = n() , mean(ytot) , median ( ytot ) ,
                                                           sd(ytot) , sd(ytot)/median(ytot), mean(ing.comuna), sum(ytot))

resumen_ingresos <- group_by(ingresos, comuna)%>%summarise( count = n() , mean(ytot) , median ( ytot ) ,
                                                                            sd(ytot) , sd(ytot)/median(ytot), mean(ing.comuna), sum(ytot))
names(resumen_ingresos_mujeres)[3] <- "Media"
names(resumen_ingresos_mujeres)[4] <- "Mediana"
names(resumen_ingresos_mujeres)[5] <- "Desvest"
names(resumen_ingresos_mujeres)[6] <- "Coef Var"
names(resumen_ingresos_mujeres)[7] <- "ing.comuna"
<<<<<<< Updated upstream
names(resumen_ingresos_mujeres)[8] <- "Suma"

# Gráfico
g <- ggplot (resumen_ingresos_mujeres, aes ( fill = resumen_ingresos_mujeres$`comuna`, x =  resumen_ingresos_mujeres$`ing.comuna`, y =  resumen_ingresos_mujeres$`Media`) )
g <- g + geom_bar(position = "dodge", stat = "identity")
g <- g + labs(y = "Media") + labs(x = "Ranking") + labs(fill = "Comunas")
g <- g + theme_pubr()

g1 <- ggplot (resumen_ingresos_mujeres, aes ( fill = resumen_ingresos_mujeres$`comuna`, x =  resumen_ingresos_mujeres$`ing.comuna`, y =  resumen_ingresos_mujeres$`Mediana`) )
g1 <- g1 + geom_bar(position = "dodge", stat = "identity")
g1 <- g1 + labs(y = "Mediana") + labs(x = "Ranking") + labs(fill = "Comunas")
g1 <- g1 + theme_pubr()


print(g)
print(g1)

# Respuesta Pregunta Grupo 7:
# Como grupo determinamos que, si bien hay casos en los que el promedio de ingreso de mujeres a 
# medida que sea más alto mayor es el ranking, no se refleja el mismo comportamiento para todas las comunas, ya que, 
# el coeficiente de variación es alto y distinto por comuna no se considera la media como una medida de tendencia central representativa,
# por lo que se considera la mediana. 
# De lo anterior se logra concluir que no existe una gran relación entre los ingresos de las mujeres en la región metropolitana 
# y el ranking asociado a sus respectivos municipios.
=======

media_ingresos_hombres <- mean(ingresos_hombres$ytot)
media_ingresos_mujeres <-mean(ingresos_mujeres$ytot)

mediana_hombres <- median(ingresos_hombres$ytot)
mediana_mujeres <- median(ingresos_mujeres$ytot)

desviacion_hombres <- sd(ingresos_hombres$ytot)
desviaion_mujeres <- sd(ingresos_mujeres$ytot)

coef_var_hombres <- desviacion_hombres/media_ingresos_hombres
coef_var_mujeres <- desviaion_mujeres/media_ingresos_mujeres


library (dplyr)
library (ggpubr)


datos <- read.csv(file = "C:/Users/Ekayn/Desktop/codigos/IME-EP-1-2-3/EP2/EP02 Datos Casen 2017.csv",
                  encoding = "latin1",
                  sep = ";")

#------------------------------------------------------------

# Histograma para la variable Rendimiento .
g1 <- gghistogram ( resumen_ingresos_mujeres$Media ,
                      x = " Rendimiento ",
                      bins = 10 ,
                      add = " mean ",
                      xlab = "Ingreso medio mujeres/comuna",
                      ylab = "Frecuencia",
                      color = "blue",
                      fill = "blue")

print ( g1 )


>>>>>>> Stashed changes
