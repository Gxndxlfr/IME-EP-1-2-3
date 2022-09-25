# ------------------- Grupo 7 -------------------
#Integrantes:
#            - NICOLÁS ANDRÉS GABRIELLI ESPINOZA
#            - ISRAEL ALONSO GUZMÁN OYARZÚN 
#            - JOAQUÍN ALONSO MORALES FUENTEALBA 
#            - NICOLAS MARCELO VALDES HERRERA 

<<<<<<< Updated upstream

# • ¿Qué variables se han cargado?
#R: Se cargan un total de 734 variables.
# La primera variable que se identifica es región, por otro lado se carga
# un intervalo de fechas 03/03/2020 - 06/03/2022 (773 días)

# • ¿Qué tipo tiene cada una de estas variables?
#R: La variable región es una variable de tipo categórica nominal, en cambio
#las 773 variables fecha son del tipo numerica discreta

# • ¿Qué escala parecen tener estas variables?
#R: La variable Región cuenta con una escala nominal y las 773 variables fecha
#cuentan con una escala de razón

library(dplyr)



#1. ¿Qué día se produjo el mayor número de casos con síntomas en la región de Los Ríos entre el 01-jun-2020 y 
# el 31-dic-2020? 

#Cargar Datos brutos
datos <- read.csv2(file = "C:/Users/usuario/Desktop/IME/IME-EP-1-2-3/EP1/EP01 Datos Covid.csv",
=======
#Cargar Datos brutos
library (dplyr)
datos <- read.csv(file = "C:/Users/Ekayn/Desktop/codigos/IME-EP-1-2-3/EP1/EP01 Datos Covid.csv",
>>>>>>> Stashed changes
                    encoding = "latin1",
                    sep = ";")

rios<-datos[datos$Region=="Los Ríos",92:305]
rios$X29.06.2020 <- as.numeric(rios$X29.06.2020)
rios$X30.06.2020 <- as.numeric(rios$X30.06.2020)
maximo<-max(rios)
rios<-rios%>%pivot_longer(colnames(rios),names_to="Fecha",values_to="Contagios")
fecha_maximo<-rios%>%filter(Contagios==maximo)

#Variable fecha_maxima contiene la respuesta de la pregunta 1

# 2. ¿Cuál fue el total de casos con síntomas para cada mes de este periodo?

total<-datos[datos$Region=="Total",92:305]

for (j in 2:length(total)) {
  total[j] <- as.numeric(total[j])
}

colnames(total)<-format(as.Date(colnames(total),format="X%d.%m.%Y"),"%m")
total<-total%>%pivot_longer(colnames(total),names_to="Periodo",values_to="Contagios")
total_por_mes<-total%>%group_by(Periodo)%>%summarise(total=sum(Contagios))

#variable total_por_mes contiene la respuesta de la pregunta 2




<<<<<<< Updated upstream
=======
#arreglo con días por mes
cantDias <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
#Modificar datos
>>>>>>> Stashed changes

datosGenerales <- datos
datos <- datos %>% filter(Region == "Los Ríos")
datos$Region <- factor(datos$Region)
#datos <- t(datos)

i <- 2
max <- 0
diaTop <- ""
while (TRUE){
  if (names(datos[i]) == "X01.06.2020"){
    j <- i
    while (TRUE){
      if (names(datos[j]) == "X31.12.2020"){
        if (datos[,j] > max){
          max <- datos[,j]
          #max <- as.numeric(max)
          diaTop <- names(datos[j])
          print(max)
        }
        datosGenerales <- datosGenerales[i:j]
        break
      }
      if (datos[,j] > max){
        max <- datos[,j]
        #max <- as.numeric(max)
        diaTop <- names(datos[j])
      }
      j <- j + 1
    }
    break
  }
  i <- i + 1
}
print(diaTop)


<<<<<<< Updated upstream
  
=======
#1. ¿Qué día se produjo el mayor número de casos con síntomas en la región de Los Ríos entre el 01-jun-2020 y el 31-dic-2020?
#2. ¿Cuál fue el total de casos con síntomas para cada mes de este periodo?
 
 
>>>>>>> Stashed changes
