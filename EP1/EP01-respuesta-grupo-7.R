# ------------------- Grupo 7 -------------------
#Integrantes:
#            - NICOLÁS ANDRÉS GABRIELLI ESPINOZA
#            - ISRAEL ALONSO GUZMÁN OYARZÚN 
#            - JOAQUÍN ALONSO MORALES FUENTEALBA 
#            - NICOLAS MARCELO VALDES HERRERA 


library(dplyr)

#Cargar Datos brutos
datos <- read.csv2(file = "C:/Users/israe/OneDrive/Escritorio/universidad/8vo semestre/IME/EP/IME-EP-1-2-3/EP1/EP01 Datos Covid.csv",
                    encoding = "latin1",
                    sep = ";")

#arreglo con días por mes
cantDias <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
#Modificar datos
datos$Region <- factor(datos$Region)

#obtener región objetivo "Los ríos"
regionObjetivo <- datos %>% filter(Region == "Los Ríos")

max <- 0
regionObjetivo <- t(regionObjetivo)
for (j in 92:305) {
  print(regionObjetivo[j])
  num <- as.numeric(regionObjetivo[j]) 
  if(num >= max) {
    max <- num
    print(j)
  }
  
}
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

# escala nominal: sirve solo para separar un conjunto de elementos en subclases excluyentes entre sí.
# Los valores no son más que nombres o estados, por lo que no podemos hacer operaciones aritméticas
# ni podemos establecer relaciones de orden.
# 
# Escala de razón: cumple con todos los atributos de la escala de intervalos, pero además tiene su origen
# en un cero verdadero. Ejemplos de tales escalas son, por ejemplo, las que permiten medir la masa o
# la distancia. En una escala de razón, la diferencia entre dos puntos es independiente de la unidad de
# medida. Por ejemplo, si medimos la masa de dos objetos, la razón es constante independientemente de
# si empleamos kilogramos, libras u onzas (a diferencia de lo que ocurre con la temperatura usando las
# escalas Celsius y Fahrenheit). +  no existe una transformación lineal que nos
#permite transformar una medida en una escala a su equivalente en otra escala





#1. ¿Qué día se produjo el mayor número de casos con síntomas en la región de Los Ríos entre el 01-jun-2020 y 
# el 31-dic-2020?
# 2. ¿Cuál fue el total de casos con síntomas para cada mes de este periodo?
  
