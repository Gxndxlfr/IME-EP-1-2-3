# ------------------- Grupo 6 -------------------
#Integrantes:
#            - NICOLÁS ANDRÉS GABRIELLI ESPINOZA
#            - ISRAEL ALONSO GUZMÁN OYARZÚN 
#            - JOAQUÍN ALONSO MORALES FUENTEALBA 
#            - NICOLAS MARCELO VALDES HERRERA 


#Librerias utiles
library(tidyverse)
library(RVAideMemoire)
library(rcompanion)

# 1. Estudios científicos han descubierto que la inteligencia musical está altamente relacionada con la
# inteligencia matemática. Pensando en mejorar la oferta de actividades culturales y recreativas, una
# Universidad ha examinado la preferencia de talleres de un grupo de 8 estudiantes de carreras científicas y
# 11 de carreras humanistas, encontrando que 6 de los primeros y 2 de los segundos participaron de talleres
# musicales. ¿Existe relación entre el tipo de carrera que cursan los estudiantes y su participación en talleres
# musicales?

#--Hipotesis
#H0: estudiantes de carreras científicas y humanistas prefieren de igual manera los talleres musicales
#H1: estudiantes de carreras científicas y humanistas NO prefieren de igual manera los talleres musicales


cientificos<-c(6,2)
humanistas<-c(2,9)

tabla<-as.table(rbind(cientificos,humanistas))

dimnames(tabla)<-list(etario=c("cientificos","humanistas joven"),
                      preferencia=c("Participa","No participa"))
print(tabla)

prueba1<-fisher.test(tabla)
print(prueba1)


#Con un nivel de significancia alfa=0.05 existe suficiente evidencia para 
#rechazar la hipotesis nula. Es decir los estudiantes de carreras científicas y humanistas
# NO prefieren de igual manera los talleres musicales.


#   2. Un polémico estudio realizado con 12 pares de gemelos abandonados por sus padres, donde uno de los
# gemelos fue adoptado por una familia funcional y el otro creció en un centro del estado, ha encontrado que,
# al llegar a la edad adulta:
#   ▪ Solo en una pareja de gemelos, ambos presentaron trastornos psicológicos.
# ▪ En dos casos, solo el gemelo adoptado presentó trastornos psicológicos.
# ▪ En 5 de los casos, solo el gemelo que creció en el centro estatal presentó trastornos psicológicos.
# ▪ En los casos restantes, ambos gemelos están libres de problemas psicológicos.
# ¿Influye el entorno en que un niño crece en la aparición de trastornos psicológicos en la edad adulta?

# Hipotesis:
# H0: No influye el entorno en que un niño crece en la aparición de trastornos psicológicos en la edad adulta
# H1: Influye el entorno en que un niño crece en la aparición de trastornos psicológicos en la edad adulta

adoptado <- c(rep("Con Trastorno", 1), rep("Con Trastorno", 2), rep("Sin Trastorno", 5), rep("Sin Trastorno", 4))
centro <- c(rep("Con Trastorno", 1), rep("Sin Trastorno", 2), rep("Con Trastorno", 5), rep("Sin Trastorno", 4))
tabla2 <- table(adoptado, centro)
prueba2 <- mcnemar.test(tabla2)
print(tabla2)
print(prueba2)

# Se puede concluir que se falla al rechazar la hipotesis nula porque el p-value = 0.4497 es mucho mayor al nivel de significancia 0.05, 
# por ende, se podría decir que con un 95% de confianza que no influye el entorno en que un niño crece con la aparición de trastornos 
# psicológicos en la edad adulta.


#   3. El 21 de marzo de 2022 se realizó un estudio acerca de la aprobación al presidente Gabriel Boric entre los
# estudiantes de una prestigiosa universidad a fin de comparar los resultados con los obtenidos en la misma
# encuesta a nivel nacional, obteniéndose los resultados que se muestran en la tabla. ¿Refleja la opinión
# estudiantil la percepción del país?

estudiantes <- c(125, 77, 21)
nacional <- c(5046, 3421, 706)

#R: Dado que se quiere conocer si la opinion de los estudiantes es representativa de la opinion nacional
#   Se utiliza la prueba chi-cuadrado de Pearson
#   Verificando las condiciones para esta prueba

#1. Las observaciones deben ser independientes entre sí.
#2. Debe haber a lo menos 5 observaciones esperadas en cada grupo.

#Se puede asumar que los resultados de la encuesta son independientes entre si.
#Se poseen mas de 5 observaciones esperadas en cada grupo

#Se definen las hipotesis a contrastar:
#H0: Los estudiantes y la nacion tienen la misma opinion respecto al presidente Gabriel Boric
#Ha: Los estudiantes y la nacion tienen diferentes opiniones respecto al presidente Gabriel Boric

tabla <- as.table(rbind(estudiantes, nacional))

dimnames(tabla) <- list( c("Estudiantes", "Nacional"),
                         c("Aprueba", "Rechaza", "Nulo"))

print(tabla)

prueba <- chisq.test(tabla)
print(prueba)

# Con un nivel de significancia a=0,05 existe suficiente evidencia para aprobar la hipotesis nula.
# Por lo tanto se concluye que la encuesta a nivel estudiantil y nacional presentan las mismas preferencias.


#   4. La Facultad de Ingeniería desea saber si existe diferencia significativa en el desempeño de los estudiantes en
# asignaturas críticas de primer semestre. Para ello, le ha entregado un archivo de datos que, para 3
# asignaturas, indica si una muestra de 50 estudiantes aprobó o reprobó. ¿Qué puede concluir la Facultad?
# Indicación: obtenga la muestra a partir del archivo EP07 Datos.csv, usando la semilla 453. Considere un nivel
# de significación α=0,05.  

#Hipotesis establecidas
#H0: La proporciOn de estudiantes aprobados es igual en todos los ramos
#H1: La proporcion de estudiantes aprobados es distinta en al menos un ramo

archivo4 <- read.csv2(file = 'C:/Users/Ekayn/Desktop/codigos/IME-EP-1-2-3/EP07/EP07 Datos.csv',
                      encoding = "UTF-8",
                      sep = ";")
set.seed(453)
datos4<-sample_n(archivo4,50)
datos4<-datos4%>%pivot_longer(c("Calculo","Algebra","Fisica"),
                              names_to="Asignatura",
                              values_to="Estado"
)
datos4[["Asignatura"]]<-factor(datos4[["Asignatura"]])
datos4[["Estado"]]<-factor(datos4[["Estado"]])

prueba4<-cochran.qtest(Estado~Asignatura|Id,data=datos4,alpha=0.05)
print(prueba4)

#Con un nivel de significancia de alfa=0.05 existe suficiente evidencia para rechazar la hipotesis alternativa.
#Por lo tanto se concluye que la proporción de aprobación de los 3 ramos es similar.