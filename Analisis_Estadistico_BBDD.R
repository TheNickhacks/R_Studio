

#Nicolas Navarro
setwd('C:/Users/C403-15/Desktop/Estadistica')

Simce <- read.csv('simce8b2013.csv')

View(Simce)

#B)
Tabla_Comuna<-table(Simce$COMUNA)

View(Tabla_Comuna)

#C)
install.packages('dplyr') #manipulacion de datos
library(dplyr) #activar libreria

#Esta funcion suma por comuna la cantidad de estudiantes, para entregar la 
#Rendicion total de pruebas por comuna
Agrupar_Com<-Simce %>% group_by(COMUNA) %>% 
  summarise(TotalAlum = sum(ALUMNOS))

#Busca el valor maximo en la columna totalalumnos
Maximo<-max(Agrupar_Com$TotalAlum)

#Entrega el indice mayor de la columna totalalumnos
which.max(Agrupar_Com$TotalAlum) #Max en pos. 38

#muestra la posicion 38 en la fila comuna
Agrupar_Com$COMUNA[38]

#D)
#Sumar la cantidad de alumnos y dividirlos por la cantidad de comunas
promedio <- sum(Agrupar_Com$TotalAlum)/length(Agrupar_Com$COMUNA)
print(promedio)

#E)
Agrupardiffpunta<- Simce %>% group_by(COMUNA) %>% 
  summarise(Promdplen = sum(DIF_LECT)/length(COMUNA))
View(Agrupardiffpunta)

which.min(Agrupardiffpunta$Promdplen)
Agrupardiffpunta$COMUNA[44]
