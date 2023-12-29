library(readxl)
library(readr)
library(ggplot2)
library(resumeRdesc)
library(modeest)

#Nicolas Navarro Garrido
#Carlos Ramirez Vallejos
#Alexandra Vargas Menocal

Notas<-read.csv("C:/Users/nicol/OneDrive/Escritorio/Lab2.Data.Science_N.Navarro_C.Ramirez_A.Vargas/student-por.csv")
View(Notas)


for (col in colnames(Notas)) {
  columna <- Notas[[col]]
  
  if (is.numeric(columna)) {
    
    media<-mean(Notas[[col]], na.rm = TRUE)#Media
    mediana<-median(Notas[[col]], na.rm = TRUE)#Mediana
    moda<-mfv(Notas[[col]])# Moda
    pd<-quantile(Notas[[col]], c(0.10));
    PV<- quantile(Notas[[col]],c(0.25))
    PC<- quantile(Notas[[col]],c(0.5));
    PS<- quantile(Notas[[col]],c(0.75));
    PCC<- quantile(Notas[[col]],c(1));
    MC<- mean(Notas[[col]], trim = 0.05)# Media cortada(5% de datos extremos descartados)
    desv<- sd(Notas[[col]]) # Desviación Estándar
    Rango<- range(Notas[[col]], na.rm = TRUE)# Rango
    Tabla<- table(Notas[[col]]) # Tabla de frecuencia
    
    #desviacion absoluta de la mediana
    med <- median(Notas[[col]])
    desv_Abs <- abs(Notas[[col]] - med)
    med_sd <- median(desv_Abs)
  
    cat("Columna",col,"\n")
    cat("Media:",media,"\n")
    cat("Mediana:",mediana,"\n")
    cat("Moda:",moda,"\n")
    cat("Percentil 10:",pd,"\n")
    cat("Percentil 25:",PV,"\n")
    cat("Percentil 50:",PC,"\n")
    cat("Percentil 75:",PS,"\n")
    cat("Percentil 100:",PCC,"\n")
    cat("Media cortada al 5%:",MC,"\n")
    cat("Desviacion estandar:", desv,"\n")
    cat("Rango:", Rango,"\n")
    cat("desviacion absoluta de la mediana:",med_sd,"\n")
    cat("Tabla:",Tabla,"\n")
    cat("\n")
  
  }
}

#Media Ponderada (usar características de la propia BD, es decir una atributo ponderado por otro)
weighted.mean(Notas$age, w= Notas$Walc) #Relacion entre el consumo de alcohol el fin de semana y la edad

# Mediana Ponderada (igual que 2)
median(rep(Notas$age, times = Notas$Walc)) #promedio ponderado entre la edad y el consumo de alcohol el fin de semana

# Error cuadrático Medio entre edades y consumo de alcohol los fin de semana
Dif<- (Notas$age - Notas$Walc)^2
ecm<- mean(Dif); ecm

# Boxplot entre características

Aux_datos <- list(Notas$Medu, Notas$Fedu, Notas$age)
boxplot(Aux_datos, names =c("Eduacion Madre","Educacion Padre","Edad Alumnos"))

Tabla_Edad <- table(Notas$age) # Tabla de frecuencia edad

View(Tabla_Edad)
# Histograma edad
hist(Notas$age,
     main = "Histograma de edades",
     xlab = "Edades",
     ylab = "Frecuencias")


barplot(prop.table(Tabla_Edad))# Gráfico de barra Edad


pie(Tabla_Edad, main= "Grafico de torta")#Grafico torta simples

# Coeficiente de correlación entre características o Matriz de correlación (con gráfico)
CEW<- cor(Notas$age, Notas$Walc); CEW
plot(Notas$age, Notas$Walc)# Gráfico de Dispersión entre características Edad y consumo alcohol el fin de semana

CED<- cor(Notas$age, Notas$Dalc); CED
plot(Notas$age, Notas$Dalc)# Gráfico de Dispersión entre características Edad y consumo alcohol el dia de semana


#Grafico de contornos

# Boxplot numérico por categoría
boxplot(Notas$Medu)
boxplot(Notas$age)
boxplot(Notas$Fedu)
boxplot(Notas$traveltime)
boxplot(Notas$studytime)
boxplot(Notas$failures)
boxplot(Notas$famrel)
boxplot(Notas$freetime)
boxplot(Notas$goout)
boxplot(Notas$Dalc)
boxplot(Notas$Walc)
boxplot(Notas$health)
boxplot(Notas$absences)
boxplot(Notas$G1)
boxplot(Notas$G2)
boxplot(Notas$G3)

# Gráfico de Violin por categoría 
as<- Notas$age
ggplot(Notas, aes(x= factor(1), y= as))+
  geom_violin()

asu<- Notas$Medu
ggplot(Notas, aes(x= factor(1), y= asu))+
  geom_violin()

asd<- Notas$Fedu
ggplot(Notas, aes(x= factor(1), y= asd))+
  geom_violin()

ast<- Notas$traveltime
ggplot(Notas, aes(x= factor(1), y= ast))+
  geom_violin()

asc<- Notas$studytime
ggplot(Notas, aes(x= factor(1), y= asc))+
  geom_violin()

ascc<- Notas$failures
ggplot(Notas, aes(x= factor(1), y= ascc))+
  geom_violin()

ass<- Notas$famrel
ggplot(Notas, aes(x= factor(1), y= ass))+
  geom_violin()

assss<- Notas$freetime
ggplot(Notas, aes(x= factor(1), y= assss))+
  geom_violin()

aso<- Notas$goout
ggplot(Notas, aes(x= factor(1), y= aso))+
  geom_violin()

asn<- Notas$Dalc
ggplot(Notas, aes(x= factor(1), y= asn))+
  geom_violin()

asud<- Notas$Walc
ggplot(Notas, aes(x= factor(1), y= asud))+
  geom_violin()

asut<- Notas$health
ggplot(Notas, aes(x= factor(1), y= asut))+
  geom_violin()

asuc<- Notas$absences
ggplot(Notas, aes(x= factor(1), y= asuc))+
  geom_violin()

asuci<- Notas$G1
ggplot(Notas, aes(x= factor(1), y= asuci))+
  geom_violin()

asus<- Notas$G2
ggplot(Notas, aes(x= factor(1), y= asus))+
  geom_violin()

asusi<- Notas$G3
ggplot(Notas, aes(x= factor(1), y= asusi))+
  geom_violin()

