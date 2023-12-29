library(readxl)
library(readr)
library(ggplot2)
library(resumeRdesc)
library(modeest)
library(lmtest)

#Nicolas Navarro Garrido
#Carlos Ramirez Vallejos
#Alexandra Vargas Menocal

#Se pasaron los datos a un archivo con formato xlsx, debido a problemas para abrir el
# archivo original.
Datos<-read_xlsx("C:/Users/nicol/OneDrive/Escritorio/Proyecto.Inferencia_GrupoN4/PROVLOGISUNO.xlsx")
View(Datos)


#Analisis estadistico para una variable

#For se salta la primera y la ultima columna, debido a que la primera
# no entrega informacion relevante, mientras que la ultima debe de realizarse una 
#tabla de frecuencia, ya que es una variable numerica, del tipo discreta.

Numero_Columnas <- ncol(Datos)
cat("Analisis estadistico para variables Numericas Cuantitativas")
for (i in 2:(Numero_Columnas-1)){
  columna <- Datos[[i]]
  Nombre <- colnames(Datos)[i]
  
  if (is.numeric(columna)){
    media<- mean(columna, na.rm = TRUE) #Media
    mediana<- median(columna, na.rm = TRUE) #Mediana
    moda<- mfv(columna) #Moda
    minimo<-min(columna) #minimo
    maximo<-max(columna) #maximo
    CuartirUno<-quantile(columna, probs = c(0.25)) #cuartil 25%
    CuartirDos<-quantile(columna, probs = c(0.5)) #cuartil 50%
    CuartirTres<-quantile(columna, probs = c(0.75)) #cuartil 75%
    
    cat("Columna ->",Nombre,"\n")
    cat("Media Aritmetica ->",media,"\n")
    cat("Mediana ->",mediana,"\n")
    cat("Moda ->",moda,"\n")
    cat("Minimo ->",minimo,"\n")
    cat("Maximo ->",maximo,"\n")
    cat("Cuartil 25% ->",CuartirUno,"\n")
    cat("Cuartil 50% ->",CuartirDos,"\n")
    cat("Cuartil 75% ->",CuartirTres,"\n")
    cat("\n")
  }
}

#Graficos Variables numericas continuas

Datou<- hist(Datos$Calidad,
             main = "Histograma de Calidad",
             xlab = "Valores",
             ylab = "Frecuencia",
             col = "green",
             border = "purple",)

Datod<- hist(Datos$agilidadentrega,
             main = "Histograma de la Agilidad de la Entrega",
             xlab = "Valores",
             ylab = "Frecuencia",
             col = "green",
             border = "purple",) 

Datot<- hist(Datos$flexpago,
             main = "Histograma de la Flexibilidad de Pago",
             xlab = "Valores",
             ylab = "Frecuencia",
             col = "green",
             border = "purple",) 

Datoc<- hist(Datos$AtencionCH,
             main = "Histograma de la Atencion del Personal",
             xlab = "Valores",
             ylab = "Frecuencia",
             col = "green",
             border = "purple",) 

Datocc<- hist(Datos$imagen,
              main = "Histograma de la Imagen Percibida de la Empresa",
              xlab = "Valores",
              ylab = "Frecuencia",
              col = "green",
              border = "purple",) 


#Boxplot

boxplot(Datos$Calidad, main = "Boxplot de Calidad", ylab = "Valores")
boxplot(Datos$agilidadentrega, main = "Boxplot de Agilidad en Entrega", ylab = "Valores")
boxplot(Datos$flexpago, main = "Boxplot de Flexibilidad de Pago", ylab = "Valores")
boxplot(Datos$AtencionCH, main = "Boxplot de Atencion al Cliente", ylab = "Valores")
boxplot(Datos$imagen, main = "Boxplot de Imagen de Empresa", ylab = "Valores")
#Analisis Variable numerica discreta

Tabla <- table(Datos$clientepref)
View(Tabla)
Barras <-barplot(Tabla, main="Clientes Preferenciales y No Preferenciales", 
                 xlab = "1 Si es Preferencial, 2 Si No es Preferencial",
                 ylab = "Frecuencia",
                 col = "green",
                 border = "purple")  


#Analisis para Dos Variables

# 3
#Relacion entre variables que describen de mejor manera la calidad del servicio
# Se utilizaran las variables de  IP y AP, sin tomar en cuenta la variable
#de la imagen percibida por los clientes, debido a que se usara en otro analisis posterior

Correlacion_Pearsonu <- cor(Datos$Calidad, Datos$AtencionCH)
Correlacion_Pearsonu

Correlacion_Pearsond <- cor(Datos$Calidad, Datos$imagen)
Correlacion_Pearsond


#4
#Grafico Dispersion entre calidad, IP y AP

plot(Datos$Calidad, Datos$imagen,
     main = "Grafico Dispersion Calidad v/s Imagen Percibida",
     xlab = "Imagen Percibida",
     ylab = "Calidad",
     col = "purple")

plot(Datos$Calidad, Datos$AtencionCH,
     main = "Grafico Dispersion Calidad v/s Atencion del Personal",
     xlab = "Calidad",
     ylab = "Atencion del Personal",
     col = "purple")

#Se eligio la variable de Atencion al Cliente
modeloLineal <- lm( Calidad ~ AtencionCH, data = Datos); modeloLineal

#Representacion Algebraica

#Calidad = 1.7106 + (0.6186* AtencionCH)

#Representacion Grafica
plot(Datos$Calidad, Datos$AtencionCH,
     main = "Grafico Dispersion Calidad v/s Atencion del Personal",
     xlab = "Atencion del Personal",
     ylab = "Calidad",
     col = "purple")
abline(modeloLineal, col = "red")

#Coeficiente de Determinacion Lineal
CoefDL <- summary(modeloLineal)$r.square;
cat("El Coeficiente de Determinacion Lineal es ->", CoefDL,"\n")

#Tabla ANOVA
Tabla_Modelo <- anova(modeloLineal); Tabla_Modelo
summary(Tabla_Modelo)

#Ajuste Modelo ANOVA
# Ajusta el modelo ANOVA
MOdelo_Ajustado <- aov(Calidad ~ AtencionCH, data = Datos);MOdelo_Ajustado

# Realiza el an??lisis de hip??tesis para el ANOVA
Resultado_Ajuste <- summary(MOdelo_Ajustado)

# Imprime el resumen del an??lisis de varianza
print(Resultado_Ajuste)

Valor_P <- 6.05e-09
Significancia_Utilizada <- 0.05 #(5%)
# Compara el valor p con el nivel de significancia
if (Valor_P < Significancia_Utilizada) {
  cat("Rechazamos la Hip??tesis Nula. Hay Diferencias Significativas Entre los Coeficientes.\n")
} else {
  cat("No Rechazamos la Hip??tesis Nula. No Hay Diferencias Significativas Entre los Coeficientes.\n")
}

#Parte 3 (Final)
#Representacion Algebraica

#Calidad = 1.7106 + (0.6186* AtencionCH)
#Beta(0) = 1.7106
#Beta(1) = 0.6186

#Test de Hipotesis par Beta(0)
#H0 = Beta(0) = 0
#H1 = Beta(0) != 0

#Test de Hipotesis par Beta(1)
#H0 = Beta(1) = 0
#H1 = Beta(1) != 0

#Shapiro.test para el modelo ajustado
residual_M.Ajuste <- residuals(MOdelo_Ajustado); residual_M.Ajuste
Testing <- shapiro.test(residual_M.Ajuste); Testing

Sign.test <- 0.05

if (Testing$p.value < Sign.test) {
  print("Rechazamos la hip??tesis nula. Los datos no siguen una distribuci??n normal.")
} else {
  print("No podemos rechazar la hip??tesis nula. Los datos siguen una distribuci??n normal.")
}

#supuesto de homocedasticidad
TestingBp <- bptest(MOdelo_Ajustado); TestingBp
Sign.test <- 0.05 #Misma en todos los test de hipotesis asociados al proyecto

if (TestingBp$p.value < Sign.test) {
  print("Rechazamos la hip??tesis nula. Hay evidencia de heterocedasticidad.")
} else {
  print("No podemos rechazar la hip??tesis nula, por falta de informacion pertinente")
}

#variable Dummy

# 1 si es preferente = 1
# 2 si no es preferente = 0
Dummy <- ifelse(Datos$clientepref == 1, 1, 0); Dummy

Ml_ConD <- lm(Calidad ~ AtencionCH + Dummy, data = Datos) #Modelo con Variable Dummy
Aux_dum <- summary(Ml_ConD); Aux_dum
Valor.ptes <- Aux_dum$coefficients["Dummy", "Pr(>|t|)"]; Valor.ptes
if (Valor.ptes < Sign.test) {
  print("Rechazamos la hip??tesis nula. Hay evidencia de que el coeficiente es significativo")
} else {
  print("No podemos rechazar la hip??tesis nula, por falta de informacion pertinente")
}

#Agregar Variable al modelo
#lm(Calidad ~ AtencionCH + Dummy, data = Datos) #Modelo con Variable Dummy

#Modelo con variable extra
Ml.ve <- lm(Calidad ~ AtencionCH + Dummy + agilidadentrega, data = Datos)
Aux_Mlv <- summary(Ml.ve); Aux_Mlv
