library(readxl)
library(readr)
library(ggplot2)
library(resumeRdesc)
library(plotly)
library(nortest)

#Nicolas Navarro Garrido
#Alexandra Vargas Menocal
#Carlos Ramirez Vallejos

Datos<-read_xlsx("C:/Users/nicol/OneDrive/Escritorio/Lab.2.Inferencia_Navarro_Vargas_Ramirez/arriendo-1.xlsx")
View(Datos)

#Analisis Exploratorio
#Analisis Uni-Variable M2
mediaM<- mean(Datos$m2, na.rm = TRUE)
medianaM<- median(Datos$m2, na.rm = TRUE)
minimoM<- min(Datos$m2)
maximoM<- max(Datos$m2)
cuartilUM<- quantile(Datos$m2, probs = c(0.25))
cuartilDM<- quantile(Datos$m2, probs = c(0.5))
cuartilTM<- quantile(Datos$m2, probs = c(0.75))

if(is.numeric(Datos$m2)){
  cat("Analisis Exploratorio de la variable -> M2 \n")
  cat("Media Aritmetica ->",mediaM,"\n")
  cat("Mediana ->",medianaM,"\n")
  cat("Minimo ->",minimoM,"\n")
  cat("Maximo ->",maximoM,"\n")
  cat("Cuartil 25% ->",cuartilUM,"\n")
  cat("Cuartil 50% ->",cuartilDM,"\n")
  cat("Cuartil 75% ->",cuartilTM,"\n")
  cat("\n")
}

#Analisis Uni-Variable Precio
mediaP<- mean(Datos$Precio, na.rm = TRUE)
medianaP<- median(Datos$Precio, na.rm = TRUE)
minimoP<- min(Datos$Precio)
maximoP<- max(Datos$Precio)
cuartilUP<- quantile(Datos$Precio, probs = c(0.25))
cuartilDP<- quantile(Datos$Precio, probs = c(0.5))
cuartilTP<- quantile(Datos$Precio, probs = c(0.75))

if(is.numeric(Datos$Precio)){
  cat("Analisis Exploratorio de la variable -> Precio \n")
  cat("Media Aritmetica ->",mediaP,"\n")
  cat("Mediana ->",medianaP,"\n")
  cat("Minimo ->",minimoP,"\n")
  cat("Maximo ->",maximoP,"\n")
  cat("Cuartil 25% ->",cuartilUP,"\n")
  cat("Cuartil 50% ->",cuartilDP,"\n")
  cat("Cuartil 75% ->",cuartilTP,"\n")
  cat("\n")
}

#Analisis Uni-Variable Distancia
mediaD<- mean(Datos$distancia, na.rm = TRUE)
medianaD<- median(Datos$distancia, na.rm = TRUE)
minimoD<- min(Datos$distancia)
maximoD<- max(Datos$distancia)
cuartilUD<- quantile(Datos$distancia, probs = c(0.25))
cuartilDD<- quantile(Datos$distancia, probs = c(0.5))
cuartilTD<- quantile(Datos$distancia, probs = c(0.75))

if(is.numeric(Datos$distancia)){
  cat("Analisis Exploratorio de la variable -> Distancia \n")
  cat("Media Aritmetica ->",mediaD,"\n")
  cat("Mediana ->",medianaD,"\n")
  cat("Minimo ->",minimoD,"\n")
  cat("Maximo ->",maximoD,"\n")
  cat("Cuartil 25% ->",cuartilUD,"\n")
  cat("Cuartil 50% ->",cuartilDD,"\n")
  cat("Cuartil 75% ->",cuartilTD,"\n")
  cat("\n")
}

#Graficos Variables Numericas Continuas

Datou<- hist(Datos$m2,
             main = "Histograma de Metros Cuadrados",
             xlab = "Valores",
             ylab = "Frecuencia",
             col = "green",
             border = "purple",)

Datod<- hist(Datos$Precio,
             main = "Histograma del Precio",
             xlab = "Valores",
             ylab = "Frecuencia",
             col = "green",
             border = "purple",) 

Datod<- hist(Datos$distancia,
             main = "Histograma de la Distancia",
             xlab = "Valores",
             ylab = "Frecuencia",
             col = "green",
             border = "purple",) 

#Variable Discreta
TablaD <- table(Datos$Categoria)
dataF <- data.frame(TablaD)
colnames(dataF) <- c("Zonas Ciudad","Frecuencia")
View(dataF)

Barras <-barplot(TablaD, main="Lado de la Ciudad", 
                 xlab = "A para Sector 1 y B para Sector 2",
                 ylab = "Frecuencia",
                 col = "green",
                 border = "purple")  

#Analisis Bi-Variable M2 v/s Precio
Correlacion_Pearsonu <- cor(Datos$m2, Datos$Precio)
if(is.numeric(Datos$m2)){
  cat("Analisis Exploratorio Bivariable M2 V/S Precio \n")
  cat("Correlacion entre Variable M2 y Precio ->",Correlacion_Pearsonu,"\n")
}

plot(Datos$m2, Datos$Precio,
     main = "Grafico Dispersion Metros Cuadrados v/s Precio",
     xlab = "Metros Cuadrados",
     ylab = "Precio",
     col = "purple")

#Analisis Bi-Variable M2 v/s Distancia
Correlacion_PearsonD <- cor(Datos$m2, Datos$distancia)
if(is.numeric(Datos$distancia)){
  cat("Analisis Exploratorio Bivariable M2 V/S Distancia \n")
  cat("Correlacion entre Variable M2 y Distancia ->",Correlacion_PearsonD,"\n")
}

plot(Datos$m2, Datos$distancia,
     main = "Grafico Dispersion Metros Cuadrados v/s Distancia",
     xlab = "Metros Cuadrados",
     ylab = "Distancia",
     col = "purple")

#Analisis Bi-Variable Distancia v/s Precio
Correlacion_PearsonT <- cor(Datos$distancia, Datos$Precio)
if(is.numeric(Datos$Precio)){
  cat("Analisis Exploratorio Bivariable Distancia V/S Precio \n")
  cat("Correlacion entre Variable Distancia y Precio ->",Correlacion_PearsonT,"\n")
}

plot(Datos$distancia, Datos$Precio,
     main = "Grafico Dispersion Distancia v/s Precio",
     xlab = "Distancia",
     ylab = "Precio",
     col = "purple")


#Planteamiento Modelo Regresion Lineal

#Modelo Regresi??n Lineal -> Precio = m2 + distancia

Modelo <- lm(Precio ~ m2 + distancia, data = Datos) #Modelo Ajustado por funcion lm

#Variable Dependiente Datos$Precio
#Variables Independendientes Datos$m2 y Datos$distancia

Datos_Modelo<- summary(Modelo)

#Grafico 3D sobre los datos utilizados en el modelo de regresion

Dispersion_3D <- ggplot(Datos, aes(x = m2, y = Precio, z = distancia)) +
  geom_point(aes(color = m2), size = 3) +
  labs(
    title = "Metros Cuadrados v/s Precio v/s Distancia",
    x = "Metros Cuadrados",
    y = "Precio",
    z = "Distancia"
  )
Grafico_Interactivo <- ggplotly(Dispersion_3D ); Grafico_Interactivo



#Datos para predecir con regresion lineal
TablaPrediccion <- data.frame(m2= Datos$m2, distancia= Datos$distancia)
View(TablaPrediccion)

#Prediccion
Predicciones <- predict(Modelo, newdata = TablaPrediccion)
TablaPrediccion$Predicciones <-predict(Modelo, newdata = TablaPrediccion)
View(TablaPrediccion)

Dispersion_Prediccion <- ggplot(TablaPrediccion, aes(x = m2, y = Predicciones, z = distancia)) +
  geom_point(aes(color = m2), size = 3) +
  labs(
    title = "Metros Cuadrados v/s Precio_Predecido v/s Distancia",
    x = "Metros Cuadrados",
    y = "Precio_Predecido",
    z = "Distancia"
  )
Grafico_Prediccion <- ggplotly(Dispersion_Prediccion ); Grafico_Prediccion

#Coeficiente de Determinacion lineal del modelo lm(Precio ~ m2 + distancia, data = Datos)

Coeficiente_Dl <- Datos_Modelo$r.squared
if(is.numeric(Datos$m2)){
  cat("Modelo = Precio ~ m2 + distancia \n")
  cat("El Coeficiente de Determinacion lineal del Modelo es: ", Coeficiente_Dl)
}

#Grafico Cuantil-Cuantil
#Vector de Datos
Vu <- TablaPrediccion$m2
Vd <- TablaPrediccion$distancia
Vt <- TablaPrediccion$Predicciones

# Generar el gr??fico Q-Q para m2
qqnorm(Vu)
qqline(Vu)
#Test de Shapiro-Wilk y Anderson-Darling para m2
shapiro.test(TablaPrediccion$m2)
ad.test(TablaPrediccion$m2)

# Generar el gr??fico Q-Q para distancia
qqnorm(Vd)
qqline(Vd)
#Test de Shapiro-Wilk y Anderson-Darling para distancia
shapiro.test(TablaPrediccion$distancia)
ad.test(TablaPrediccion$distancia)

# Generar el gr??fico Q-Q para Precio_Predictivo
qqnorm(Vt)
qqline(Vt)
#Test de Shapiro-Wilk y Anderson-Darling para Precio_Predictivo
shapiro.test(TablaPrediccion$Predicciones)
ResultadosTest <-ad.test(TablaPrediccion$Predicciones); ResultadosTest

#Test de hipotesis para ver si la variables predichas distribuyen normal
#Confianza del 95% 
# Define la significancia 1 - Confianza -> 0.05

Significancia <- 0.05

# Analiza el resultado
if (ResultadosTest$p.value < Significancia) {
  cat("Se rechaza la hip??tesis nula (H0). Los datos no siguen una distribuci??n normal.\n")
} else {
  cat("No se rechaza la hip??tesis nula (H0). Los datos pueden seguir una distribuci??n normal.\n")
  cat("El p-value es de: ",ResultadosTest$p.value,"\n")
}

#Test de Hipotesis para 8 y 9

PruebaHipo <- t.test(TablaPrediccion$Predicciones, mu = 970)
cat("Resultado del test de hip??tesis para la media:\n"); print(PruebaHipo)

#5% de Significancia

if (PruebaHipo$p.value < 0.05) {
  cat("Se rechaza la hip??tesis nula. Hay evidencia de que la media es diferente de 970.\n")
} else {
  cat("No se rechaza la hip??tesis nula. No hay suficiente evidencia para afirmar que la media es diferente de 970.\n")
}


TablaAux <- Datos[Datos$Categoria == "A", ]
View(TablaAux)

TablaAuxd <- Datos[Datos$Categoria == "B", ]
View(TablaAuxd)

PruebaHipotesis <- t.test(TablaAux$Precio, TablaAuxd$Precio, alternative = "two.sided")
print(PruebaHipotesis)

Significanciados<- 0.05
if (PruebaHipotesis$p.value <= Significanciados) {
  cat("Se rechaza la hip??tesis nula. Hay evidencia de diferencia entre las medias.\n")
  cat("\n")
  print(PruebaHipotesis)
} else {
  cat("No se rechaza la hip??tesis nula. No hay evidencia de diferencia entre las medias.\n")
  cat("\n")
  print(PruebaHipotesis)
}
