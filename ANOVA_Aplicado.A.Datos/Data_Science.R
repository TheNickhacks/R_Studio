library(readxl)
library(readr)
library(ggplot2)
library(resumeRdesc)
library(modeest)
library(lmtest)
library(agricolae)
install.packages("psych")
library(psych)


#Nicolas Navarro Garrido
#Alexandra Vargas Menocal

Datos<-read_csv("C:/Users/nicol/OneDrive/Escritorio/Control.Data.Science/Pokemon.csv")
View(Datos)

# Item 1
numeroCOl<- ncol(Datos) 
for(col in 2:(numeroCOl)){
  Columna<-Datos[[col]]
  Nombre<- colnames(Datos)[col]
  
  if (is.numeric(Columna)){
    media<- mean(Columna, na.rm = TRUE); #Media
    mediana<- mean(Columna, na.rm = TRUE); #Mediana
    DesvE<- sd(x = Columna); #Desviacion Estandar
    Rango<- max(Columna) - min(Columna); #Rango
    CU<- quantile(Columna, probs = c(0.25)) #cuartil 25%
    CD<- quantile(Columna, probs = c(0.5)) #cuartil 50%
    CT<- quantile(Columna, probs = c(0.75)) #cuartil 75%
    
    cat("\n")
    cat("Columna",Nombre,"\n")
    cat("Media:",media,"\n")
    cat("Mediana:",mediana,"\n")
    cat("Desviacion Estandar:",DesvE,"\n")
    cat("Rango:",Rango,"\n")
    cat("Cuartil 1 (25%): ",CU,"\n")
    cat("Cuartil 2 (50%): ",CD,"\n")
    cat("Cuartil 3 (75%): ",CT,"\n")
  }
}
# Dispesion Vida v/s Ataque
plot(Datos$HP, Datos$Attack, xlab = "Vida Maxima", ylab = "Poder de Ataque", col = "red", main = "Dispersi??n Vida V/s Ataque")

#Histograma Puntos de Defensa
hist(Datos$Defense, xlab = "Valor", ylab = "Frecuencia", col = "green", main = "Histograma Puntos de Defensa")

#Grafico Barras Frecuencia de Tipo 1 de Pokemon
Frecuencia<- table(Datos$Type_1)
View(Frecuencia)
df_Frecuencia<- as.data.frame(Frecuencia)
colnames(df_Frecuencia)<- c("TipoUno","Frecuencia")


barplot(df_Frecuencia$Frecuencia, names.arg = df_Frecuencia$TipoUno, xlab = "Tipo", ylab = "Frecuencia", col = "blue", main = "Tipo 1 Pokemon V/s Recurrencia")




# Item 2
#Supuestos Normalidad Variable Dependiente, Homocedasticidad e Independencia de Observaciones
#Variables Categoricas Generales

VCU <-Datos$HP 
VCD <-Datos$Attack
VCT <-Datos$Defense
#Normalidad Categoricas Generales
A <-shapiro.test(VCU)
B <-shapiro.test(VCD)
C <-shapiro.test(VCT)

#Analisis Velocidad
cat("Test de Normalidad (Shapiro.test)")
AV <-Datos$Speed
#Normalidad Vd
D <-shapiro.test(AV)
A;B;C;D
#Homocedasticidad Prueba Breusch-Pagan
cat("Test de Homocedasticidad (Breusch-Pagan)")
HA <-bptest(AV ~ VCU);
HB <-bptest(AV ~ VCD);
HC <-bptest(AV ~ VCT);
HA;HB;HC

#Independencia Por Correlacion Pearson
cat("Test de Independencia (Correlacion Pearson)")
CA <-cor(AV, VCU, method = "pearson");
CB <-cor(AV, VCD, method = "pearson");
CC <-cor(AV, VCT, method = "pearson");
cat("Correlacion Pearson de Velocidad v/s Vida Maxima -->",CA)
cat("Correlacion Pearson de Velocidad v/s Ataque -->",CB)
cat("Correlacion Pearson de Velocidad v/s Defensa -->",CC)

# Crear el grC!fico de dispersiC3n
plot(VCU, AV,col = "red", main=" Velocidad v/s Vida Maxima con LC-nea de Tendencia", 
     xlab="Variable Independiente (VCU)", ylab="Variable Dependiente (AV)")
# Agregar una lC-nea de tendencia lineal
abline(lm(AV ~ VCU), col="Blue")

# Crear el grC!fico de dispersiC3n
plot(VCD, AV,col = "red", main=" Velocidad v/s Ataque con LC-nea de Tendencia", 
     xlab="Variable Independiente (VCD)", ylab="Variable Dependiente (AV)")
# Agregar una lC-nea de tendencia lineal
abline(lm(AV ~ VCD), col="Blue")

# Crear el grC!fico de dispersiC3n
plot(VCT, AV,col = "red", main=" Velocidad v/s Defensa con LC-nea de Tendencia", 
     xlab="Variable Independiente (VCT)", ylab="Variable Dependiente (AV)")
# Agregar una lC-nea de tendencia lineal
abline(lm(AV ~ VCT), col="Blue")


#Analisis Sp.Ataque
cat("Test de Normalidad (Shapiro.test)")
AD <-Datos$`Sp. Atk`
#Normalidad Vd
DU <-shapiro.test(AD)
A;B;C;DU
#Homocedasticidad Prueba Breusch-Pagan
cat("Test de Homocedasticidad (Breusch-Pagan)")
HAU <-bptest(AD ~ VCU);
HBU <-bptest(AD ~ VCD);
HCU <-bptest(AD ~ VCT);
HAU;HBU;HCU

#Independencia Por Correlacion Pearson
cat("Test de Independencia (Correlacion Pearson)")
CAU <-cor(AD, VCU, method = "pearson");
CBU <-cor(AD, VCD, method = "pearson");
CCU <-cor(AD, VCT, method = "pearson");
cat("Correlacion Pearson de Sp.Ataque v/s Vida Maxima -->",CAU)
cat("Correlacion Pearson de Sp.Ataque v/s Ataque -->",CBU)
cat("Correlacion Pearson de Sp.Ataque v/s Defensa -->",CCU)

# Crear el grC!fico de dispersiC3n
plot(VCU, AD,col = "red", main=" Sp.Ataque v/s Vida Maxima con LC-nea de Tendencia", 
     xlab="Variable Independiente (VCU)", ylab="Variable Dependiente (AD)")
# Agregar una lC-nea de tendencia lineal
abline(lm(AD ~ VCU), col="Blue")

# Crear el grC!fico de dispersiC3n
plot(VCD, AD,col = "red", main=" Sp.Ataque v/s Ataque con LC-nea de Tendencia", 
     xlab="Variable Independiente (VCD)", ylab="Variable Dependiente (AD)")
# Agregar una lC-nea de tendencia lineal
abline(lm(AD ~ VCD), col="Blue")

# Crear el grC!fico de dispersiC3n
plot(VCT, AD,col = "red", main=" Sp.Ataque v/s Defensa con LC-nea de Tendencia", 
     xlab="Variable Independiente (VCT)", ylab="Variable Dependiente (AD)")
# Agregar una lC-nea de tendencia lineal
abline(lm(AD ~ VCT), col="Blue")


#Analisis Sp.Def
cat("Test de Normalidad (Shapiro.test)")
AF <-Datos$`Sp. Def`
#Normalidad Vd
DD <-shapiro.test(AF)
A;B;C;DD
#Homocedasticidad Prueba Breusch-Pagan
cat("Test de Homocedasticidad (Breusch-Pagan)")
HAD <-bptest(AF ~ VCU);
HBD <-bptest(AF ~ VCD);
HCD <-bptest(AF ~ VCT);
HAD;HBD;HCD

#Independencia Por Correlacion Pearson
cat("Test de Independencia (Correlacion Pearson)")
CAD <-cor(AF, VCU, method = "pearson");
CBD <-cor(AF, VCD, method = "pearson");
CCD <-cor(AF, VCT, method = "pearson");
cat("Correlacion Pearson de Sp.Def v/s Vida Maxima -->",CAD)
cat("Correlacion Pearson de Sp.Def v/s Ataque -->",CBD)
cat("Correlacion Pearson de Sp.Def v/s Defensa -->",CCD)

# Crear el grC!fico de dispersiC3n
plot(VCU, AF,col = "red", main=" Sp.Defensa v/s Vida Maxima con LC-nea de Tendencia", 
     xlab="Variable Independiente (VCU)", ylab="Variable Dependiente (AF)")
# Agregar una lC-nea de tendencia lineal
abline(lm(AF ~ VCU), col="Blue")

# Crear el grC!fico de dispersiC3n
plot(VCD, AF,col = "red", main=" Sp.Defensa v/s Ataque con LC-nea de Tendencia", 
     xlab="Variable Independiente (VCD)", ylab="Variable Dependiente (AF)")
# Agregar una lC-nea de tendencia lineal
abline(lm(AF ~ VCD), col="Blue")

# Crear el grC!fico de dispersiC3n
plot(VCT, AF,col = "red", main=" Sp.Defensa v/s Defensa con LC-nea de Tendencia", 
     xlab="Variable Independiente (VCT)", ylab="Variable Dependiente (AF)")
# Agregar una lC-nea de tendencia lineal
abline(lm(AF ~ VCT), col="Blue")


#Filtrado de Pokemones por tipo (3 mas repetidos)
DataU <- subset(Datos,Type_1 == "Bug")
Datad <- subset(Datos,Type_1 == "Electric")
Datas <- subset(Datos,Type_1 == "Rock")
# Ver las filas filtradas
View(DataU)
View(Datad)
View(Datas)

#Analisis Correlacional por Tipo de Pokemon
#Tipo Bug
VB <-DataU$HP 
VBD <-DataU$Attack
VBT <-DataU$Defense
#Normalidad Categoricas Generales
AB <-shapiro.test(VB)
BB<-shapiro.test(VBD)
CB <-shapiro.test(VBT)

#Analisis Velocidad Pokemon Bug
cat("Test de Normalidad Pokemon Tipo Bug (Shapiro.test)")
ABV <-DataU$Speed
#Normalidad Vd
DB <-shapiro.test(ABV)
AB;BB;CB;DB
#Homocedasticidad Prueba Breusch-Pagan
cat("Test de Homocedasticidad Pokemon Tipo Bug (Breusch-Pagan)")
HAB <-bptest(ABV ~ VB);
HBB <-bptest(ABV ~ VBD);
HCB <-bptest(ABV ~ VBT);
HAB;HBB;HCB

#Independencia Por Correlacion Pearson
cat("Test de Independencia Pokemon Tipo Bug (Correlacion Pearson)")
CAB <-cor(ABV, VB, method = "pearson");
CBB <-cor(ABV, VBD, method = "pearson");
CCB <-cor(ABV, VBT, method = "pearson");
cat("Correlacion Pearson de Velocidad v/s Vida Maxima -->",CAB)
cat("Correlacion Pearson de Velocidad v/s Ataque -->",CBB)
cat("Correlacion Pearson de Velocidad v/s Defensa -->",CCB)

# Crear el grafico de dispersion
plot(VB, ABV,col = "red", main=" Velocidad v/s Vida Maxima Pokemon Tipo Bug", 
     xlab="Variable Independiente (VB)", ylab="Variable Dependiente (ABV)")
# Agregar una lC-nea de tendencia lineal
abline(lm(ABV ~ VB), col="Blue")

# Crear el grC!fico de dispersiC3n
plot(VBD, ABV,col = "red", main=" Velocidad v/s Ataque Pokemon Tipo Bug", 
     xlab="Variable Independiente (VBD)", ylab="Variable Dependiente (ABV)")
# Agregar una lC-nea de tendencia lineal
abline(lm(ABV ~ VBD), col="Blue")

# Crear el grC!fico de dispersiC3n
plot(VBT, ABV,col = "red", main=" Velocidad v/s Defensa Pokemon Tipo Bug", 
     xlab="Variable Independiente (VBT)", ylab="Variable Dependiente (ABV)")
# Agregar una lC-nea de tendencia lineal
abline(lm(ABV ~ VBT), col="Blue")



#Analisis Correlacional por Tipo de Pokemon
#Tipo Rock
VR <-Datas$HP 
VRD <-Datas$Attack
VRT <-Datas$Defense
#Normalidad Categoricas Generales
AR <-shapiro.test(VR)
BR <-shapiro.test(VRD)
CR <-shapiro.test(VRT)

#Analisis Velocidad Pokemon Rock
cat("Test de Normalidad Pokemon Tipo Rock (Shapiro.test)")
ARV <-Datas$Speed
#Normalidad Vd
DR <-shapiro.test(ARV)
AR;BR;CR;DR
#Homocedasticidad Prueba Breusch-Pagan
cat("Test de Homocedasticidad Pokemon Tipo Rock (Breusch-Pagan)")
HAR <-bptest(ARV ~ VR);
HBR <-bptest(ARV ~ VRD);
HCR <-bptest(ARV ~ VRT);
HAR;HBR;HCR

#Independencia Por Correlacion Pearson
cat("Test de Independencia Pokemon Tipo Rock (Correlacion Pearson)")
CAR <-cor(ARV, VR, method = "pearson");
CBR <-cor(ARV, VRD, method = "pearson");
CCR <-cor(ARV, VRT, method = "pearson");
cat("Correlacion Pearson de Velocidad v/s Vida Maxima -->",CAR)
cat("Correlacion Pearson de Velocidad v/s Ataque -->",CBR)
cat("Correlacion Pearson de Velocidad v/s Defensa -->",CCR)

# Crear el grafico de dispersion
plot(VR, ARV,col = "red", main=" Velocidad v/s Vida Maxima Pokemon Tipo Rock", 
     xlab="Variable Independiente (VR)", ylab="Variable Dependiente (ARV)")
# Agregar una lC-nea de tendencia lineal
abline(lm(ARV ~ VR), col="Blue")

# Crear el grC!fico de dispersiC3n
plot(VRD, ARV,col = "red", main=" Velocidad v/s Ataque Pokemon Tipo Rock", 
     xlab="Variable Independiente (VRD)", ylab="Variable Dependiente (ARV)")
# Agregar una lC-nea de tendencia lineal
abline(lm(ARV ~ VRD), col="Blue")

# Crear el grC!fico de dispersiC3n
plot(VRT, ARV,col = "red", main=" Velocidad v/s Defensa Pokemon Tipo Rock", 
     xlab="Variable Independiente (VRT)", ylab="Variable Dependiente (ARV)")
# Agregar una lC-nea de tendencia lineal
abline(lm(ARV ~ VRT), col="Blue")


#Analisis Correlacional por Tipo de Pokemon
#Tipo Electric
VE <-Datad$HP 
VED <-Datad$Attack
VET <-Datad$Defense
#Normalidad Categoricas Generales
AE <-shapiro.test(VE)
BE <-shapiro.test(VED)
CE <-shapiro.test(VET)


#Analisis Velocidad Pokemon Rock
cat("Test de Normalidad Pokemon Tipo Electric (Shapiro.test)")
AEV <-Datad$Speed
#Normalidad Vd
DE <-shapiro.test(AEV)
AE;BE;CE;DE
#Homocedasticidad Prueba Breusch-Pagan
cat("Test de Homocedasticidad Pokemon Tipo Electric (Breusch-Pagan)")
HAE <-bptest(AEV ~ VE);
HBE <-bptest(AEV ~ VED);
HCE <-bptest(AEV ~ VET);
HAE;HBE;HCE

#Independencia Por Correlacion Pearson
cat("Test de Independencia Pokemon Tipo Electric (Correlacion Pearson)")
CAE <-cor(AEV, VE, method = "pearson");
CBE <-cor(AEV, VED, method = "pearson");
CCE <-cor(AEV, VET, method = "pearson");
cat("Correlacion Pearson de Velocidad v/s Vida Maxima -->",CAE)
cat("Correlacion Pearson de Velocidad v/s Ataque -->",CBE)
cat("Correlacion Pearson de Velocidad v/s Defensa -->",CCE)

# Crear el grafico de dispersion
plot(VE, AEV,col = "red", main=" Velocidad v/s Vida Maxima Pokemon Tipo Electric", 
     xlab="Variable Independiente (VE)", ylab="Variable Dependiente (AEV)")
# Agregar una lC-nea de tendencia lineal
abline(lm(AEV ~ VE), col="Blue")

# Crear el grC!fico de dispersiC3n
plot(VED, AEV,col = "red", main=" Velocidad v/s Ataque Pokemon Tipo Electric", 
     xlab="Variable Independiente (VED)", ylab="Variable Dependiente (AEV)")
# Agregar una lC-nea de tendencia lineal
abline(lm(AEV ~ VED), col="Blue")

# Crear el grC!fico de dispersiC3n
plot(VET, AEV,col = "red", main=" Velocidad v/s Defensa Pokemon Tipo Electric", 
     xlab="Variable Independiente (VET)", ylab="Variable Dependiente (AEV)")
# Agregar una lC-nea de tendencia lineal
abline(lm(AEV ~ VET), col="Blue")




#Item 3
#Analisis ANOVA
Modelo <- aov(HP ~ Type_1, data = Datos) #VIDA MAXIMA
cat("Analisis ANOVA Sobre la Vida Maxima, Respecto al Tipo de Pokemon")
summary(Modelo)

ModeloU <- aov(Attack ~ Type_1, data = Datos) #ATAQUE
cat("Analisis ANOVA Sobre el Ataque, Respecto al Tipo de Pokemon")
summary(ModeloU)

ModeloD <- aov(Defense ~ Type_1, data = Datos) #DEFENSA
cat("Analisis ANOVA Sobre la Defensa, Respecto al Tipo de Pokemon")
summary(ModeloD)

ModeloT <- aov(Speed ~ Type_1, data = Datos) #VELOCIDAD
cat("Analisis ANOVA Sobre la Velocidad, Respecto al Tipo de Pokemon")
summary(ModeloT)

#Pruebas de Tukey

# Realizar la prueba de Tukey
tukey_test <- HSD.test(Modelo, "Type_1", group = TRUE)

# Ver los resultados de la prueba
print(tukey_test)

# Mostrar un gr??fico de los resultados
plot(tukey_test, main = "Grafico de Grupos de Medias", xlab = "Tipos Pokemon", ylab = "Valor Media")




#Item 4
#Analisis Factorial Exploratorio

columnas_numericas <- Datos[, sapply(Datos, is.numeric)]

# Muestra el resultado
View(columnas_numericas)

#Linealidad
cat("Linealidad y Aditividad")
Correlacion <- cor(columnas_numericas)
Correlacion

#Matriz Covarianza 
Cova <-cov(columnas_numericas)
Cova

#Analisis de Horn
Valores <- fa.parallel(Cova, fm = "pa", fa = "fa", n.iter = 1000)
print(Valores)


#Filtrado de Pokemones por tipo (3 mas repetidos)
Datau <- subset(Datos,Type_1 == "Bug")
DataD <- subset(Datos,Type_1 == "Electric")
DataT <- subset(Datos,Type_1 == "Rock")
#Eliminar no numericas
BugNumerico <- Datau[, sapply(Datau, is.numeric)]
ElecNumerico <- DataD[, sapply(DataD, is.numeric)]
RockNumerico <- DataT[, sapply(DataT, is.numeric)]

View(BugNumerico)
View(ElecNumerico)
View(RockNumerico)

CovB <-cov(BugNumerico)
CovB
ValoresB <- fa.parallel(CovB, fm = "pa", fa = "fa", n.iter = 1000)
print(ValoresB)


CovE <-cov(ElecNumerico)
CovE
ValoresE <- fa.parallel(CovE, fm = "pa", fa = "fa", n.iter = 1000)
print(ValoresE)

CovR <-cov(RockNumerico)
CovR
ValoresR <- fa.parallel(CovR, fm = "pa", fa = "fa", n.iter = 1000)
print(ValoresR)
