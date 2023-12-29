library(readxl)
library(readr)
library(ggplot2)
library(resumeRdesc)
library(modeest)
library(lmtest)

#Nicolas Navarro Garrido
#Carlos Ramirez Vallejos
#Alexandra Vargas Menocal

Notas<-read.csv("C:/Users/nicol/OneDrive/Escritorio/Lab3.Data.Science_N.Navarro_C.Ramirez_A.Vargas/student-por.csv")
View(Notas)

#Variable Categoricas Generales
VCu <- Notas$famrel
VCd <- Notas$freetime
VCt <- Notas$goout


#Analisis Edad
Edad <- Notas$age
#Normalidad Variable Dependiente
a<-shapiro.test(Edad)
b<-shapiro.test(VCu)
c<-shapiro.test(VCd)
d<-shapiro.test(VCt)
a;b;c;d
#Analisis Homocedasticidad  Prueba de Breusch-Pagan
resultadou <- bptest(Edad ~ VCu); resultadou 
resultadoud <- bptest(Edad ~ VCd); resultadoud
resultadout <- bptest(Edad ~ VCd); resultadout
#Independencia de Observaciones Por correlacion de Pearson
correlationu <- cor(Edad, VCu, method = "pearson");correlationu
correlationd <- cor(Edad, VCd, method = "pearson");correlationd
correlationt <- cor(Edad, VCt, method = "pearson");correlationt


#Analisis Eduacion Madre
EducacionM <- Notas$Medu
#Normalidad Variable Dependiente
ad<-shapiro.test(EducacionM)
b<-shapiro.test(VCu)
c<-shapiro.test(VCd)
d<-shapiro.test(VCt)
ad;b;c;d
#Analisis Homocedasticidad  Prueba de Breusch-Pagan
resultadouu <- bptest(EducacionM ~ VCu); resultadouu
resultadodd <- bptest(EducacionM ~ VCd); resultadodd
resultadodt <- bptest(EducacionM ~ VCd); resultadodt
#Independencia de Observaciones Por correlacion de Pearson
correlationud <- cor(EducacionM, VCu, method = "pearson");correlationud
correlationdd <- cor(EducacionM, VCd, method = "pearson");correlationdd
correlationtd <- cor(EducacionM, VCt, method = "pearson");correlationtd

#Analisis Notas 1
NotasGu <- Notas$G1
#Normalidad Variable Dependiente
at<-shapiro.test(NotasGu)
b<-shapiro.test(VCu)
c<-shapiro.test(VCd)
d<-shapiro.test(VCt)
at;b;c;d
#Analisis Homocedasticidad  Prueba de Breusch-Pagan
resultadotu <- bptest(NotasGu ~ VCu); resultadotu 
resultadotd <- bptest(NotasGu ~ VCd); resultadotd
resultadott <- bptest(NotasGu ~ VCd); resultadott
#Independencia de Observaciones Por correlacion de Pearson
correlationut <- cor(NotasGu, VCu, method = "pearson");correlationut
correlationdt <- cor(NotasGu, VCd, method = "pearson");correlationdt
correlationtt <- cor(NotasGu, VCt, method = "pearson");correlationtt


#Analisis Notas 2
NotasGd <- Notas$G2
#Normalidad Variable Dependiente
ac<-shapiro.test(NotasGd)
b<-shapiro.test(VCu)
c<-shapiro.test(VCd)
d<-shapiro.test(VCt)
ac;b;c;d
#Analisis Homocedasticidad  Prueba de Breusch-Pagan
resultadocu <- bptest(NotasGd ~ VCu); resultadocu
resultadocd <- bptest(NotasGd ~ VCd); resultadocd
resultadoct <- bptest(NotasGd ~ VCd); resultadoct
#Independencia de Observaciones Por correlacion de Pearson
correlationuc <- cor(NotasGd, VCu, method = "pearson");correlationuc
correlationdc <- cor(NotasGd, VCd, method = "pearson");correlationdc
correlationtc <- cor(NotasGd, VCt, method = "pearson");correlationtc

#Analisis Notas 3
NotasGt <- Notas$G3
#Normalidad Variable Dependiente
as<-shapiro.test(NotasGt)
b<-shapiro.test(VCu)
c<-shapiro.test(VCd)
d<-shapiro.test(VCt)
as;b;c;d
#Analisis Homocedasticidad  Prueba de Breusch-Pagan
resultadosu <- bptest(NotasGt ~ VCu); resultadosu 
resultadosd <- bptest(NotasGt ~ VCd); resultadosd
resultadost <- bptest(NotasGt ~ VCd); resultadost
#Independencia de Observaciones Por correlacion de Pearson
correlationus <- cor(NotasGt, VCu, method = "pearson");correlationus
correlationds <- cor(NotasGt, VCd, method = "pearson");correlationds
correlationts <- cor(NotasGt, VCt, method = "pearson");correlationts
