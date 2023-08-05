#Nicolas Navarro

library(readxl)
chile <- read_excel("C:/Users/C403-13/Downloads/chile.xlsx")
View(chile)
library(summary)
install.packages("summarytools")

library(ggplot2)
install.packages('ggplot2')

IQR(chile$Perc.Income)
#Diferencia entre el tercer cuartil(75%) y primer cuartil(25%), es decir
#corresponde a la mediana de los datos utilizados

kurtosis(chile$Perc.Income)
#Kurtosis corresponde a la simetria de los datos, estableciendo que 
# el apuntalamiento es Leptocurtica, ya que tiene un valor de 17.6904
#Lo que significa que los datos estan sobrecargados mas en una zona que en la
#siguiente, por lo que hace que sea una distribucion asimetrica
ggplot(data = chile) +
  geom_histogram(mapping = aes(x = Perc.Income), bins = 20, fill = 'darkred')+
  labs(title='Histograma Ingresos por comuna',
       x='ingresos por comuna', 
       y='Frecuencia acumulada')
skewness(chile$Perc.Income)
#La medida de skewness tiene un valor de 3.9636, lo que hace que sea una distribucion
#Extendida hacia la derecha, debido a que existen comunas que tienen ingresos superiores
#a la media del los ingresos por comuna

ggplot(data = chile) + 
  geom_boxplot(mapping = aes(y = Perc.Income), fill = 'red')+ 
               labs(title='Boxplot de ingresos por comunas',
               y='ingresos comuna')

summary(chile$Perc.Income)
#median(chile$Perc.Income)
