#Estadistica descriptiva

#Usar para los archivos R Script

#Las librerias se instalan igual que python con pip3 install

#Para ejecutar el programa, es por linea y se ejecuta con las teclas "Ctrl+Enter"

#Instalar paquete que sirve para leer archivos excel
install.packages('readxl')

#Activar la libreria instalada
library(readxl)


#Getwd()
#Comando para saber la ruta donde esta guardado el archivo

#Setwd('Direccion')
#Se usa para cambiar el directorio de trabajo del archivo, tiene que ser la direccion de la BBDD
# Los \ deben ser cambiados por /

#Leer base de datos
data <- read_excel('Particulas_Contaminantes.xlsx')

#read_excel solo lee archivos .xlsx

#Aqui en R, para asignar valor en vez de utilizar "=", se utiliza el operador "<-"

head(data)
#Comando para mirar la base de datos, en la consola

View(data)
#Para mirar la base de datos, desde una ventana externa


#--------------------------------------------------------------------------
#Como cargar un archivo .csv

#Nos vamos a environment, import Dataset, opcion "From text(base),la buscamos y luego se abre y se importa"
#para guardarla en el script, nos vamos a history y aparecerá lo que hicimos pero en codigo y luego se usara el termino "To source"


#Codigo guardado en history
brain_stroke <- read.csv("C:/Users/C403-10.C403-10-PC/Downloads/brain_stroke.csv")

#La entrega debe ser asi : brain_stroke <- read.csv("brain_stroke.csv")
# Y hacer el posterior cambio de directorio como se hace con Setwd('Direccion Archivo.csv')
View(brain_stroke)

#DataScienceJobs <- read.csv("C:/Users/C403-10.C403-10-PC/Downloads/DataScienceJobs.csv", comment.char="#")


DataScienceJobs <- read.csv("DatascienceJobs.csv")

View(DataScienceJobs)

