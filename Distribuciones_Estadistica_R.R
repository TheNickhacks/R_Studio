
#Nicolas Navarro

#1)

#Cantidad total de estudiantes
CasosT = 2000000

#Cantidad total de personas favorecidas dentro del 2%
CasosFav=2000000*0.02

#Probabilidad del exito (de que las personas discapacitadas reciban trato especial)
Pexi= CasosFav/CasosT

#Muestra la probabilidad
print(Pexi)

#Cantidad de la muestra seleccionada
n = 25

#1) A X==x, con x==1
#Calcula la distribucion biomial cuando X tiende a 1
pbinom(1,n,Pexi)


#1) B X>=x, con x==1

#El dominio de la distribucion de poisson es X>0

#Calcula la probabilidad de x cuando es mayor  o igual a 1
Aux=1-pbinom(0,n,Pexi)
print(Aux)


#1) C X>=x, con x==2

#Probabilidad acumulada de que x sea mayor o igual a 2
Auxu=1-pbinom(1,n,Pexi)
print(Auxu)

#2) 
#distribucion Normal 
#Estableciendo los valores entregados

#Cantidad de paracaidas
x=5
#Valor esperado de la distribucion
VM=200
#Desviacion estandar de la distribucion
Dv=30
#Varianza de la distribucion
Var=(Dv)^2
print(Var)
#Calcular la probabilidad con x==5
pnorm(x,VM,Var)

#4) Distribucion Poisson

#Valor esperado de la distribucion
Lamda=20

#A)

#Definicion de la variable aleatoria
#Variable Aleatoria = Numero de Conductores

#Definicion de la distribucion de Poisson
#Distribucion = ((e^-u)*(u^x))/x!

#B)
#Establece valores seudoaleatorios con los datos del valor esperado y la 
#cantidad de observaciones
rpois(10,Lamda)


#C) X<=x, con x==10

#Calculo de la probabilidad empirica, (N° veces que ocurre el evento)/(N° Total de Observaciones)
#Probabilidad empirica de x cuando es menor o igual a 10
Poiss=ppois(10,Lamda)

#Mostrar el valor calculado
print(Poiss)

#D) X<=x, con x==10
#Calculo de la probabilidad teorica, (N° Total resultados favorables)/(N° Total resultados posibles)
#Probabilidad Teorica
Poss=ppois(10,Lamda)/10

#Mostrar el valor calculado
print(Poss)

#E)
#Son distintos los valores debido a que el calcular la probabilidad empirica, es un calculo 
#de la distribucion de Poisson, mientras que la probabilidad teorica corresponde a la division
#de la probabilidad acumulada de Poisson, dividido en la cantidad de observaciones favorables.