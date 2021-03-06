###################################################################################
# Practica 6
# PROPOSITO: Realizar un An�lisis Factorial completo e interpretar los resultados
# AUTORES: Sergio Casado L�pez y Sandra G�mez G�lvez
# FECHA: 20/04/2019
# COMENTARIOS:Factanal para los datos introducidos mediante txt no funcionar� al ser
#             una matriz no invertible con det=0
###################################################################################

# Ejecutamos la libreria que tiene que estar instalada
library(psych)


# Cogemos el fichero Datos1.txt
M <-read.table("C:/Users/sandr/Documents/Geometr�a Computancional/PRACTICA6/Cereales2.txt",header=T,sep="")

det(data.matrix(M)) #Como su determinante es 0 la matriz no es invertible

# Realizamos el Test de esfericidad de Bartlett
bartlett.test(M)
#Por tanto, podemos realizar el an�lisis factorial 


# princomp( ): an�lisis de componentes principales sin rotar
Modelo1 <- princomp(M, cor=TRUE)

# Realizamos la Varianza de cada factor
summary(Modelo1 ) 
# Estamos trabajando con 8 dimensiones, pero podemos trabajar con menos �pero cu�ntas menos? 
    ##Puntuaciones de las componentes y gr�fico de sedimentaci�n:
# Puntuaciones en los factores
loadings(Modelo1)
# Gr�fico de sedimentaci�n
plot(Modelo1,type="lines") 

## nfactors permite establecer el n�mero de factores. 
# Rotate permite definir la rotaci�n: puede tomar los valores "none",
# "varimax", "quartimax", "promax", "oblimin", "simplimax", or
# "cluster"
library(GPArotation)
Modelo2 <- principal(M, nfactors=2, rotate="varimax") #Con varimax se ajusta lo m�ximo posible a la realidad
# Imprime los resultados
Modelo2
# Varianza de cada factor
summary(Modelo2)
# Puntuaciones en los factores
loadings(Modelo2)
# Puntuaciones de los casos
Modelo2 $scores
biplot(Modelo2)

# Factanal produce el an�lisis factorial basado en m�xima verosimilitud
Modelo3<- factanal(M,4, rotation="varimax") 
  # Esto nos produce un error, ya que el valor que sale de la funci�n es menor que el m�nimo estandar
print(Modelo3, digits=2, cutoff=.3, sort=TRUE)
# plot factor 1 y factor 2
load <- Modelo3$loadings[,1:2]
plot(load,type="n") # Establecemos el plot
text(load,labels=names(M),cex=.7) # a�adimos los nombres de las variables

