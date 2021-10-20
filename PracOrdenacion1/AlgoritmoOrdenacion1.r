###################################################################################
# Practica 2
# PROPOSITO: 
            # Algoritmos de ordenaci�n: algoritmo de la burbuja implementado 
              # con el c�digo de clase y con otro c�digo, y algoritmo de 
              # ordenaci�n de la inserci�n
            # Gr�fica de comparaci�n de los mismos
# AUTORES: Sergio Casado L�pez y Sandra G�mez G�lvez
# FECHA: 07/03/2019
# COMENTARIOS:  Se adjunta tambi�n el Algoritmo de la Selecci�n e Inserci�n hecho en clase
###################################################################################


################## ALGORITMO BURBUJA 0(n^2) #######################################
      # Queremos ordenar de menor a mayor, comparando cada elemento con el siguiente 

############### Nuestro c�digo: 
  ordenar<- function(x){
    n<- length(x)           # Cogemos la longitud de x
                                # Recordamos que los Arrays en R comienzan en 1
    for(i in 2:n){          # Realizamos un bucle for anidado desde i=2 hasta n
      for(j in 1:(n-1)){      # y desde j=1 hasta n-1
        if(x[j]>x[j+1]){    # Si x en la posici�n j es mayor que x en la posici�n j+1 hacemos:
          v<- x[j];           # v = x en la posici�n j
          x[j]<-x[j+1];       # x en la posici�n j ser� = x en la posici�n j+1
          x[j+1]<-v;          # x en la posici�n j+1 ser� = v
        }
      }
    }
    x                       # Mostramos x
  }
  
################ Bubblesort de clase:
  # Funci�n intercambio: asigna el valor de pair[i] en pair[j]
  intercambio=function(pair,i,j){  
    aux=pair[i];                   
    pair[j]=aux;                  
    pair
  }
  # La funci�n larger va a comparar dos elementos de una lista 
  larger= function(pair){
    if((pair[1]>pair[2])){
       return(TRUE)
    }else{
      return(FALSE)
    }
      
  }
  # La funci�n swap_if_larger permuta los elementos de la lista si se satisface la condicion de larger
  swap_if_larger=function(pair){
    if(larger(pair)){
      return(rev(pair)) #Rev le da la vuelta
    }else{
      return(pair)
    }
  }
  # La funci�n swap_pass permuta los elementos de la lista comenzando desde el primer valor hasta el final del vector si se satisface la condicion de larger
  swap_pass= function(vec){
    for(i in seq(1, length(vec)-1)){
      vec[i:(1+i)]=swap_if_larger(vec[i:(i+1)])
    }
    return (vec)
    
  }
  # Por tanto bublesort ser�: 
  bubble_sort= function(vec){
    new_vec= swap_pass(vec)
    if(isTRUE(all.equal(vec, new_vec))){
      return(new_vec)
    }else{
      return(bubble_sort(new_vec))
    }
  }
  
############ Para probarlos: 
    # Podemos crear por consola un vector: 
              # x <- c(1,6,2,9,5,4,2,3)
              # bubble_sort(x)
    # Si no, podemos generar un vector aleatorio de n� enteros (es un random): test_vec=round(runif(100,0,100))
              # runif es para random numbers
              #system.time genera el tiempo que tarda en realizarlo 
              # Por tanto, se prueba con: 
                # test_vec=round(runif(100,0,100))
                # bubble_sort(test_vec)
                # system.time(bubble_sort(test_vec)) 


 
  
################### ALGORITMO DE LA INSERCI�N ####################################
  insertar<- function(v){
    n<- length(v)
    for( i in 1:n){
      actual=v[i]
      j=i-1 
      while((j>0)&&(v[j]>actual)){
        v[j+1]=v[j]
        j= j-1
      }
      v[j+1]=actual
    }
    return(v)
  }
  
  
  
  ############ Gr�fica de tiempos: 
  # Crearemos una gr�fica comparando los c�digos con vectores de distintos tama�os: 
  
  # Para 10 elementos: 
  vec=round(runif(10,0,100))
  bubble_sort(vec)
  t<-system.time(bubble_sort(vec))  
  ordenar(vec)
  t2<-system.time(ordenar(vec)) 
  insertar(vec)
  t4<-system.time(insertar(vec))
  
  plot(0:100,seq(0,0.2,by=0.002) , col="white")
  points(10, t2[1] , col="blue", pch="o" )
  
  points(10, t4[1], col="green", pch="x")
  
  points(10, t[1], col="red", pch="*")
  
  # Para 50 elementos: 
  vec=round(runif(50,0,100))
  bubble_sort(vec)
  t1<-system.time(bubble_sort(vec))  
  ordenar(vec)
  t3<-system.time(ordenar(vec))  
  insertar(vec)
  t5<-system.time(insertar(vec))
  
  points(50, t5[1], col="green", pch="x")
  x=c(10,50)
  y=c(t4[1],t5[1])
  lines(x, y, col="green",lty=3)
  
  points(50, t1[1], col="red", pch="*")
  x=c(10,50)
  y=c(t[1],t1[1])
  lines(x, y, col="red",lty=1)
  
  points(50, t3[1], col="blue", pch="o")
  x=c(10,50)
  y=c(t2[1],t3[1])
  lines(x, y, col="blue",lty=2)
  
  # Para 100 elementos: 
  vec=round(runif(100,0,100))
  bubble_sort(vec)
  t<-system.time(bubble_sort(vec))  
  ordenar(vec)
  t2<-system.time(ordenar(vec))  
  insertar(vec)
  t4<-system.time(insertar(vec))
  
  points(100, t4[1], col="green", pch="x")
  x=c(50,100)
  y=c(t5[1],t4[1])
  lines(x, y, col="green",lty=3)
  
  points(100, t[1], col="red", pch="*")
  x=c(50,100)
  y=c(t1[1],t[1])
  lines(x, y, col="red",lty=1)
  
  points(100, t2[1], col="blue", pch="o")
  x=c(50,100)
  y=c(t3[1],t2[1])
  lines(x, y, col="blue",lty=2)
  
  # El algoritmo creado por nosotros mismos es el m�s r�pido
  
###################### ALGORITMO DE LA SELECCION ##############################
#Buscamos el menor elemento desde la posicion i hasta el final e intercambiamos el minimo por el elemento de esa posicion
  seleccion_sort <- function(v){
    n<- length (v)
    for( i in 1: (n-1) ){
      minimo=i
      temp=v[i]
      for(j in i:n){
        if(v[j]<temp){
          minimo=j
          temp=v[j]
        }
      }
    v[minimo]=v[i];
    v[i]=temp;
    }
    return(v)
  }
  