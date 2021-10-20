###################################################################################
# Practica 3
# PROPOSITO: 
# Programar los algoritmos de ordenación rápidos: Mergesort, Quicksort, Heapsort. 
#Gráfica de comparación de los mismos
# AUTORES: Sergio Casado López y Sandra Gómez Gálvez
# FECHA: 28/03/2019
# COMENTARIOS:  
     #Se pueden realizar pruebas con: 
          #x = c(1,6,2,9,5,4,2,3)
     # ó (entre otros):
          #test_vec=round(runif(100,0,100))
     # Y llamando a la función
###################################################################################



##################  MERGESORT  #############################
############################################################


merge<-function(a,b){
  r<-numeric(length(a)+length(b))
  ai<-1; bi<-1; j<-1;
  for(j in 1:length(r)) {
    if((ai<=length(a) && a[ai]<b[bi]) || bi>length(b)) {
      r[j] <- a[ai]
      ai <- ai+1
    } else {
      r[j] <- b[bi]
      bi <- bi+1          
    }
  }
  r
}


mergesort<-function(array){
  if (length(array)>1){
    centro<-ceiling(length(array)/2)
    a1<-mergesort(array[1:centro])
    a2<-mergesort(array[(centro+1):length(array)])
    merge(a1,a2)
  }else{
    array
  }
}

###################### QUICKSORT ###########################
############################################################
quickSort <- function(array) {
  # Numero aleatorio
  eleIndex <- sample(seq_along(array), 1)
  ele<-array[eleIndex]
  array<-array[-eleIndex]
  
  # Vectores para izquierda y derecha
  izq <- c()
  der <- c()
  
  izq<-array[which(array<=ele)]
  der<-array[which(array>ele)]
  
  if(length(der)>1){
    der<-quickSort(der)
  }
  
  if(length(izq)>1){
    izq<-quickSort(izq)
  }
    
  c(izq,ele,der)
}


#############################################################
################### HEAPSORT ################################
modHeap<-function(heap,nodo)
{
  len=length(heap)
  flag=1
  
  while (nodo*2<=len&&flag==1)
  {
    izq=nodo*2
    der=nodo*2+1
    flag=0
    son=c(heap[izq],heap[der])
    son=son[!is.na(son)]
    min=which.min(son)
    if (heap[nodo]>son[min])
    {
      flag=1
      heap_ind=c(izq,der)[min]
      
      tmp=heap[heap_ind]
      heap[heap_ind]=heap[nodo]
      heap[nodo]=tmp
      
      nodo=heap_ind
    }
  }
  return(heap)
}

heapsort<-function(vec)
{
  n=length(vec)
  heap=vec
  for (i in n:1)
  {
    heap=modHeap(heap,i)
  }
  ordenado=NULL
  n=length(heap)
  while(n>0)
  {
    ordenado=c(ordenado,heap[1])
    n=length(heap)
    heap[1]=heap[n]
    heap=heap[1:(n-1)]
    heap=modHeap(heap,nodo=1)
    n=n-1
  }
  return(ordenado)
}

################################################################
###########################GRÁFICA#############################
###### 10 elementos ##########
array<-sample(1:500,10,replace=T)
m1<-system.time(mergesort(array))
q1<-system.time(quickSort(array))
h1<-system.time(heapsort(array))

ms1<-mergesort(array)
qs1<-quickSort(array)
hs1<-heapsort(array)

plot(0:500,seq(0,0.1,by=0.0002) , col="white")

points(10, m1[1] , col="blue", pch="o" )
points(10, q1[1], col="green", pch="x")
points(10, h1[1], col="red", pch="*")


###### 50 elementos ##########
array<-sample(1:500,50,replace=T)
m2<-system.time(mergesort(array))
q2<-system.time(quickSort(array))
h2<-system.time(heapsort(array))

ms2<-mergesort(array)
qs2<-quickSort(array)
hs2<-heapsort(array)

points(50, q2[1], col="green", pch="x")
x=c(10,50)
y=c(q1[1],q2[1])
lines(x, y, col="green",lty=3)
q1<-q2

points(50, h2[1], col="red", pch="*")
x=c(10,50)
y=c(h1[1],h2[1])
lines(x, y, col="red",lty=1)
h1<-h2

points(50, m2[1], col="blue", pch="o")
x=c(10,50)
y=c(m1[1],m2[1])
lines(x, y, col="blue",lty=2)
m1<-m2


###### 100 elementos ##########
array<-sample(1:500,100,replace=T)
m2<-system.time(mergesort(array))
q2<-system.time(quickSort(array))
h2<-system.time(heapsort(array))

ms3<-mergesort(array)
qs3<-quickSort(array)
hs3<-heapsort(array)

points(100, q2[1], col="green", pch="x")
x=c(50,100)
y=c(q1[1],q2[1])
lines(x, y, col="green",lty=3)
q1<-q2

points(100, h2[1], col="red", pch="*")
x=c(50,100)
y=c(h1[1],h2[1])
lines(x, y, col="red",lty=1)
h1<-h2

points(100, m2[1], col="blue", pch="o")
x=c(50,100)
y=c(m1[1],m2[1])
lines(x, y, col="blue",lty=2)
m1<-m2


###### 200 elementos ##########
array<-sample(1:500,200,replace=T)
m2<-system.time(mergesort(array))
q2<-system.time(quickSort(array))
h2<-system.time(heapsort(array))

ms4<-mergesort(array)
qs4<-quickSort(array)
hs4<-heapsort(array)

points(200, q2[1], col="green", pch="x")
x=c(100,200)
y=c(q1[1],q2[1])
lines(x, y, col="green",lty=3)
q1<-q2

points(200, h2[1], col="red", pch="*")
x=c(100,200)
y=c(h1[1],h2[1])
lines(x, y, col="red",lty=1)
h1<-h2

points(200, m2[1], col="blue", pch="o")
x=c(100,200)
y=c(m1[1],m2[1])
lines(x, y, col="blue",lty=2)
m1<-m2


###### 300 elementos ##########
array<-sample(1:500,300,replace=T)
m2<-system.time(mergesort(array))
q2<-system.time(quickSort(array))
h2<-system.time(heapsort(array))

ms5<-mergesort(array)
qs5<-quickSort(array)
hs5<-heapsort(array)

points(300, q2[1], col="green", pch="x")
x=c(200,300)
y=c(q1[1],q2[1])
lines(x, y, col="green",lty=3)
q1<-q2

points(300, h2[1], col="red", pch="*")
x=c(200,300)
y=c(h1[1],h2[1])
lines(x, y, col="red",lty=1)
h1<-h2

points(300, m2[1], col="blue", pch="o")
x=c(200,300)
y=c(m1[1],m2[1])
lines(x, y, col="blue",lty=2)
m1<-m2



###### 400 elementos ##########
array<-sample(1:500,400,replace=T)
m2<-system.time(mergesort(array))
q2<-system.time(quickSort(array))
h2<-system.time(heapsort(array))

ms6<-mergesort(array)
qs6<-quickSort(array)
hs6<-heapsort(array)

points(400, q2[1], col="green", pch="x")
x=c(300,400)
y=c(q1[1],q2[1])
lines(x, y, col="green",lty=3)
q1<-q2

points(400, h2[1], col="red", pch="*")
x=c(300,400)
y=c(h1[1],h2[1])
lines(x, y, col="red",lty=1)
h1<-h2

points(400, m2[1], col="blue", pch="o")
x=c(300,400)
y=c(m1[1],m2[1])
lines(x, y, col="blue",lty=2)
m1<-m2


###### 500 elementos ##########
array<-sample(1:500,500,replace=T)
m2<-system.time(mergesort(array))
q2<-system.time(quickSort(array))
h2<-system.time(heapsort(array))

ms7<-mergesort(array)
qs7<-quickSort(array)
hs7<-heapsort(array)

points(500, q2[1], col="green", pch="x")
x=c(400,500)
y=c(q1[1],q2[1])
lines(x, y, col="green",lty=3)
q1<-q2

points(500, h2[1], col="red", pch="*")
x=c(400,500)
y=c(h1[1],h2[1])
lines(x, y, col="red",lty=1)
h1<-h2

points(500, m2[1], col="blue", pch="o")
x=c(400,500)
y=c(m1[1],m2[1])
lines(x, y, col="blue",lty=2)
m1<-m2