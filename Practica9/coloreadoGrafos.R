###################################################################################
# Practica  9
# PROPOSITO: Coloreado de un mapa (grafo) cualquiera 
# AUTORES: Sergio Casado López y Sandra Gómez Gálvez
# FECHA: 15/05/2019
# COMENTARIOS: La idea es realizar un recorrido en profundidad rellenando los 
#              colores de cada nodo. Para ello tendremos un grafo que será un 
#              array de listas, que recorreremos. Y también un array donde
#              asociaremos cada color a un nodo
###################################################################################


#Creamos el grafo: 
grafo <- list(c(3,4,5), c(5,6), c(1,4,6), c(1,3), c(1,2,6), c(2,3,5))
#Creamos el array de visitados y lo instanciamos entero a 0, lo que implicara que no 
# tiene ningún color
visitados<- c(0,0,0,0,0,0)



#Llamamos a la función colorear con 3 colores y vemos si es posible colorear con 3: 
#if(colorear(grafo, 3, visitados,1)){
#  c("Podemos colorear con",3,"colores")
#  for(i in visitados){
#      print(i)
#  }
#}else{
#Como vemos que no es posible colorear con 3 colores, vemos si es posible colorear con 4: 
#  if(colorear(grafo, 4, visitados,1)){
#    c("Podemos colorear con",4,"colores")
#    for(i in visitados){
#      print(i)
#    }
#  }
#}


colorear<-function(grafo, numColores, visitados,etapa){
  print("HOLAAAAAAAAAAAAA")
  if(estanTodosVisitados(visitados)){
    print("HOLA3")
    print(c("Podemos colorear con",5,"colores"))
        for(i in visitados){
          print(i)
        }
    return(TRUE)
  }else{
    suficientes<-FALSE
    print("Hola1")
    i<-1
    #Probamos a poner los distintos colores en el nodo i
    while((i<= numColores) && (!suficientes)){
      if(esFactible(i,etapa, grafo, visitados )){
        print("Hola")
        visitados[etapa]<-i
        suficientes<- colorear(grafo, numColores, visitados, etapa+1)
      }
      i<- i+1
    }
    visitados[i]<-0;
    return(suficientes)
  }
}

estanTodosVisitados<-function(visitados){
  si<-TRUE
  i<-1
  while(i<=length(visitados) && si){
    if(visitados[i]==0){
      si<-FALSE;
    }
    i<-i+1
  }
  return(si)
}
esFactible<-function(tipo,nodo, grafo, visitados ){
  esFac<-TRUE
  ##Recorremos el grafo y miramos si dos adyacentes son el mismo 
  
  for(i in grafo){
    while(esFac){
      for(j in i){
        t<- (j+1)
        esFac<- visitados[t]!=tipo
      }
    }
  }
  return(esFac)
}