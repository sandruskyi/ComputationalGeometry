X<-c(-2,0,4,-1,3,6,2,5,-3,7)
Y<-c(2,2,4,1,-5,5,5,-8,2,3)
p<-array(c(X,Y),dim = c(10,2))

#En la variable p se encuentran listado los puntos del poligono,
#suponemos que esos puntos se encuentran ordenados segun la coordenada Y de manera decreciente.
#Si no es asi, es necesario ordenar el vector de puntos.
#La variable matriz es la matriz de adyacencia teniendo en cuenta la nueva ordenacion de los puntos ya dicha, es decir, el vector p y la matriz se corresponden actualmente.
algFuerzaBruta<-function(p,matriz){
  #pila contiene los vertices que ya hemos recorrido
  pila<-array()
  #aristas contiene las aristas dibujadas para realizar la triangulacion
  #se guardan como pares (v1,v2) de vertices, siendo una matriz de 4 columnas y n filas (n numero de aristas)
  #Las columnas 1 y 2 son las coordenadas X e Y de v1, y la 3 y 4 las coordenadas de v2
  #Es importante que cada vez que se añada una arista, se refleje en la matriz de adyacencia, y nunca se añada una arista si los vertices ya son adyacentes.
  aristas<-array()
  pila<-array(c(p[1,1],p[2,1],p[1,2],p[2,2]),dim = c(2,2))
  for (i in 3:(length(p)/2)) {
    ady1=adyacente(p[i,1:2],pila[1,1:2],matriz)
    ady2=adyacente(p[i,1:2],pila[length(pila)/2,1:2],matriz)
    if(ady1 && !ady2){
      #añadir aristas (p[1], pila[2])......(p[1], pila[length(pila)])
      #dejar solo en pila pila[length(pila)] y p[1]
    }else if(!ady1 && ady2){
      #mientras visible(p[i],pila[length(pila)-1])
        #añadir arista (p[i],pila[length(pila)-1])
        #quitar de la pila pila[length(pila)]
      #fuera del while, meter p[i] en la pila
    }else if(ady1 && ady2){
      #añadir aristas (p[1], pila[2])......(p[1], pila[length(pila)-1])
      #vaciar la pila e i=length(p)/2
    }
    
    return(aristas)
    
  }
  
  
  
  #añadir cosas a array
  #solucion<-array(c(solucion[1:(length(solucion)/2),1],p[i,1],solucion[1:(length(solucion)/2),2],p[i,2]),dim = c((length(solucion)/2)+1,2))
}