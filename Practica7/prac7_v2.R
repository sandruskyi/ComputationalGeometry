
X<-c(-2,0,4,-1,3,6,2,5,-3,7)
Y<-c(2,2,4,1,-5,5,5,-8,2,3)
p<-array(c(X,Y),dim = c(10,2))
p


algFuerzaBruta<-function(p){
  solucion<-array()
  for (i in 1:(length(p)/2)) {
    for(j in 1:(length(p)/2)){
      if(i!=j){
        #true significa que no hay puntos a la izq o der, false que hay al menos un punto
        izq=TRUE 
        der=TRUE
        m<-pendiente(p[i,1:2], p[j,1:2])
        #pequeña derecha
        #grande izq
        for(k in 1:(length(p)/2)){
          if((k!=i)&&(k!=j)){
            x<-bajo(p[i,1:2], p[j,1:2])
            m1<-pendiente(p[k,1:2], x)
            if(m<0){
              if(comprobar(x,p[k,1:2])){ #true derecha y false izq
                if(m1>m){
                  der=FALSE
                }else if(m1<m){
                  izq=FALSE
                }
              }else{
                if(m1<m){
                  der=FALSE
                }else if(m1>m){
                  izq=FALSE
                }
              }
            }else{
              if(comprobar(x,p[k,1:2])){ #true derecha y false izq
                if(m1<m){
                  der=FALSE
                }else if(m1>m){
                  izq=FALSE
                }
              }else{
                if(m1>m){
                  der=FALSE
                }else if(m1<m){
                  izq=FALSE
                }
              }
            }
          }
        }
        if(izq||der){
          if(length(solucion)==1){
            solucion<-array(c(p[i,1],p[j,1],p[i,2],p[j,2]),dim = c(2,2))
          }else{
            if(!pertenece(solucion, p[i,1:2])){
              solucion<-array(c(solucion[1:(length(solucion)/2),1],p[i,1],solucion[1:(length(solucion)/2),2],p[i,2]),dim = c((length(solucion)/2)+1,2))
            }
            if(!pertenece(solucion, p[j,1:2])){
              solucion<-array(c(solucion[1:(length(solucion)/2),1],p[j,1],solucion[1:(length(solucion)/2),2],p[j,2]),dim = c((length(solucion)/2)+1,2))
            }
          }
        }
      }
    }
  }
  return(solucion)
}

pendiente<-function(p,q){
  if(p[1]>q[1]){
    m<-((p[2]-q[2])/(p[1]-q[1]))
  }else{
    m<-((q[2]-p[2])/(q[1]-p[1]))
  }
  return(m)
}
bajo<-function(p,q){ #Cogemos el punto que más abajo esté en el eje Y,en caso de que estén a la misma altura, se coge el punto más a la izq
  if(p[2]<q[2]){
    return(p)
  }else if(p[2]>q[2]){
    return(q)
  }else if(p[1]<q[1]){
    return(p)
  }else{
    return(q)
  }
}
comprobar<-function(x,p){ #true derecha y false izq
  if(x[1]<p[1]){
    return(TRUE)
  }else{
    return(FALSE)
  }
}
pertenece<-function(solucion, p){
  per=FALSE
  for(i in 1:(length(solucion)/2)){
    if((p[1]==solucion[i,1])&&(p[2]==solucion[i,2])){
      per=TRUE
    }
  }
  return(per)
}