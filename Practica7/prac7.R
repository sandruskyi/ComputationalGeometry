
#p<-c(c(1,2),c(2,2),c(3,3),c(0,2),c(4,-2),c(-1,2),c(-2,3),c(5,3))
p<-array(c(c(1,2),c(2,2),c(3,3),c(0,2),c(4,-2),c(-1,2),c(-2,3),c(5,3)),dim = c(3,2))


algFuerzaBruta<-function(p){
  solucion<-c()
  for (i in 1:length(p)) {
    for(j in 1:length(p)){
      if(i!=j){
        #true significa que no hay puntos a la izq o der, false que hay al menos un punto
        izq=TRUE 
        der=TRUE
        m<-pendiente(p[i], p[j])
        #pequeña derecha
        #grande izq
        for(k in 1:length(p)){
          if((k!=i)&&(k!=j)){
            x<-bajo(p[i], p[j])
            m1<-pendiente(p[k], x)
            if(m<0){
              if(comprobar(x,p[k])){ #true derecha y false izq
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
              if(comprobar(x,p[k])){ #true derecha y false izq
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
          if(!pertenece(solucion, p[i])){
            solucion<-c(solucion,p[i])
          }
          if(!pertenece(solucion, p[j])){
            solucion<-c(solucion,p[j])
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
  for(i in 1:length(solucion)){
    if((p[1]==solucion[i][1])&&(p[2]==solucion[i][2])){
      per=TRUE
    }
  }
}