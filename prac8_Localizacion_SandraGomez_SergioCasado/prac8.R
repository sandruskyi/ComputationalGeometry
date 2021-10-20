###################################################################################
# Practica  8
# PROPOSITO: Utilizar una estrategia Greedy o alguno de los tres algoritmos propuestos para resolver alguno 
#            de los problemas p-mediana o 1-centro de los problemas test
# AUTORES: Sergio Casado López y Sandra Gómez Gálvez
# FECHA: 24/04/2019
# COMENTARIOS: Utilizaremos el ALGORITMO 1-CENTRO
###################################################################################

#library(Rmpfr)
# Cogemos el fichero phub_50_5_1.txt
M <-read.table("C:/Users/sandr/Documents/Geometría Computancional/Practica8/phub_50_5_5.txt",header=T,sep="")

PUNTOS<-as.matrix(M[1:50,1:2])
#PUNTOS<- mpfr(PUNTOS,120)
#alg1Centro(PUNTOS)

alg1Centro<-function(S){
  
  if((length(S)/2)<4){
    if((length(S)/2)==1){
      centroMin<-S[1,1:2]
      radioMin<-0
    }else if((length(S)/2)==2){
      centroMin<-puntoMedio(S[1,1:2], S[2,1:2])
      diam<-distancia(S[1,1:2], S[2,1:2])
      radioMin<-(diam/2)
    }else if((length(S)/2)==3){
      x1<-S[1,1]
      x2<-S[2,1]
      x3<-S[3,1]
      y1<-S[1,2]
      y2<-S[2,2]
      y3<-S[3,2]
      a <- rbind(c(x1,y1,1),c(x2,y2,1),c(x3,y3,1))
      if(det(a)!=0){
        b <- c(-x1^2-y1^2,-x2^2-y2^2,-x3^2-y3^2)
        solucion <- solve(a,b, tol = 1e-30)
        a<-solucion[1]
        b<-solucion[2]
        c<-solucion[3]
        radioMin<-sqrt(a^2+b^2-4*c)/2
        centrox<-(-a/2)
        centroy<-(-b/2)
        centroMin<-c(centrox,centroy)
      }else{
        if(x1==x2 && x1==x3 && x2==x3){ #Vemos el caso en el que los 3 puntos estén alineados 
                                        #en vertical
          masArribay<-y3 
          masArribax<-x3
          masAbajoy<-y1
          masAbajox<-x1
          if(y1>masArribay){
            masArribay<-y1
            masArribax<-x1
          }else if(y2>masArribay){
            masArribay<-y2
            masArribax<-x2
          }
          if(y2<masAbajoy){
            masAbajoy<-y2
            masArribax<-x2
          }else if(y3<masAbajoy){
            masAbajoy<-y3
            masAbajox<-x3
          }
          puntoAbajo<-c(masAbajox,masAbajoy)
          puntoArriba<-c(masArribax,masArribay)
          centroMin<-puntoMedio(puntoAbajo,puntoArriba)
          diam<-distancia(puntoArriba, puntoAbajo)
          radioMin<-(diam/2)
        }else{ #demás casos
          masALaDerechax<-x3
          masALaDerechay<-y3
          masALaIzquierdax<-x1
          masALaIzquierday<-y1
          if(x2<masALaIzquierdax){
            masALaIzquierdax<-x2
            masALaIzquierday<-y2
          }else if(x3<masALaIzquierdax){
            masAlaIzquierdax<-x3
            masAlaIzquierday<-y3
          }
          if(x2>masALaDerechax){
            masALaDerechax<-x2
            masALaDerechay<-y2
          }else if(x1>masALaDerechax){
            masAlaDerechax<-x1
            masAlaDerechay<-y1
          }
          puntoIzquierda<-c(masALaIzquierdax,masALaIzquierday)
          puntoDerecha<-c(masALaDerechax,masALaDerechay)
          centroMin<-puntoMedio(puntoIzquierda,puntoDerecha)
          diam<-distancia(puntoDerecha, puntoIzquierda)
          radioMin<-(diam/2)
        }
        
      }
    }
    
  }else{
    radioMin<-0
    for (i in 1:(length(S)/2)) {
      for(j in 1:(length(S)/2)){
        if(i!=j){ #Cogemos 2 puntos distintos y calculamos su diámetro, radio y centro
          diam<-distancia(S[i,1:2], S[j,1:2])
          radio<-(diam/2)
          centro<-puntoMedio(S[i,1:2], S[j,1:2])
          existe<-TRUE
          for(k in 1:(length(S)/2)){ #Comprobará el resto de puntos que no serán los iniciales y comprueba que estén 
                                      #todos dentro, si están todos dentro existe el circulo mínimo
            di<-distancia(S[k,1:2], centro)
            if(di>radio){
              existe<-FALSE
            }
          }
          if(existe){ #Si no hay puntos exteriores
            if((radio<radioMin)||(radioMin==0)){ #Vemos si es el círculo menor
              radioMin<-radio
              centroMin<-centro
            }
          }
        }
      }
    }
    for (i in 1:(length(S)/2)){ #Recorreremos ahora 3 puntos
      for(j in i:(length(S)/2)){
        for(k in j:(length(S)/2)){
          if((i!=j)&&(i!=k)&&(j!=k)){
            x1<-S[i,1]
            x2<-S[j,1]
            x3<-S[k,1]
            y1<-S[i,2]
            y2<-S[j,2]
            y3<-S[k,2]
            a <- rbind(c(x1,y1,1),c(x2,y2,1),c(x3,y3,1))
            if(det(a)!=0){
              b <- c(-(x1^2)-(y1^2),-(x2^2)-(y2^2),-(x3^2)-(y3^2))
              solucion <- solve(a,b, tol = 1e-30)
              #print("solucion")
              #print(solucion)
              a<-solucion[1]
              b<-solucion[2]
              c<-solucion[3]
              radio<-sqrt(a^2+b^2-4*c)/2
              centrox<-(-(a/2))
              centroy<-(-(b/2))
              centro<-c(centrox,centroy)
              existe<-TRUE
              for(l in 1:(length(S)/2)){ #Comprobará el resto de puntos que no serán los iniciales y comprueba que 
                                          #estén todos dentro, si están todos dentro existe el circulo mínimo
                di<-distancia(S[l,1:2], centro)
                if(di>radio){
                  existe<-FALSE
                }
              }
              if(existe){ #Si no hay puntos exteriores
                if((radio<radioMin)||(radioMin==0)){ #Vemos si es el círculo menor
                  radioMin<-radio
                  centroMin<-centro
                }
              }
            }
          }
        }
      }
    }
  }
  return(c("El radio es:",radioMin,"El centro es:",centroMin))
}

distancia<-function(p1, p2){
  d<-sqrt(((p2[1]-p1[1])^2)+((p2[2]-p1[2])^2))
  return(d)
}
puntoMedio<-function(p1,p2){
  pm<-c((p1[1]+p2[1])/2,(p1[2]+p2[2])/2)
  return(pm)
}

dibujar<-function(v,p){
  
  radio<-as.numeric(v[2])
  centro<-c(as.numeric(v[4]),as.numeric(v[5]))
  
  if(radio<=0) stop("El radio de una circunferencia es estrictamente positivo")
  if(length(centro)!=2) stop("El centro de una circunferencia en el plano debe ser un vector de dimensión 2")
  
  t <- seq(0, 2*pi, length.out = 100)
  
  xx<-centro[1]+cos(t)*radio
  yy<-centro[2]+sin(t)*radio
  
  plot(xx,yy,type="l")
  for (i in 1:(length(p)/2)){
    points(p[i,1], p[i,2] , col="green")
  }
}