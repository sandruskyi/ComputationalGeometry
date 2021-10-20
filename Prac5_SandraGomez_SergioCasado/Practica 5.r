
###################################################################################
# Practica 5
# PROPOSITO: 
# Dado un conjunto de puntos en R2 realizar tralación respecto (-2,2), rotacion respecto pi/3,
#     simetria respecto y=2x+2, y homotecia de centro (1,1) y razon 4
# En R3 realizar traslacion respecto (-2,2,1),Rotación respecto pi/3,Simetría respecto y= 2x+2y-1,
#     Homotecia de centro (1,1,1) y razón 4
# AUTORES: Sergio Casado López y Sandra Gómez Gálvez
# FECHA: 04/04/2019
# COMENTARIOS:---
###################################################################################

##################################R2

Traslacion<-function(p,x){
  y=c()
  y[1]=p[1]+x[1]
  y[2]=p[2]+x[2]
  y
}
Rotacion<-function(p,t){
  A=matrix(c(cos(t),sin(t),-sin(t),cos(t)),2,2)
  y=A%*%p
  y
}
Simetria<-function(p,f){
  m=(f(1)-f(0))/(1-0)
  t=atan(m)
  n=f(0)
  A=matrix(c(-n*sin(2*t),n+n*cos(2*t)),2,1)
  B=matrix(c(cos(2*t),sin(2*t),sin(2*t),-cos(2*t)),2,2)
  y=A+B%*%p
  y
}
Homotecia<-function(p,cent,r){
  A=matrix(c(cent[1]*(1-r),cent[2]*(1-r)),2,1)
  y=A+r*p
  y
}

EjemploTraslacion<-function(){
  X=round(runif(10,0,10))
  Y=round(runif(10,0,10))
  plot(-2:12, -2:12, col="white")
  for (i in 1:10) {
    points(X[i], Y[i] , col="blue", pch=paste("o",i) )
    p=Traslacion(c(-2,2),c(X[i],Y[i]))
    x=c(X[i],p[1])
    y=c(Y[i],p[2])
    points(p[1], p[2] , col="green", pch=paste("o",i) )
    lines(x, y, col="red",lty=1)
    
  }
}

EjemploRotacion<-function(){
  X=round(runif(10,0,10))
  Y=round(runif(10,0,10))
  plot(-10:10, -10:10, col="white")
  for (i in 1:10) {
    points(X[i], Y[i] , col="blue", pch=paste("o",i) )
    p=Rotacion(c(X[i],Y[i]),pi/3)
    x=c(X[i],p[1])
    y=c(Y[i],p[2])
    points(p[1], p[2] , col="green", pch=paste("o",i) )
    lines(x, y, col="red",lty=1)
  }
}

EjemploSimetria<-function(){
  X=round(runif(10,0,10))
  Y=round(runif(10,0,10))
  f<-function(x)2*x+2
  t=-10:10
  plot(t, f(t), col="white")
  lines(t, f(t), col="black",lty=1)
  for (i in 1:10) {
    points(X[i], Y[i] , col="blue", pch=paste("o",i) )
    p=Simetria(c(X[i],Y[i]),f)
    x=c(X[i],p[1])
    y=c(Y[i],p[2])
    points(p[1], p[2] , col="green", pch=paste("o",i) )
    lines(x, y, col="red",lty=1)
  }
}

EjemploHomotecia<-function(){
  X=round(runif(10,0,10))
  Y=round(runif(10,0,10))
  plot(0:20, 0:20, col="white")
  for (i in 1:10) {
    points(X[i], Y[i] , col="blue", pch=paste("o",i) )
    p=Homotecia(c(X[i],Y[i]),c(1,1),4)
    x=c(X[i],p[1])
    y=c(Y[i],p[2])
    points(p[1], p[2] , col="green", pch=paste("o",i) )
    lines(x, y, col="red",lty=1)
  }
}

######################################R3
TraslacionR3<-function(p,x){
  y=c()
  y[1]=p[1]+x[1]
  y[2]=p[2]+x[2]
  y[3]=p[3]+x[3]
  y
}
#Rotacion respecto al eje Y
RotacionR3<-function(p,t){
  Ay=matrix(c(cos(t),0,-sin(t),0,1,0,sin(t),0,cos(t)),3,3)
  y=A%*%p
  y
}

install.packages('plot3Drgl', dependencies=TRUE)
library("plot3Drgl")

EjemploTraslacionR3<-function(){
  X=round(runif(10,0,10))
  Y=round(runif(10,0,10))
  Z=round(runif(10,0,10))
  
  #plot3D(-2:12, -2:12, -2:12,col="blue", type="p")
  plot3d(replicate(2, 1:12), type= 'n',xlim= c(-1,1),ylim=c(-1,1),zlim=c(-3,3))
  planes3d(3,3,0,-3, col = 'red', alpha= 0.6)
  for (i in 1:10) {
    points3d(X[i], Y[i] ,Z[i], col="blue", pch=paste("o",i) )
    p=TraslacionR3(c(-2,2,1),c(X[i],Y[i],Z[i]))
    x=c(X[i],p[1])
    y=c(Y[i],p[2])
    z=c(Z[i],p[3])
    
    points3d(p[1], p[2] ,p[3], col="green", pch=paste("o",i) )
    lines3d(x, y,z, col="red",lty=1)
    decorate3d()
    
  }
  
}
EjemploRotacionR3<-function(){
  X=round(runif(10,0,10))
  Y=round(runif(10,0,10))
  Z=round(runif(10,0,10))
  plot3d(replicate(2, 1:12), type= 'n',xlim= c(-1,1),ylim=c(-1,1),zlim=c(-3,3))
  planes3d(3,3,0,-3, col = 'red', alpha= 0.6)
  for (i in 1:10) {
    points3d(X[i], Y[i] ,Z[i], col="blue", pch=paste("o",i) )
    p=RotacionR3(c(X[i],Y[i],Z[i]),pi/3)
    x=c(X[i],p[1])
    y=c(Y[i],p[2])
    z=c(Z[i],p[3])
    points3d(p[1], p[2] ,p[3], col="green", pch=paste("o",i) )
    lines(x, y,z,col="red",lty=1)
    decorate3d()
  }
}

