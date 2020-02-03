# Gráficos 3D con modelo plug-flow


library(rgl) # Se abre un paquete útil para realiza gráficos en 3D
library(deSolve)
library(ggplot2)

An = 5 # Valores Iniciales
Dn = 1.0

f = function(Dn,An) # Función de Biodisponibilidad
{ 
  r_init  = c(rep(1,100))
  c_init = c(rep(0,100))
  
  Dn = Dn
  An = An
  Do = 2
mod1 <- function(coord, y , parms){
  with(as.list(c(y, parms)),{
    dr = - (Dn/3)*((1-C)/r)
    dC = (Dn*Do*r*(1-C))-(2*An*C)
    return(list(c(dr, dC)))
  })
}
y <- c(r = 1, C = 0)
parms <- c(Dn = Dn, Do = Do,An = An)
coord <- seq(0,1, 0.01)
out <- ode(y, coord, mod1, parms)

results = data.frame(coord = as.numeric(out[,"time"]),
                     r = as.numeric(out[,"r"]),
                     C = as.numeric(out[,"C"]))

results$F = as.numeric(1-((results[,"r"])^3)-(results[,"C"]/Do))

  return(tail(results$F,n=1))

rm(y,parms, coord, out,)

}

f(An = 5, Dn = 1.0) # Prueba de Función 1

An = seq(0.5,6,length=100) # Generador de malla eje X
Dn = seq(0.1,10,length=100) # Generador de malla eje Y
outer1 = expand.grid(An, Dn) # Generador de Combinatorios malla

n = dim(outer1)[1] # Dimensiones Totales

for (i in 1:n) { # Generador de Respuesta en Malla
  outer1[i,3] <- f(An = outer1[i,1], Dn = outer1[i,2])
}

outer2 <- na.omit(outer1, 3) # Eliminación de valores no convergentes
plot3d(outer2[,1],outer2[,2],outer1[,3],
       front="line",
       back="line",
       add=TRUE,
       xlab="An",
       ylab="Dn",
       zlab="Biodisponibilidad (F)",
       col="red",
       ticktype="detailed",
       aspect="iso") # Gráfico Final de Resultados
# Animación y toma de fotos
movie3d(spin3d(axis = c(0,0,1), rpm = 10), duration=6,  type = "png")
tempdir()   




