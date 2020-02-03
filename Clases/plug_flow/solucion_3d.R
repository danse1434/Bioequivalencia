##------------------------------------------------------------------------#
## Nombre del Script: Estudio de modelamiento de Plug-Flow ----------------
##  
## Proposito del Script: realizar un estudio del modelo Plug-Flow para enten
## der la relevancia del numero de dosis, número de absorcion, etc..
##
## Gráficos 3D con modelo plug-flow
##  
## Autor: Daniel S. Parra Gonzalez 
## Fecha de creacion:  24-10-2019 
##  
## Copyright (c) Daniel S. Parra, 2018 
##  
## Email: dsparrag@unal.edu.co 
##------------------------------------------------------------------------#
##########################################################################-
# Apertura de paquetes
library(rgl) # Se abre un paquete útil para realiza gráficos en 3D
library(deSolve)
library(tidyverse)
library(Rcpp)

##########################################################################-
# Definición de valores iniciales
An = 5.0 # N. de absorción
Dn = 1.0 # N. de dosis

##########################################################################-
# Función de Biodisponibilidad --------------------------------------------
##########################################################################-
##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##  1 Especificar funcion (g) para obtener biodisponibilidad teniendo en 
##  cuenta numeros adimensionales Dn, An, Do
##  2 Escribir sistema EDO para modelo plug and flow, escribir en objeto 
##  *mod1* que actua como funcion que tiene como parametros coord, y, parms
##  3 Escribir las condiciones iniciales, r: diametro de particula, y C: es
##  concentracion. En el tiempo = 0 se tiene que r es 1, y C es 0.
##  4 Escribir parametros de modelo Dn, Do, y An
##  5 Escribir sistemas de coordenadas para modelar en el sistema ODE
##  6 Modelar con la funcion ode del paquete deSolve
##  7 Escribir los resultados en forma de tabla
##  8 Modificar la tabla como numeros, y calcular F que es la biodisponibi-
##  lidad calculada (F)
##  9 Retornar como resultado principal, el último valor de F obtenido en la
##  base de datos.
##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

f = function(Dn, An, Do) {
  mod1 <- function(coord, y , parms) {
    with(as.list(c(y, parms)), {
      dr = -(Dn / 3) * ((1 - C) / r) * C
      dC = (Dn * Do * r * (1 - C)) - (2 * An * C)
      return(list(c(dr, dC)))
    })
  }
  # Condiciones iniciales
  y <- c(r = 1, C = 0)
  # Parámetros del modelo
  parms <- c(Dn = Dn, Do = Do, An = An)
  # Sistema de coordenada
  coord <- seq(0, 1, 0.01)
  
  # Modelamiento de ecuaciones diferenciales ordinarias
  out <- deSolve::ode(y = y, times = coord, func = mod1, parms = parms)

  results = data.frame(coord = out[, "time"],
                       r = out[, "r"],
                       C = out[, "C"])
  
  # Resultados de Biodisponibilidad
  results <- results %>%
    dplyr::mutate_all(as.numeric) %>% 
    dplyr::mutate(F = 1 - r ^ 3 - (C / Do))
  
  results %>% 
    magrittr::use_series("F") %>% 
    tail(., n = 1) %>% 
    return(.)
}

# microbenchmark::microbenchmark(f(An = 5, Dn = 1.0, Do = 2))

##########################################################################-
# Gráfico de contorno -----------------------------------------------------
##########################################################################-
##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##  1 Escribir una tabla con valores en escala logarítmica
##  2 Transformar los valores de parametros a escala ordinaria
##  3 Vectorizar la funcion f en los parametros a explorar, en este caso Do,
##  y Dn.
##  4 Realizar un producto outer entre los dos vectores, para conocer todas
##  las combinaciones. Calcular el parametro f para cada combinacion de 
##  parametros.
##  5 Abrir entorno 3D, y abrir la perspectiva 3D
##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

dx = data.frame(
  log_Dn = seq(-2, 2, length = 1e1), 
  log_Do = seq(-2, log10(500), length = 1e1))

dx <- dx %>%
  mutate(Dn = 10 ^ log_Dn,
         Do = 10 ^ log_Do)

f <- Vectorize(f, vectorize.args = c('Do', 'Dn'))

OUT <- outer(dx$Dn, dx$Do, f, An = 2)

open3d()
persp3d(x = dx$Dn, y = dx$Do, z = OUT,
        front = "line", back = "line", 
        xlab = 'Dn', ylab = 'Do')
rgl.postscript('persp3d.pdf', 'pdf')

# Gráfico de contorno
contour(An, Dn, OUT)
