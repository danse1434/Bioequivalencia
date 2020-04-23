# PROYECTO FARMACOCINÉTICA POBLACIONAL DE PIPERACILINA EN UNIDADES DE CUIDADO INTENSIVO
# Simulación de Concentraciones de Piperacilina
library(truncnorm)
library(ggplot2); theme_set(theme_classic())
library(plyr)
library(reshape2)

setwd("C:/Users/ACER/Desktop/ADAPT")
m = 12 # Número de Archivos de Datos a General (Constante) ##############

# Creación de Texto #
for(i in 1:m){
  fileConn = file(paste("data_A",i,".aci",sep=""))
  writeLines(c(paste("data_A",i,".run",sep=""),
               "2           # Compartment number for bolus input",
               "2           # Estimation option number: 1-WLS, 2-ML, 3-MAP",
               "Y           # Estimate (Y/N) Kel       ",
               "Y           # Estimate (Y/N) Vc        ",
               "Y           # Estimate (Y/N) Ka        ",
               "Y           # Estimate (Y/N) F         ",
               "N           # Estimate (Y/N) IC(   1)",
               "N           # Estimate (Y/N) IC(   2)",
               "Y           # Estimate (Y/N) SDinter1  ",
               "Y           # Estimate (Y/N) SDslope1  ",
               "1000        # Maximum number of iterations",
               "y           # Print intermediate iterations (Y/N)",
               "3           # Exit NPD",
               "# Created by NPD"),fileConn)
  close(fileConn)}

# Creación de Texto #
for(i in 1:m){
  fileConn = file(paste("data_B",i,".aci",sep=""))
  writeLines(c(paste("data_B",i,".run",sep=""),
               "2           # Compartment number for bolus input",
               "2           # Estimation option number: 1-WLS, 2-ML, 3-MAP",
               "Y           # Estimate (Y/N) Kel       ",
               "Y           # Estimate (Y/N) Vc        ",
               "Y           # Estimate (Y/N) Ka        ",
               "Y           # Estimate (Y/N) F         ",
               "N           # Estimate (Y/N) IC(   1)",
               "N           # Estimate (Y/N) IC(   2)",
               "Y           # Estimate (Y/N) SDinter1  ",
               "Y           # Estimate (Y/N) SDslope1  ",
               "1000        # Maximum number of iterations",
               "y           # Print intermediate iterations (Y/N)",
               "3           # Exit NPD",
               "# Created by NPD"),fileConn)
  close(fileConn)}
