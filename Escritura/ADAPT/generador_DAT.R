# PROYECTO PARCIAL 1
library(plyr)

setwd("C:/Users/ACER/Desktop/ADAPT")
m = 12 # Número de Archivos de Datos a General (Constante) ##############
data = read.csv("data_example_1_bioequivalence.csv",header = T,sep = ",")

line.data = function(x, i, t){
      paste(data[data$Type == x & data$ID == i & data$Time == t,]["Time"],",",
            data[data$Type == x & data$ID == i & data$Time == t,]["Conc."],", ",sep="")}

# Creación de Archivos de Datos #
for(i in 1:m){
  fileConn = file(paste("data_A",i,".dat",sep=""))
  writeLines(c(paste("Sujeto ",i,"IV,,",sep=""),
    paste("1",",,",sep=""),
    paste("1",",,",sep=""),
    paste("2",",,",sep=""),
    paste("0,280000,0",sep=""),
    paste("0.0005,0,0",sep=""),
    paste("1",",,",sep=""),
    paste("10",",,",sep=""),
    line.data("IV",i,t=0.5),
    line.data("IV",i,t=1.0),
    line.data("IV",i,t=1.5),
    line.data("IV",i,t=2.0),
    line.data("IV",i,t=3.0),
    line.data("IV",i,t=4.0),
    line.data("IV",i,t=6.0),
    line.data("IV",i,t=8.0),
    line.data("IV",i,t=10.0),
    line.data("IV",i,t=12.0),
    paste("Sujeto ",i,"A,,",sep=""),
    paste("1",",,",sep=""),
    paste("1",",,",sep=""),
    paste("1",",,",sep=""),
    paste("0,0,700",sep=""),
    paste("1",",,",sep=""),
    paste("11",",,",sep=""),
    line.data("A",i,t=0.0),
    line.data("A",i,t=0.5),
    line.data("A",i,t=1.0),
    line.data("A",i,t=1.5),
    line.data("A",i,t=2.0),
    line.data("A",i,t=3.0),
    line.data("A",i,t=4.0),
    line.data("A",i,t=6.0),
    line.data("A",i,t=8.0),
    line.data("A",i,t=10.0),
    line.data("A",i,t=12.0)),fileConn)
  close(fileConn)}

# Creación de Archivos de Datos #
for(i in 1:m){
  fileConn = file(paste("data_B",i,".dat",sep=""))
  writeLines(c(
    paste("Sujeto ",i,"IV,,",sep=""),
    paste("1",",,",sep=""),
    paste("1",",,",sep=""),
    paste("2",",,",sep=""),
    paste("0,280000,0",sep=""),
    paste("0.0005,0,0",sep=""),
    paste("1",",,",sep=""),
    paste("10",",,",sep=""),
    line.data("IV",i,t=0.5),
    line.data("IV",i,t=1.0),
    line.data("IV",i,t=1.5),
    line.data("IV",i,t=2.0),
    line.data("IV",i,t=3.0),
    line.data("IV",i,t=4.0),
    line.data("IV",i,t=6.0),
    line.data("IV",i,t=8.0),
    line.data("IV",i,t=10.0),
    line.data("IV",i,t=12.0),
    paste("Sujeto ",i,"A,,",sep=""),
    paste("1",",,",sep=""),
    paste("1",",,",sep=""),
    paste("1",",,",sep=""),
    paste("0,0,700",sep=""),
    paste("1",",,",sep=""),
    paste("11",",,",sep=""),
    line.data("B",i,t=0.0),
    line.data("B",i,t=0.5),
    line.data("B",i,t=1.0),
    line.data("B",i,t=1.5),
    line.data("B",i,t=2.0),
    line.data("B",i,t=3.0),
    line.data("B",i,t=4.0),
    line.data("B",i,t=6.0),
    line.data("B",i,t=8.0),
    line.data("B",i,t=10.0),
    line.data("B",i,t=12.0)),fileConn)
    close(fileConn)}
