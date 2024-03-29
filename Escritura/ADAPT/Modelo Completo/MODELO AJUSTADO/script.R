

setwd(file.path("F:","Documentos","Estudio - Documentos","Farmacocinética 2012-1","Bioequivalencia (2018)",
                "Escritura","ADAPT","Modelo Completo","MODELO AJUSTADO"))

data = read.csv(file = "BPLT.csv",header = T,sep = ",",dec = ".")


require("ggplot2")

G1 = ggplot() + 
  geom_line(data=data, aes(x=Plot.Time,y=Y.1.,group=Individ.,col=Individ.)) +
  geom_point(data = na.omit(data[,-8]), aes(x=Obser..Time, y = Z.1., group=Individ.,col=Individ.)) +
  labs(title="Modelo Poblacional ADAPT", subtitle = "Parcial BE-BD", 
       x="Tiempo (h)", y = "Concentración (mg/L)")

ggsave(filename = 'Modelo.pdf',plot = G1,device = 'pdf',width = 8,height = 6,units = 'in')


source("modelos_compartimentos.R")

data1 <- data.frame(t0 = seq(0,12.5,0.01),
                    A = cmptm.1.PO(D0 = 700,ka = 1.50,ke = 0.230,Vd = 26.1,t = seq(0,12.5,0.01)),
                    B = 0.997*cmptm.1.PO(D0 = 700,ka = 2.80,ke = 0.230,Vd = 26.1,t = seq(0,12.5,0.01)))

G2 = ggplot() + 
  geom_line(data=data, aes(x=Plot.Time,y=Y.1.,group=Individ.,col=Individ.),alpha=0.5) +
  geom_line(data = data1, aes(x=t0, y = A),size=1.2)+
  geom_line(data = data1, aes(x=t0, y = B),size=1.2)+
  labs(title="Modelo Poblacional ADAPT", subtitle = "Parcial BE-BD", 
       x="Tiempo (h)", y = "Concentración (mg/L)")

ggsave(filename = 'Modelo_2.pdf',plot = G2,device = 'pdf',width = 8,height = 6,units = 'in')


# Residuales --------------------------------------------------------------
df = read.csv(file = "BRSD.csv",header = T,sep = ",",dec = ".")
View(df
    )


G3 = ggplot() + geom_point(data =df,aes(x=Data,y=ModelPred.),col="blue") +
  labs(title="GOF Predicciones Individuales", subtitle = "Parcial BE-BD", 
       x="Predicciones Individuales", y = "Datas Observados")

G4 = ggplot() + geom_point(data =df,aes(x=Data,y=PopModelPred.),col="red")  +
  labs(title="GOF Predicciones Poblacionales", subtitle = "Parcial BE-BD", 
       x="Predicciones Poblacionales", y = "Datas Observados")

ggsave(filename = 'GOF1.pdf',plot = G3,device = 'pdf',width = 7,height = 6,units = 'in')
ggsave(filename = 'GOF2.pdf',plot = G4,device = 'pdf',width = 7,height = 6,units = 'in')


G5 = ggplot() + geom_point(data = df, aes(x=Obser.Time, y = Residual)) +
  labs(title="Gráfico Residuales", subtitle = "Parcial BE-BD", 
       x="Predicciones Poblacionales", y = "Datas Observados")

G6 = ggplot() + geom_point(data = df, aes(x=Obser.Time, y = Std.Resid.))+
  labs(title="Gráfico Residuales Estándar", subtitle = "Parcial BE-BD", 
       x="Predicciones Poblacionales", y = "Datas Observados")

ggsave(filename = 'Residuales1.pdf',plot = G5,device = 'pdf',width = 7,height = 6,units = 'in')
ggsave(filename = 'Residuales2.pdf',plot = G6,device = 'pdf',width = 7,height = 6,units = 'in')















