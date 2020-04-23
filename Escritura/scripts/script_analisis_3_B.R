### PROYECTO PARCIAL 1 ### 
### BIODISPONIBILIDAD Y BIOEQUIVALENCIA 2018 - 2### 
### DEPARTAMENTO DE FARMACIA ###
### UNIVERSIDAD NACIONAL DE COLOMBIA ###

# Apertura de paquetes
library("ggplot2")
library("plyr")
library("reshape2")

# Apertura de datos
setwd(file.path("F:","Documentos","Estudio - Documentos","Farmacocinética 2012-1",
                "Bioequivalencia (2018)","Escritura"))
data = read.csv("./data/data_example_1_bioequivalence.csv",header=T,sep = ",",dec =".")

# Apertura de modelos PK
source("./scripts/modelos_compartimentos.R",echo=FALSE)


# Regresión No Lineal de Datos Administración Peroral --------------------------
m0 = length(unique(subset(data,Type == "B")$ID)) # Número de Individuos
list_reg3 = list() # Creación de Lista

for (i in 1:m0) {
  list_reg3[[i]] = nls(Conc. ~ cmptm.1.PO(D0=Dosis, ka, ke, Vd, t=Time),
                       data=subset(data,Type == "B" & ID == i),
                       start = list(ka=1.504, ke=0.22972, Vd=26.27))
  }

# Obtener todos los resúmenes de modelos no lineales
list_reg3_summaries = lapply(list_reg3, function(x) summary(x))
# Obtener todos los R^2 de correlación para modelos
list_reg3_correlations = list()
for (i in 1:m0) {
  list_reg3_correlations[[i]] = cor(subset(data,Type == "B" & ID == i)$Conc., 
                                     predict(list_reg3[[i]]),method="pearson")
}
# Obtener todos los perfiles para modelos no lineales
list_reg_3_profiles = lapply(list_reg3, function(x) profile(x, alpha = 0.05))
  
# Predicciones
tt <- c(seq(0,2,length=40),seq(2,13,length=40))

list_reg3_predictions = list()
  for (i in 1:m0) {
    list_reg3_predictions[[i]]=predict(list_reg3[[i]], 
                                           newdata=list(Dosis= 700, Time = tt),
                                           interval="confidence")
    }

# Dataframe para Graficar
df.reg3.1 = data.frame(ID = rep(unique(subset(data,Type=="B")$ID),each=length(tt)),
                       ID2 = rep(unique(subset(data,Type=="B")$ID2),each=length(tt)),
                       Time = tt,
                       CPRED = unlist(list_reg3_predictions))
# Gráfico en Páneles por Individuo
graph.reg1.2 = ggplot(subset(data,Type=="B"),aes(Time,Conc.,group=ID2))+
  geom_point(col="black",fill=NA)+
  facet_wrap( ~ ID2,scales = "fixed",nrow=4,ncol=3)+
  guides(fill=guide_legend(title="TIPO"))+
  labs(x="Tiempo (horas)", y=expression('Concentración ('*mu*'g/mL)')) +
  scale_x_continuous(sec.axis=dup_axis(name=NULL,labels = NULL),
                     breaks = c(seq(0,14,by=2)),
                     limits=c(0,13)) +
  scale_y_continuous(sec.axis=dup_axis(name=NULL,labels = NULL), 
                     breaks = c(seq(0,30,by=5)),
                     limits=c(0,25))+
  scale_color_manual(values=c("red4","blue4"),name="Sujeto")+
  geom_line(data=df.reg3.1, aes(x=df.reg3.1$Time,y=df.reg3.1$CPRED))+
  theme(panel.grid = element_line(colour="gray90",size =0.5),
        legend.text = element_blank(),
        panel.background=element_rect(colour = NA,fill = NA),
        panel.border = element_rect(colour = "black",fill = NA),
        strip.background = element_rect(colour="black",fill="green1"),
        strip.text.x = element_text(face="bold"))
# Gráfico de Spaguetti 1
graph.reg2.2 = ggplot(df.reg3.1,aes(group=as.factor(ID),col=as.factor(ID)))+
  geom_point(data=subset(data,Type=="B"),aes(x=subset(data,Type=="B")$Time,y=subset(data,Type=="B")$Conc.,col=as.factor(subset(data,Type=="B")$ID)),shape=16,size=1)+
  geom_line(aes(x=Time,y=CPRED,col=as.factor(ID)),size=0.3)+
  guides(colour=guide_legend(title="Sujeto",ncol=2))+
  labs(x="Tiempo (horas)", y=expression('Concentración ('*mu*'g/mL)')) +
  scale_x_continuous(sec.axis=dup_axis(name=NULL,labels = NULL),
                     breaks = c(seq(0,14,by=2)),
                     limits=c(0,13)) +
  scale_y_continuous(sec.axis=dup_axis(name=NULL,labels = NULL), 
                     breaks = c(seq(0,30,by=5)),
                     limits=c(0,NA))+
  theme(panel.grid = element_line(colour="gray98",size =0.25),
        panel.background=element_rect(colour = NA,fill = NA),
        panel.border = element_rect(colour = "black",fill = NA))+
  scale_colour_brewer(palette = "Paired",name="Sujeto")
# Gráfico de Spaguetti 2
graph.reg3.2 = ggplot(df.reg3.1,aes(group=as.factor(ID),col=as.factor(ID)))+
  geom_point(data=subset(data,Type=="B"),aes(x=subset(data,Type=="B")$Time,y=subset(data,Type=="B")$Conc.,col=as.factor(subset(data,Type=="B")$ID)),shape=16,size=1)+
  geom_line(aes(x=Time,y=CPRED,col=as.factor(ID)),size=0.3)+
  guides(colour=guide_legend(title="Sujeto",ncol=2))+
  labs(x="Tiempo (horas)", y=expression('Concentración ('*mu*'g/mL)')) +
  scale_x_continuous(sec.axis=dup_axis(name=NULL,labels = NULL),
                     breaks = c(seq(0,14,by=2))) +
  scale_y_continuous(sec.axis=dup_axis(name=NULL,labels = NULL), 
                     breaks = c(seq(1,10,by=1),seq(10,100,by=10)),trans = "log10")+
  coord_cartesian(xlim=c(0,13),ylim = c(1.5E-00,30))+
  theme(panel.grid = element_line(colour="gray98",size =0.25),
        panel.background=element_rect(colour = NA,fill = NA),
        panel.border = element_rect(colour = "black",fill = NA))+
  scale_colour_brewer(palette = "Paired",name="Sujeto")

# Resumen de Parámetros
l1 = length(list_reg3)
v1 = vector(length = l1)
v2 = vector(length = l1)
v3 = vector(length = l1)
v4 = vector(length = l1)
v5 = vector(length = l1)
v6 = vector(length = l1)
v7 = vector(length = l1)
v8 = vector(length = l1)
v9 = vector(length = l1)
v10 = vector(length = l1)
for (i in 1:l1) {
  v1[i] = list_reg3_summaries[[i]]$parameters[1,1]
  v2[i] = list_reg3_summaries[[i]]$parameters[1,2]
  v3[i] = list_reg3_summaries[[i]]$parameters[2,1]
  v4[i] = list_reg3_summaries[[i]]$parameters[2,2]
  v5[i] = list_reg3_summaries[[i]]$parameters[3,1]
  v6[i] = list_reg3_summaries[[i]]$parameters[3,2]
  v7[i] = list_reg3_summaries[[i]]$sigma
  v8[i] = list_reg3_correlations[[i]]
  v9[i] = sqrt(deviance(list_reg3[[i]])/df.residual(list_reg3[[i]])) # RSE
  v10[i] = df.residual(list_reg3[[i]])
  }


A3 = data.frame(ID = unique(data$ID),
               ID2 = unique(data$ID2),
               D0 = rep(700,l1),
               ka.value = v1,
               ka.sd = v2,
               ke.value = v3,
               ke.sd = v4,
               V1.value = v5,
               V1.sd = v6,
               CL.value = v3*v5,
               RSE = v9,
               DF = v10,
               Sigma = v7,
               R2 = v8); A3

A3$t.vida.media = 0.693/A3$ke.value
#### Matriz para cálculo AUC
B3 = dcast(subset(data,Type == "B")[,c(1:5)], Time ~ ID, value.var="Conc.")
# 
{attach(B3)
B3 <- B3[order(Time),]
detach(B3)  }
B3
# Cálculo de AUC
AUC3 = matrix(nrow=dim(B3)[1],ncol=(dim(B3)[2])-1)
for (i in (1:dim(B3)[1])) {
  for (j in (2:dim(B3)[2])) {
    if (i < (dim(B3)[1])) {
      AUC3[i,(j-1)] = (B3[i,j]+B3[(i+1),j])*(B3[i+1,1]-B3[i,1])/2  # Acá se toman todos los datos experimentales para obtener la curva
    } else {
      AUC3[i,(j-1)] = (B3[i,j]/A3[(j-1),6]) # En el último punto, se hace una extrapolación
    }
  }
}
# Adición de AUC a matriz A
A3$AUC.trunc = apply(AUC3[-11,],2,sum)
A3$AUC.total = apply(AUC3,2,sum)
# Cálculo de T max y C max
A3$tmax = (log(A3$ka.value)-log(A3$ke.value))/(A3$ka.value-A3$ke.value)

Cmax1 = list()
for (i in 1:m0) {
  Cmax1[[i]]=predict(list_reg3[[i]], newdata=list(Dosis= 700, Time = A3$tmax[i]))
}
A3$cmax = c(unlist(Cmax1))


