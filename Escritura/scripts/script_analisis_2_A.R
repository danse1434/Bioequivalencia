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
m0 = length(unique(subset(data,Type == "A")$ID)) # Número de Individuos
list_reg2 = list() # Creación de Lista

for (i in 1:m0) {
  list_reg2[[i]] = nls(Conc. ~ cmptm.1.PO(D0=Dosis, ka, ke, Vd, t=Time),
                       data=subset(data,Type == "A" & ID == i),
                       start = list(ka=1.504, ke=0.22972, Vd=26.27))
  }

# Obtener todos los resúmenes de modelos no lineales
list_reg2_summaries = lapply(list_reg2, function(x) summary(x))
# Obtener todos los R^2 de correlación para modelos
list_reg2_correlations = list()
for (i in 1:m0) {
  list_reg2_correlations[[i]] = cor(subset(data,Type == "A" & ID == i)$Conc., 
                                     predict(list_reg2[[i]]),method="pearson")
}
# Obtener todos los perfiles para modelos no lineales
list_reg_2_profiles = lapply(list_reg2, function(x) profile(x, alpha = 0.05))
  
# Predicciones
tt <- c(seq(0,2,length=40),seq(2,13,length=40))

list_reg2_predictions = list()
  for (i in 1:m0) {
    list_reg2_predictions[[i]]=predict(list_reg2[[i]], 
                                           newdata=list(Dosis= 700, Time = tt),
                                           interval="confidence")
    }

# Dataframe para Graficar
df.reg1.2 = data.frame(ID = rep(unique(subset(data,Type=="A")$ID),each=length(tt)),
                       ID2 = rep(unique(subset(data,Type=="A")$ID2),each=length(tt)),
                       Time = tt,
                       CPRED = unlist(list_reg2_predictions))
# Gráfico en Páneles por Individuo
graph.reg1.1 = ggplot(subset(data,Type=="A"),aes(Time,Conc.,group=ID2))+
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
  geom_line(data=df.reg1.2, aes(x=df.reg1.2$Time,y=df.reg1.2$CPRED))+
  theme(panel.grid = element_line(colour="gray90",size =0.5),
        legend.text = element_blank(),
        panel.background=element_rect(colour = NA,fill = NA),
        panel.border = element_rect(colour = "black",fill = NA),
        strip.background = element_rect(colour="black",fill="lightblue1"),
        strip.text.x = element_text(face="bold"))
# Gráfico de Spaguetti 1
graph.reg2.1 = ggplot(df.reg1.2,aes(group=as.factor(ID),col=as.factor(ID)))+
  geom_point(data=subset(data,Type=="A"),aes(x=subset(data,Type=="A")$Time,y=subset(data,Type=="A")$Conc.,col=as.factor(subset(data,Type=="A")$ID)),shape=16,size=1)+
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
graph.reg3.1 = ggplot(df.reg1.2,aes(group=as.factor(ID),col=as.factor(ID)))+
  geom_point(data=subset(data,Type=="A"),aes(x=subset(data,Type=="A")$Time,y=subset(data,Type=="A")$Conc.,col=as.factor(subset(data,Type=="A")$ID)),shape=16,size=1)+
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
l1 = length(list_reg2)
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
  v1[i] = list_reg2_summaries[[i]]$parameters[1,1]
  v2[i] = list_reg2_summaries[[i]]$parameters[1,2]
  v3[i] = list_reg2_summaries[[i]]$parameters[2,1]
  v4[i] = list_reg2_summaries[[i]]$parameters[2,2]
  v5[i] = list_reg2_summaries[[i]]$parameters[3,1]
  v6[i] = list_reg2_summaries[[i]]$parameters[3,2]
  v7[i] = list_reg2_summaries[[i]]$sigma
  v8[i] = list_reg2_correlations[[i]]
  v9[i] = sqrt(deviance(list_reg2[[i]])/df.residual(list_reg2[[i]])) # RSE
  v10[i] = df.residual(list_reg2[[i]])
  }


A1 = data.frame(ID = unique(data$ID),
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
               R2 = v8); A1

A1$t.vida.media = 0.693/A1$ke.value
#### Matriz para cálculo AUC
B1 = dcast(subset(data,Type == "A")[,c(1:5)], Time ~ ID, value.var="Conc.")
# 
{attach(B1)
B1 <- B1[order(Time),]
detach(B1)  }
B1
# Cálculo de AUC
AUC1 = matrix(nrow=dim(B1)[1],ncol=(dim(B1)[2])-1)
for (i in (1:dim(B1)[1])) {
  for (j in (2:dim(B1)[2])) {
    if (i < (dim(B1)[1])) {
      AUC1[i,(j-1)] = (B1[i,j]+B1[(i+1),j])*(B1[i+1,1]-B1[i,1])/2  # Acá se toman todos los datos experimentales para obtener la curva
    } else {
      AUC1[i,(j-1)] = (B1[i,j]/A1[(j-1),6]) # En el último punto, se hace una extrapolación
    }
  }
}
# Adición de AUC a matriz A
A1$AUC.trunc = apply(AUC1[-11,],2,sum)
A1$AUC.total = apply(AUC1,2,sum)
# Cálculo de T max y C max
A1$tmax = (log(A1$ka.value)-log(A1$ke.value))/(A1$ka.value-A1$ke.value)

Cmax = list()
for (i in 1:m0) {
  Cmax[[i]]=predict(list_reg2[[i]], newdata=list(Dosis= 700, Time = A1$tmax[i]))
}
A1$cmax = c(unlist(Cmax))








