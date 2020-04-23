### BIODISPONIBILIDAD Y BIOEQUIVALENCIA 2018-2
### DEPARTAMENTO DE FARMACIA - UNIVERSIDAD NACIONAL DE COLOMBIA ###

# Apertura de paquetes
library("ggplot2")
library("plyr")
library("reshape")
library("reshape2")
library("tables")

# Apertura de datos
setwd(file.path("F:","Documentos","Estudio - Documentos","Farmacocinética 2012-1",
                "Bioequivalencia (2018)","Escritura"))
data = read.csv("./data/data_example_1_bioequivalence.csv",header=T,sep = ",",dec =".")

# Apertura de modelos PK
source("./scripts/modelos_compartimentos.R",echo=FALSE)
source("./scripts/script_analisis_1_IV.R",echo=FALSE)
source("./scripts/script_analisis_2_A.R",echo=FALSE)
source("./scripts/script_analisis_3_B.R",echo=FALSE)

# Cálculo de Biodisponibilidad
A1$Tipo = rep("A",length=m0)
A1$F.trunc = (A1$AUC.trunc/700)/(A$AUC.trunc/140)
A1$F.total = (A1$AUC.total/700)/(A$AUC.total/140)
A1$Vd.value = A1$V1.value*A1$F.total
A1$CL.value = A1$ke.value*A1$Vd.value

A3$Tipo = rep("B",length=m0)
A3$F.trunc = (A3$AUC.trunc/700)/(A$AUC.trunc/140)
A3$F.total = (A3$AUC.total/700)/(A$AUC.total/140)
A3$Vd.value = A3$V1.value*A3$F.total
A3$CL.value = A3$ke.value*A3$Vd.value

A$Tipo = rep("IV",length=m0)

# Unión de los Resultados en Data.Frame
LOL = merge(A1,A3, all=T)  # Sólo producto y referencia
LOL2 = rbind.fill(A,A1,A3) # Todos los productos

# Resúmen de Parámetros Farmacocinéticos por Tipo de Producto -------------
{VA1 = aggregate(x=LOL2[,c(4, 6, 8, 14, 18, 15, 16)],by=list(LOL2$Tipo),FUN=mean) # Media aritmética, la de toda la vida
VA2 = aggregate(x=LOL2[,c(4, 6, 8, 14, 18, 15, 16)],by=list(LOL2$Tipo),FUN=sd) # Desviación estándar
VA3 = aggregate(x=LOL2[,c(4, 6, 8, 14, 18, 15, 16)],by=list(LOL2$Tipo),FUN=var) # Varianza
VA4 = aggregate(x=LOL2[,c(4, 6, 8, 14, 18, 15, 16)],by=list(LOL2$Tipo),function(x){{prod(x)^(1/length(x))}}) # Media geométrica
VA5 = aggregate(x=LOL2[,c(4, 6, 8, 14, 18, 15, 16)],by=list(LOL2$Tipo),function(x){sd(x)*100/mean(x)}) # Coeficiente de variación (%)
VA6 = aggregate(x=LOL2[,c(4, 6, 8, 14, 18, 15, 16)],by=list(LOL2$Tipo),function(x){mean(log(x))}) # media del logaritmo
VA7 = aggregate(x=LOL2[,c(4, 6, 8, 14, 18, 15, 16)],by=list(LOL2$Tipo),function(x){var(log(x))}) # varianza del logaritmo
VA = rbind(VA1,VA2,VA3,VA4,VA5, VA6, VA7)} ### Creación de Marco de Datos General# Cálculo de Intervalos de Confianza para Parámetros Farmacocinéticos
M <- matrix(ncol=8,nrow=3)

for (i in 1:3) {
  for (j in 2:8) {
    M[i,j] = paste(round(exp(VA6[i,j] - (qt(0.975,df=11)*sqrt((VA7[i,j]/12)+((VA7[i,j]^2)/(2*(12-1)))))),3),
          "-",round(exp(VA6[i,j] + (qt(0.975,df=11)*sqrt((VA7[i,j]/12)+((VA7[i,j]^2)/(2*(12-1)))))),3))
  }
}
M[,1] = c("A","B","IV") #### Nombre de las Filas
# Resumen 
Resume = structure(list(IV = t(round(VA[3,-1],2)), 
               IV.IC = c(M[3,-1]),
               A = t(round(VA[1,-1],2)),
               A.IC = c(M[1,-1]),
               B = t(round(VA[2,-1],2)), 
               B.IC = c(M[2,-1])), 
          class = "data.frame", 
          row.names = c("$V_{D}$","$k_{e}$","$CL_{T}$","$t_{1/2}$","$k_{a}$","$\\textrm{AUC}_{\\textrm{trunc}}$","$\\textrm{AUC}$"))
Resume[5,c(1:2)] = "-"
colnames(Resume) <- c("Producto IV", NA, "Producto A", NA, "Producto B", NA)

# Resúmen de Parámetros Biofarmacéuticos por Tipo de Producto -------------
{
VA8 = aggregate(x=LOL[,c(18,19,21,22)],by=list(LOL$Tipo),FUN=mean) # Media aritmética, la de toda la vida
VA9 = aggregate(x=LOL[,c(18,19,21,22)],by=list(LOL$Tipo),FUN=sd) # Desviación estándar
VA10 = aggregate(x=LOL[,c(18,19,21,22)],by=list(LOL$Tipo),FUN=var) # Varianza
VA11 = aggregate(x=LOL[,c(18,19,21,22)],by=list(LOL$Tipo),function(x){{prod(x)^(1/length(x))}}) # Media geométrica
VA12 = aggregate(x=LOL[,c(18,19,21,22)],by=list(LOL$Tipo),function(x){sd(x)*100/mean(x)}) # Coeficiente de variación (%)
VA13 = aggregate(x=LOL[,c(18,19,21,22)],by=list(LOL$Tipo),function(x){mean(log(x))}) # media del logaritmo
VA14 = aggregate(x=LOL[,c(18,19,21,22)],by=list(LOL$Tipo),function(x){var(log(x))}) # varianza del logaritmo
VB = rbind(VA8,VA9,VA10,VA11,VA12,VA13,VA14)} ### Creación de Marco de Datos General
colnames(VB) <- c("Tipo","tmax","cmax","F.trunc","F.total")
M1 <- matrix(ncol=5,nrow=2)



for (i in 1:2) {
  for (j in 2:5) {
    M1[i,j] = paste(round(exp(VA13[i,j] - (qt(0.975,df=11)*sqrt((VA14[i,j]/12)+((VA14[i,j]^2)/(2*(12-1)))))),5),
                   "-",round(exp(VA13[i,j] + (qt(0.975,df=11)*sqrt((VA14[i,j]/12)+((VA14[i,j]^2)/(2*(12-1)))))),5))
  }
}
M1[,1] = c("A","B") #### Nombre de las Filas
# Resumen 
Resume1 = structure(list(A = t(round(VB[1,-1],3)),
                        A.IC = c(M1[1,-1]),
                        B = t(round(VB[2,-1],3)), 
                        B.IC = c(M1[2,-1])), 
                   class = "data.frame", 
                   row.names = c("$t_{max}$","$C_{max}$","$F_{\\textrm{trunc}}$","$F_{\\textrm{total}}$"))
colnames(Resume1) <- c("Producto A", NA, "Producto B", NA)

# ANÁLISIS DE VARIANZA ----------------------------------------------------
# AUC
ANOV1 = aov(F.total ~ as.factor(ID)+as.factor(Tipo), data =LOL)
ANOV1b = aov(F.trunc ~ as.factor(ID)+as.factor(Tipo), data =LOL)
ANOV2 = aov(tmax ~ as.factor(ID)+as.factor(Tipo), data =LOL)
ANOV3 = aov(cmax ~ as.factor(ID)+as.factor(Tipo), data =LOL)

summary(ANOV1)
summary(ANOV1b)
summary(ANOV2)
summary(ANOV3)

MANOV1 = manova(cbind(F.total,tmax,cmax) ~ as.factor(ID)+as.factor(Tipo), data =LOL)


summary(MANOV1, test = c("Pillai", "Wilks", "Hotelling-Lawley", "Roy"))


# GRÁFICOS FINALES ----------------------------------------------------
hm <- matrix(nrow=length(tt),ncol=3)

for (i in (1:length(tt))) {
  hm[i,1]=tt[i]
  hm[i,2]= VB[1,5]*cmptm.1.PO(D0= 700, ka = VA[1,6], ke = VA[1,3], Vd = VA[1,2], t=tt[i])
  hm[i,3]= VB[2,5]*cmptm.1.PO(D0= 700, ka = VA[2,6], ke = VA[2,3], Vd = VA[2,2], t=tt[i])
    
}
hm <- data.frame(hm)
colnames(hm) <- c("Time","A","B")

cols = c("Producto A" = "blue3",
         "Producto B" = "red3")

h1 <- ggplot() +  
  geom_line(data=hm,aes(x=hm$Time, y=hm$A,col="Producto A"),size=1.2) +
  geom_line(data=hm,aes(x=hm$Time, y=hm$B,col="Producto B"),size=1.2) +
  geom_point(data=subset(data,Type=="A"),
             aes(x = subset(data,Type=="A")$Time,y=subset(data,Type=="A")$Conc.,col="Producto A"),size=0.2)+
  geom_point(data=subset(data,Type=="B"),
             aes(x = subset(data,Type=="B")$Time,y=subset(data,Type=="B")$Conc.,col="Producto B"),size=0.2)+
  scale_colour_manual(name="Medicamento",values=cols)+
  theme(panel.grid = element_blank(),
        legend.text = element_text(colour="black"),
        panel.background=element_rect(colour = NA,fill = NA),
        panel.border = element_rect(colour = "black",fill = NA),
        axis.ticks = element_line(colour = "black"),
        axis.text = element_text(colour="black"))+
  scale_x_continuous(breaks=seq(0,14,by=1))+
  scale_y_continuous(breaks=seq(0,25,by=2))+
  labs(x="Tiempo (horas)", y = expression('Concentración ('*mu*"g/mL)"))

pool.A = nls(Conc. ~ cmptm.1.PO(D0=Dosis, ka, ke, Vd, t=Time),
                     data=subset(data,Type == "A"),
                     start = list(ka=1.504, ke=0.22972, Vd=26.27))

pool.B = nls(Conc. ~ cmptm.1.PO(D0=Dosis, ka, ke, Vd, t=Time),
             data=subset(data,Type == "B"),
             start = list(ka=1.504, ke=0.22972, Vd=26.27))

list_reg2[[2]]

t0 <- subset(data,(Type %in% c('A')))$Time
IC.function = function(t0, t1, reg_model){
  reg_model = reg_model
  ka = summary(reg_model)$parameters[1,1]; k10 = summary(reg_model)$parameters[2,1]
  Vd = summary(reg_model)$parameters[3,1]; D = 700
  MSE.1 = summary(reg_model)$sigma
t0 <- as.numeric(t0)
t1 <- as.numeric(t1)
J0 = matrix(nrow = length(t0),ncol=3)
J1 = matrix(nrow = length(t1),ncol=3)
for (i in (1:length(t0))) {
  J0[i,1] = (-D/(Vd^2))*(ka/(ka-k10))*((exp(-k10*t0[i])-exp(-ka*t0[i])))
  J0[i,2] = (D*((exp(-k10*t0[i])-exp(-ka*t0[i])))/(Vd*(ka-k10)))-
    ((D*((exp(-k10*t0[i])-exp(-ka*t0[i])))/Vd)*(ka/(ka-k10)^2))+
    (((D*t0[i]/Vd)*(ka/(ka-k10)))*exp(-ka*t0[i]))
  J0[i,3] = ((D*((exp(-k10*t0[i])-exp(-ka*t0[i])))/Vd)*(ka/(ka-k10)^2))-
    ((D*t0[i]/Vd)*(ka/(ka-k10))*exp(-k10*t0[i]))
}
for (i in (1:length(t1))) {
  J1[i,1] = (-D/(Vd^2))*(ka/(ka-k10))*((exp(-k10*t1[i])-exp(-ka*t1[i])))
  J1[i,2] = (D*((exp(-k10*t1[i])-exp(-ka*t1[i])))/(Vd*(ka-k10)))-
    ((D*((exp(-k10*t1[i])-exp(-ka*t1[i])))/Vd)*(ka/(ka-k10)^2))+
    (((D*t1[i]/Vd)*(ka/(ka-k10)))*exp(-ka*t1[i]))
  J1[i,3] = ((D*((exp(-k10*t1[i])-exp(-ka*t1[i])))/Vd)*(ka/(ka-k10)^2))-
    ((D*t1[i]/Vd)*(ka/(ka-k10))*exp(-k10*t1[i]))
}
R0 = qr.R(qr(J0))
CB.1 = sqrt(MSE.1)*norm(J1%*%solve(R0))*qt(p=(1-0.025),df=(12-3))
return(CB.1)}

hm$A.LI = hm$A-IC.function(t0,hm$Time,list_reg2[[1]])
hm$A.LS = hm$A+IC.function(t0,hm$Time,list_reg2[[1]])

hm$B.LI = hm$B-IC.function(t0,hm$Time,list_reg3[[1]])
hm$B.LS = hm$B+IC.function(t0,hm$Time,list_reg3[[1]])

h2 = h1 + 
  geom_ribbon(data=hm, aes(x=hm$Time,ymin=hm$A.LI,ymax=hm$A.LS),
              alpha=0.1,fill="blue4",col="blue4",size=0.3,linetype="dashed")+
  geom_ribbon(data=hm, aes(x=hm$Time,ymin=hm$B.LI,ymax=hm$B.LS),
              alpha=0.1,fill="red4",col="red4",size=0.3,linetype="dashed")+
  coord_cartesian(xlim=c(0,14), ylim=c(0,25))

# Inferencia de Efectos ---------------------------------------------------
#write.csv(A3, "./results/A3.csv")
#write.csv(A3, "./results/A3.csv")
#Apertura de archivo con resumen de parámetros biofarmacéuticos, que se ha modificado para añadir secuencias, periodos
df1 = read.csv("./data/summary_data.csv",header=T,sep = ",",dec =".")
#Apertura de archivo con UDF para obtener inferencias
source("./scripts/script_analisis_5_funcion.R",echo=FALSE)
Infer1 <- bioequiv_inference(x="AUC.total",n1=6,n2=6,data=df1)
Infer2 <- bioequiv_inference(x="cmax",n1=6,n2=6,data=df1)
Infer3 <- bioequiv_inference(x="F.total",n1=6,n2=6,data=df1)
# ANOVA de Medidas Repetidas ---------------------------------------------------
df1[["ID"]] <- as.factor(df1[["ID"]])
df1[["Secuencia"]] <- as.factor(df1[["Secuencia"]])
df1[["Periodo"]] <- as.factor(df1[["Periodo"]])
df1[["Formulacion"]] <- as.factor(df1[["Formulacion"]])

ANOVA1 <- aov(AUC.total ~ Formulacion+Periodo+Secuencia+Error(ID/(Formulacion+Periodo)),data=df1)
ANOVA2 <- aov(cmax ~ Formulacion+Periodo+Secuencia+Error(ID/(Formulacion+Periodo)),data=df1)
ANOVA3 <- aov(F.total ~ Formulacion+Periodo+Secuencia+Error(ID/(Formulacion+Periodo)),data=df1)

ANOVA1a <- aov(log(AUC.total) ~ Formulacion+Periodo+Secuencia+Error(ID/(Formulacion+Periodo)),data=df1)
ANOVA2a <- aov(log(cmax) ~ Formulacion+Periodo+Secuencia+Error(ID/(Formulacion+Periodo)),data=df1)
ANOVA3a <- aov(log(F.total) ~ Formulacion+Periodo+Secuencia+Error(ID/(Formulacion+Periodo)),data=df1)


R.ANOVA1 <- anova_latex(data=ANOVA1,digitos = 3)
R.ANOVA2 <- anova_latex(data=ANOVA2,digitos = 3)
R.ANOVA3 <- anova_latex(data=ANOVA3,digitos = 3)

R.ANOVA1a <- anova_latex(data=ANOVA1a,digitos = 3)
R.ANOVA2a <- anova_latex(data=ANOVA2a,digitos = 3)
R.ANOVA3a <- anova_latex(data=ANOVA3a,digitos = 3)


set.seed(2018-12)
Comp.BE.1 <- bioequiv_comparacion(x="AUC.total",n1=6,n2=6,data=df1)
Comp.BE.2 <- bioequiv_comparacion(x="cmax",n1=6,n2=6,data=df1)
Comp.BE.3 <- bioequiv_comparacion(x="F.total",n1=6,n2=6,data=df1)
# calibrador0 = read.csv("./data/calibrador0.csv",header=T,sep = ",",dec =".")
# CAL0 <- bioequiv_comparacion(x="AUC.total",n1=9,n2=9,data=calibrador0)

  

Tmax.compar = wilcox.test(df1[df1$Formulacion == '1',][['tmax']],
                          df1[df1$Formulacion == '2',][['tmax']],
                          paired=T, alternative = "two.sided",conf.level = 0.90)






































# Boxplot de Parámetros Biofarmacéuticos
BPDF = data.frame(ID = c(df1[df1$Formulacion == '1',][['ID']],
                         df1[df1$Formulacion == '2',][['ID']]),
                  FORM = c(df1[df1$Formulacion == '1',][['Formulacion']],
                         df1[df1$Formulacion == '2',][['Formulacion']]),
                  F.total = c(df1[df1$Formulacion == '1',][['F.total']],
                              df1[df1$Formulacion == '2',][['F.total']]),
                  AUC.total = c(df1[df1$Formulacion == '1',][['AUC.total']],
                                df1[df1$Formulacion == '2',][['AUC.total']]),
                  cmax = c(df1[df1$Formulacion == '1',][['cmax']],
                                df1[df1$Formulacion == '2',][['cmax']]),
                  tmax = c(df1[df1$Formulacion == '1',][['tmax']],
                           df1[df1$Formulacion == '2',][['tmax']]))

g1 <- ggplot(data=BPDF, aes(x=as.factor(FORM),y=AUC.total,group=ID)) + 
  geom_boxplot(data=BPDF,aes(x=as.factor(BPDF$FORM),y=BPDF$AUC.total),inherit.aes = FALSE,
               colour="gray90",alpha=0.05)+
  geom_point(aes(col=ID))+geom_line(aes(col=ID))+
  theme_bw() +
  coord_cartesian(ylim = c(90, 150)) +
  scale_y_continuous(breaks = seq(90,200,by=10)) +
  scale_color_gradient(name = "Sujeto",low="blue1",high="blue4") +
  xlab("Producto Farmacéutico") + 
  labs(y = expression("AREA "~AUC[0]^{"\U221E"})) +
  guides(fill=guide_legend(title="Producto"))+
  theme(panel.grid = element_blank(),
        legend.text = element_text(colour="black"),
        panel.background=element_rect(colour = NA,fill = NA),
        panel.border = element_rect(colour = "black",fill = NA),
        axis.ticks = element_line(colour = "black"),
        axis.text = element_text(colour="black"))
  
g2 <- ggplot(data=BPDF, aes(x=as.factor(FORM),y=F.total,group=ID)) + 
  geom_boxplot(data=BPDF,aes(x=as.factor(BPDF$FORM),y=BPDF$F.total),inherit.aes = FALSE,
               colour="gray90",alpha=0.05)+
  geom_point(aes(col=ID))+geom_line(aes(col=ID))+
  theme_bw() +
  coord_cartesian(ylim = c(0.5, 1)) +
  scale_y_continuous(breaks = seq(0,1,by=0.05)) +
  scale_color_gradient(name = "Sujeto",low="green1",high="green4") +
  xlab("Producto Farmacéutico") + 
  labs(y = expression("Biodisponibilidad "~F[T])) +
  guides(fill=guide_legend(title="Producto"))+
  theme(panel.grid = element_blank(),
        legend.text = element_text(colour="black"),
        panel.background=element_rect(colour = NA,fill = NA),
        panel.border = element_rect(colour = "black",fill = NA),
        axis.ticks = element_line(colour = "black"),
        axis.text = element_text(colour="black"))

g3 <- ggplot(data=BPDF, aes(x=as.factor(FORM),y=cmax,group=ID)) + 
  geom_boxplot(data=BPDF,aes(x=as.factor(BPDF$FORM),y=BPDF$cmax),inherit.aes = FALSE,
               colour="gray90",alpha=0.05)+
  geom_point(aes(col=ID))+geom_line(aes(col=ID))+
  theme_bw() +
  coord_cartesian(ylim = c(15, 25)) +
  scale_y_continuous(breaks = seq(15,25,by=1)) +
  scale_color_gradient(name = "Sujeto",low="purple1",high="purple4") +
  xlab("Producto Farmacéutico") + 
  labs(y = expression(~C[max])) +
  guides(fill=guide_legend(title="Producto"))+
  theme(panel.grid = element_blank(),
        legend.text = element_text(colour="black"),
        panel.background=element_rect(colour = NA,fill = NA),
        panel.border = element_rect(colour = "black",fill = NA),
        axis.ticks = element_line(colour = "black"),
        axis.text = element_text(colour="black"))


g4 <- ggplot(data=BPDF, aes(x=as.factor(FORM),y=tmax,group=ID)) + 
  geom_boxplot(data=BPDF,aes(x=as.factor(BPDF$FORM),y=BPDF$tmax),inherit.aes = FALSE,
               colour="gray90",alpha=0.05)+
  geom_point(aes(col=ID))+geom_line(aes(col=ID))+
  theme_bw() +
  coord_cartesian(ylim = c(0.9, 1.52)) +
  scale_y_continuous(breaks = seq(0,5,by=0.05)) +
  scale_color_gradient(name = "Sujeto",low="red1",high="red4") +
  xlab("Producto Farmacéutico") + 
  labs(y = expression(~T[max])) +
  guides(fill=guide_legend(title="Producto"))+
  theme(panel.grid = element_blank(),
        legend.text = element_text(colour="black"),
        panel.background=element_rect(colour = NA,fill = NA),
        panel.border = element_rect(colour = "black",fill = NA),
        axis.ticks = element_line(colour = "black"),
        axis.text = element_text(colour="black"))

