# FUNCIONES DE BIOEQUIVALENCIA
# Tabla de Inferencias Modelo 2 x 2
bioequiv_inference = function(x,n1,n2,data){
n1 = as.numeric(n1)
n2 = as.numeric(n2)
df1 = as.data.frame(data)

# Efecto de Carry-Over
U.bar..1 = mean(log(df1[df1$Periodo == '1' & df1$Secuencia == '1',][[x]]) + log(df1[df1$Periodo == '2' & df1$Secuencia == '1',][[x]]))
U.bar..2 = mean(log(df1[df1$Periodo == '1' & df1$Secuencia == '2',][[x]]) + log(df1[df1$Periodo == '2' & df1$Secuencia == '2',][[x]]))
U.i1 = log(df1[df1$Periodo == '1' & df1$Secuencia == '1',][[x]]) + log(df1[df1$Periodo == '2' & df1$Secuencia == '1',][[x]])
U.i2 = log(df1[df1$Periodo == '1' & df1$Secuencia == '2',][[x]]) + log(df1[df1$Periodo == '2' & df1$Secuencia == '2',][[x]])
C.hat = U.bar..2-U.bar..1
sigma_u2 = (1/(n1+n2-2))*(sum((U.i1-U.bar..1)^2)+sum((U.i2-U.bar..2)^2))
T_u = C.hat/sqrt(sigma_u2*((1/n1)+(1/n2)))
T_tab = qt(1-(0.05/2),df=(n1+n2-2))
pvalue_u = 2*pt(-abs(T_u),df=(n1+n2-2))
IC.u = T_tab * sqrt(sigma_u2 * ((1/n1)+(1/n2)))
C.IC=paste(round(C.hat-IC.u,4),",", round(C.hat+IC.u,4),sep="")

# Efecto de Formulación 
d.i1 = log(df1[df1$Periodo == '2' & df1$Secuencia == '1',][[x]]) - log(df1[df1$Periodo == '1' & df1$Secuencia == '1',][[x]])
d.i2 = log(df1[df1$Periodo == '2' & df1$Secuencia == '2',][[x]]) - log(df1[df1$Periodo == '1' & df1$Secuencia == '2',][[x]])  
d.bar.1 = mean(d.i1)
d.bar.2 = mean(d.i2)
F.hat.1 = d.bar.1-d.bar.2  # Forma normal de cálculo
sigma_d2 = (1/(n1+n2-2))*(sum((d.i1-d.bar.1)^2)+sum((d.i2-d.bar.2)^2))
T_d.1 = F.hat.1/sqrt(sigma_d2*((1/n1)+(1/n2)))
pvalue_d.1 = 2*pt(-abs(T_d.1),df=(n1+n2-2))
IC.d = T_tab * sqrt(sigma_d2 * ((1/n1)+(1/n2)))
T.IC.1=paste(round(F.hat.1-IC.d,4),",", round(F.hat.1+IC.d,4),sep="")
Y.hat.C = sum(log(df1[df1$Periodo == '2' & df1$Secuencia == '1',][[x]])) + sum(log(df1[df1$Periodo == '1' & df1$Secuencia == '2',][[x]]))
Y.hat.R = sum(log(df1[df1$Periodo == '1' & df1$Secuencia == '1',][[x]])) + sum(log(df1[df1$Periodo == '2' & df1$Secuencia == '2',][[x]]))
F.hat.2 = (1/(n1+n2))*(Y.hat.C-Y.hat.R)  # Forma alternativa de cálculo
T_d.2 = F.hat.2/sqrt(sigma_d2*((1/n1)+(1/n2)))
pvalue_d.2 = 2*pt(-abs(T_d.2),df=(n1+n2-2))
T.IC.2=paste(round(F.hat.2-IC.d,4),",", round(F.hat.2+IC.d,4),sep="")
# Efecto de Periodo
P.hat =d.bar.1 + (d.bar.2)
T_o = P.hat/sqrt(sigma_d2*((1/n1)+(1/n2)))
pvalue_o = 2*pt(-abs(T_o),df=(n1+n2-2))
P.IC = paste(round(P.hat-IC.d,4),",", round(P.hat+IC.d,4),sep="")
# Resultados en Tablas
{MVUE = round(c(C.hat, F.hat.1, F.hat.2, P.hat),4)
MVUE.IC = c(as.name(C.IC),as.name(T.IC.1),as.name(T.IC.2),as.name(P.IC))
TVALUE = round(c(T_u,T_d.1,T_d.2,T_o),4)
PVALUE = formatC(c(pvalue_u,pvalue_d.1,pvalue_d.2,pvalue_o),format="e",digits=3)}
LSA = list(A = MVUE,
B = c(paste("(",MVUE.IC[[1]],")",sep=""), paste("(",MVUE.IC[[2]],")",sep=""), paste("(",MVUE.IC[[3]],")",sep=""), paste("(",MVUE.IC[[4]],")",sep="")),
C = TVALUE,D = PVALUE)

resume5 = structure(.Data=LSA,class = "data.frame", 
          row.names = c("Carry-Over","Formulación 1","Formulación 2","Periodo")); resume5

colnames(resume5) <- c("MVUE","IC95%","T","Valor p") 

sigmas1 = paste("var(d) =",formatC(sigma_d2,format = "E",digits = 4),sep="")
sigmas3 = paste("var(u2) =",formatC(sigma_u2,format = "E",digits = 4),sep="")
sigmasT = paste(sigmas1,sigmas3,sep="; ")
rest <- list("resultados"=resume5, "sigmas"=sigmasT)

return(rest)
}

# Tabla de ANOVA en Latex
anova_latex = function(data, digitos){
  digitos = as.numeric(digitos)
  zT <- summary(data) 
glt = (2*(zT[[1]][[1]]$Df[2]+2)-1); glt # Grados de Libertad Totales
SSt = round((sum(zT[[1]][[1]]$`Sum Sq`)+sum(zT[[2]][[1]]$`Sum Sq`)),digitos) # Suma de CUadrados Totales
MSt = round(SSt/glt,digitos) # Media de Cuadrados Totales
z1 = c("",zT[[1]][[1]]$Df, "",zT[[2]][[1]]$Df,glt)
z2 = c("",round(c(zT[[1]][[1]]$`Sum Sq`),digitos),"",round(zT[[2]][[1]]$`Sum Sq`,digitos),SSt)
z3 = c("",round(c(zT[[1]][[1]]$`Mean Sq`),digitos),"",round(zT[[2]][[1]]$`Mean Sq`,digitos),MSt)
F.v = round((zT[[1]][[1]]$`Mean Sq`[2]/zT[[2]][[1]]$`Mean Sq`[[3]]),digitos)
z4 = c("",round(c(zT[[1]][[1]]$`F value`[-2]),digitos),F.v,"",round(zT[[2]][[1]]$`F value`[-3],digitos),"-","-")
z5 = c("",
  round(qf(p=1-0.05,df1=as.numeric(z1[2]),df2=as.numeric(z1[3])),digitos),
  round((qf(p=(1-(0.05/1)),df1=as.numeric(z1[3]),df2=as.numeric(z1[7]))),digitos),"",
  round((qf(p=1-0.05,df1=as.numeric(z1[5]),df2=as.numeric(z1[7]))),digitos),
  round((qf(p=1-0.05,df1=as.numeric(z1[6]),df2=as.numeric(z1[7]))),digitos),"-","-")
pF.v = pf(q=F.v, df1=as.numeric(z1[3]),df2=as.numeric(z1[7]), lower.tail=FALSE)
z6 = c("",
       formatC(c(zT[[1]][[1]]$`Pr(>F)`[-2]),format = 'E',digits=digitos-1),
       formatC(pF.v,format = 'E',digits=digitos-1),
       "",
       formatC(zT[[2]][[1]]$`Pr(>F)`[-3],format = 'E',digits=digitos-1),
       "-","-")
AOvLISt <- list(z1,z2,z3,z4,z5,z6)
AOVrnames <- c("$\\mathbf{\\textrm{Interindividual}}$","Carry-Over","Residuales",
               "$\\mathbf{\\textrm{Intraindividual}}$","Formulación","Periodo","Residuales ","Totales")
resume8 = structure(.Data=AOvLISt,class = "data.frame", 
                    row.names = AOVrnames); resume8
colnames(resume8) <- c("$g_{L}$","Suma Cuadrados (SC)","Promedio Cuadrados (PC)","$F_{\\textrm{CALC}}$",
                       "$F_{\\textrm{TAB}}$","Valor p")

return(resume8)}


bioequiv_comparacion = function(x,n1,n2,data){
  library(boot); library(ggplot2)
  n1 = as.numeric(n1)
  n2 = as.numeric(n2)
  df1 = as.data.frame(data)
  gl = n1+n2-2 ;   m = (1/n1)+(1/n2)
  dgts1 = 4;   lwd1 = 0.6
  
  y.1.1 = df1[df1$Periodo == '1' & df1$Secuencia == '1',][[x]]
  y.2.1 = df1[df1$Periodo == '2' & df1$Secuencia == '1',][[x]]
  y.1.2 = df1[df1$Periodo == '1' & df1$Secuencia == '2',][[x]]
  y.2.2 = df1[df1$Periodo == '2' & df1$Secuencia == '2',][[x]]
  
  # Efecto de Carry-Over ----------------------------------------------------
  U.bar..1 = mean(log(y.1.1) + log(y.2.1))
  U.bar..2 = mean(log(y.1.2) + log(y.2.2))
  U.i1 = log(y.1.1) + log(y.2.1)
  U.i2 = log(y.1.2) + log(y.2.2)
  C.hat = U.bar..2-U.bar..1
  sigma_u2 = (1/(n1+n2-2))*(sum((U.i1-U.bar..1)^2)+sum((U.i2-U.bar..2)^2))
  # Efecto de Formulación ---------------------------------------------------
  Y_R = (sum(log(y.1.1)) + sum(log(y.2.2)))*(1/(n1+n2))
  Y_T = (sum(log(y.2.1)) + sum(log(y.1.2)))*(1/(n1+n2))
  F.hat.1 = Y_T-Y_R  # Forma normal de cálculo
  
  d.i1 = (y.2.1-y.1.1)/2
  d.i2 = (y.2.2-y.1.2)/2
  
  d.i1.LN = (log(y.2.1)-log(y.1.1))/2
  d.i2.LN = (log(y.2.2)-log(y.1.2))/2
  
  sigma_d2 = (1/(gl))*(sum((d.i1.LN-mean(d.i1.LN))^2)+sum((d.i2.LN-mean(d.i2.LN))^2))
  
  # Efecto de Periodo -------------------------------------------------------
  P.hat =mean(d.i1.LN) + (mean(d.i2.LN))
  # Estimación de Delta MLE -------------------------------------------------
  delta.MLE = exp(F.hat.1)
  delta.LI = exp(F.hat.1 - (qt(1-(0.025*2),df=gl)*sqrt(sigma_d2)*sqrt(m)))
  delta.LS = exp(F.hat.1 + (qt(1-(0.025*2),df=gl)*sqrt(sigma_d2)*sqrt(m)))
  IC.MLE = paste("(",round(delta.LI,dgts1),", ",round(delta.LS,dgts1),")", sep="")
  sesgo.MLE = delta.MLE * (exp(m*sigma_d2/2)-1)
  var.MLE = delta.MLE^2 * (exp(m*sigma_d2)-1) *exp(m*sigma_d2)
  MSE.MLE = var.MLE+(sesgo.MLE^2)
  SSD = gl*sigma_d2
  ################################################################################
  # Creación de Data.Frame para las diferencias entre periodos
  dfb1 = data.frame(Index = seq(1,n1,by=1),
                    d.i1.LN = as.numeric(d.i1.LN),
                    d.i2.LN = as.numeric(d.i2.LN))
  # Creación de funciones para Indices
  d.vec.1 = function(data,indices){
    bar = data[indices,2]; return(bar)}
  d.vec.2 = function(data,indices){
    bar = data[indices,3]; return(bar)}
  # Muestreo de Diferencias por Bootstrap
  bootgh.1 <- boot(dfb1,d.vec.1,R=500)
  bootgh.2 <- boot(dfb1,d.vec.2,R=500)
  # Cálculo de F.hat.1
  MLE.bootstrap = exp(apply(bootgh.1$t, 1, mean)-apply(bootgh.2$t, 1, mean))
  MLE.boots = paste("(",round(dplyr::nth(sort(MLE.bootstrap),(.1/2)*500),4),", ",
                    round(dplyr::nth(sort(MLE.bootstrap),1-(.1/2)*500),4),")",sep="")
  
  p1 = ggplot(as.data.frame(MLE.bootstrap),aes(as.data.frame(MLE.bootstrap)$MLE.bootstrap)) +
    geom_histogram(col="black",fill="red1",bins=30) +
    geom_vline(xintercept = c(0.80,1.25), linetype="solid",size=lwd1/2, col="black")+
    geom_vline(xintercept = c(delta.LI,delta.LS), linetype="dotted",size=lwd1, col="blue2")+
    geom_vline(xintercept = c(dplyr::nth(sort(MLE.bootstrap),(.1/2)*500),
                              dplyr::nth(sort(MLE.bootstrap),1-(.1/2)*500)),
               linetype="dotted",size=lwd1, col="green4")+
    coord_cartesian(ylim=c(0,200), xlim=c(0.7,1.3))+
    scale_x_continuous(breaks=c(seq(0,1.5,by=0.1)))+
    ylab("Frecuencia") + 
    labs(x = expression(~delta[ML]),
         title=paste("MLE IC: t vs bootstrap",x)) +
    theme(panel.grid = element_blank(),
          legend.text = element_text(colour="black"),
          panel.background=element_rect(colour = NA,fill = NA),
          panel.border = element_rect(colour = "black",fill = NA),
          axis.ticks = element_line(colour = "black"),
          axis.text = element_text(colour="black"))
  
  ################################################################################
  # Estimación de Delta MVUE ------------------------------------------------
  neymann.scott.function = function(m,SSD,gl){
    seq.a.1 <- seq(0,160,1)
    seq.a.2 <- (gamma(gl/2)/(gamma((gl/2)+seq.a.1)*factorial(seq.a.1)))*((-(m/4)*SSD)^(seq.a.1))
    return(sum(seq.a.2))
  }
  
  delta.MVUE = delta.MLE * neymann.scott.function(m, SSD, gl)
  var.MVUE = (delta.MLE^2)*((neymann.scott.function(m,SSD,gl)^2)-neymann.scott.function(4*m,SSD,gl))
  eff.MVUE = (var.MLE/var.MVUE)*100 # Porcentaje Eficiencia Comparativa
  
  SSD.bootstrap = gl*(1/(n1+n2-2))*(apply((bootgh.1$t-apply(bootgh.1$t, 1, mean))^2,1,sum)+
                                      apply((bootgh.2$t-apply(bootgh.2$t, 1, mean))^2,1,sum))
  
  NSF <- vector(length = length(SSD.bootstrap))
  
  for (i in (1:length(SSD.bootstrap))) {
    NSF[i] = neymann.scott.function(m,SSD.bootstrap[i],gl)  
  }
  
  MVUE.bootstrap = MLE.bootstrap*NSF
  MVUE.boots = paste("(",round(dplyr::nth(sort(MVUE.bootstrap),(.1/2)*500),4),", ",
                     round(dplyr::nth(sort(MVUE.bootstrap),1-(.1/2)*500),4),")",sep="")
  
  p2 = ggplot(as.data.frame(MVUE.bootstrap),aes(as.data.frame(MVUE.bootstrap)$MVUE.bootstrap)) +
    geom_histogram(col="black",fill="blue1",bins=30) +
    geom_vline(xintercept = c(0.80,1.25), linetype="solid",size=lwd1/2, col="black")+
    geom_vline(xintercept = c(dplyr::nth(sort(MVUE.bootstrap),(.1/2)*500),
                              dplyr::nth(sort(MVUE.bootstrap),1-(.1/2)*500)),
               linetype="dotted",size=lwd1, col="green4")+
    coord_cartesian(ylim=c(0,200), xlim=c(0.7,1.3))+
    scale_x_continuous(breaks=c(seq(0,1.5,by=0.1)))+
    ylab("Frecuencia") + 
    labs(x = expression(~delta[MVUE]),
         title=paste("MVUE IC: bootstrap",x)) +
    theme(panel.grid = element_blank(),
          legend.text = element_text(colour="black"),
          panel.background=element_rect(colour = NA,fill = NA),
          panel.border = element_rect(colour = "black",fill = NA),
          axis.ticks = element_line(colour = "black"),
          axis.text = element_text(colour="black"))
  
  # Estimación de Delta ISR (MIR) -------------------------------------------
  
  r.i1 = y.2.1/y.1.1
  r.i2 = y.1.2/y.2.2
  delta.MIR = (mean(r.i1) + mean(r.i2))/2
  sesgo.MIR = 0.5 * delta.MLE*((exp(2*sigma_d2)*(exp(-P.hat)+exp(P.hat)))-2)
  
  V.mir.asterisk = exp((2*F.hat.1)+(4*sigma_d2))*exp((4*sigma_d2)-1)
  
  var.MIR = 0.25*V.mir.asterisk*(((1/n1)*exp(2*P.hat))+((1/n2)*exp(2*P.hat)))
  MSE.MIR = var.MIR+(sesgo.MIR^2)
  #############################################################################
  #############################################################################
  r.df <- data.frame(r.i1=c(r.i1),
                     r.i2=c(r.i2))
  
  # Creación de funciones para Indices
  d.vec.3 = function(data,indices){
    bar = data[indices,1]; return(bar)}
  d.vec.4 = function(data,indices){
    bar = data[indices,2]; return(bar)}
  # Muestreo de Diferencias por Bootstrap
  bootgh.3 <- boot(r.df,d.vec.3,R=500)
  bootgh.4 <- boot(r.df,d.vec.4,R=500)
  # Cálculo de F.hat.1
  MIR.bootstrap = (apply(bootgh.3$t, 1, mean)+apply(bootgh.4$t, 1, mean))/2
  MIR.boots = paste("(",round(dplyr::nth(sort(MIR.bootstrap),(.1/2)*500),4),", ",
                    round(dplyr::nth(sort(MIR.bootstrap),1-(.1/2)*500),4),")",sep="")
  
  p3 = ggplot(as.data.frame(MIR.bootstrap),aes(as.data.frame(MIR.bootstrap)$MIR.bootstrap)) +
    geom_histogram(col="black",fill="green1",bins=30) +
    geom_vline(xintercept = c(0.80,1.25), linetype="solid",size=lwd1/2, col="black")+
    geom_vline(xintercept = c(dplyr::nth(sort(MIR.bootstrap),(.1/2)*500),
                              dplyr::nth(sort(MIR.bootstrap),1-(.1/2)*500)),
               linetype="dotted",size=lwd1, col="green4")+
    coord_cartesian(ylim=c(0,200), xlim=c(0.7,1.3))+
    scale_x_continuous(breaks=c(seq(0,1.5,by=0.1)))+
    ylab("Frecuencia") + 
    labs(x = expression(~delta[MIR]),
         title=paste("MIR IC: bootstrap",x)) +
    theme(panel.grid = element_blank(),
          legend.text = element_text(colour="black"),
          panel.background=element_rect(colour = NA,fill = NA),
          panel.border = element_rect(colour = "black",fill = NA),
          axis.ticks = element_line(colour = "black"),
          axis.text = element_text(colour="black"))
  
  
  #############################################################################
  #############################################################################
  # Estimación de Delta MR --------------------------------------------------
  X_R = mean(c(y.1.1, y.2.2))
  X_T = mean(c(y.2.1, y.1.2))
  delta.RM = X_T/X_R
  V_R = var(log(c(y.1.1,y.2.2)))
  V_T = var(log(c(y.2.1, y.1.2)))
  CV_RT = cov(log(c(y.1.1,y.2.2)),log(c(y.2.1, y.1.2)))
  
  error1 = V_T-V_R; error2 = V_R-CV_RT; error3 = CV_RT
  sesgo.MR = delta.MLE*((exp(error1/2)*(1+(exp(error3)*(exp(error2)-1)/(4*(n1+n2)))))-1)
  
  #############################################################################
  #############################################################################
  RM.df <- data.frame(X_R=c(c(y.1.1,y.2.2)),
                      X_T=c(c(y.2.1,y.1.2)))
  # Muestreo de Diferencias por Bootstrap
  bootgh.5 <- boot(RM.df,d.vec.3,R=500)
  bootgh.6 <- boot(RM.df,d.vec.4,R=500)
  # Cálculo de F.hat.1
  RM.bootstrap = (apply(bootgh.6$t, 1, mean)/apply(bootgh.5$t, 1, mean))
  RM.boots = paste("(",round(dplyr::nth(sort(RM.bootstrap),(.1/2)*500),4),", ",
                   round(dplyr::nth(sort(RM.bootstrap),1-(.1/2)*500),4),")",sep="")
  
  p4 = ggplot(as.data.frame(RM.bootstrap),aes(as.data.frame(RM.bootstrap)$RM.bootstrap)) +
    geom_histogram(col="black",fill="pink1",bins=30) +
    geom_vline(xintercept = c(0.80,1.25), linetype="solid",size=lwd1/2, col="black")+
    geom_vline(xintercept = c(dplyr::nth(sort(RM.bootstrap),(.1/2)*500),
                              dplyr::nth(sort(RM.bootstrap),1-(.1/2)*500)),
               linetype="dotted",size=lwd1, col="green4")+
    coord_cartesian(ylim=c(0,200), xlim=c(0.7,1.3))+
    scale_x_continuous(breaks=c(seq(0,1.5,by=0.1)))+
    ylab("Frecuencia") + 
    labs(x = expression(~delta[RM]),
         title=paste("RM IC: bootstrap",x)) +
    theme(panel.grid = element_blank(),
          legend.text = element_text(colour="black"),
          panel.background=element_rect(colour = NA,fill = NA),
          panel.border = element_rect(colour = "black",fill = NA),
          axis.ticks = element_line(colour = "black"),
          axis.text = element_text(colour="black"))
  #############################################################################
  #############################################################################
  # Resultados en Tablas  
  DELTAS = round(c(delta.MLE,delta.MVUE,delta.MIR,delta.RM),dgts1)
  SESGO = c(formatC(sesgo.MLE,format="E",digits=dgts1-1),"-",
            formatC(sesgo.MIR,format="E",digits=dgts1-1),
            formatC(sesgo.MR,format="E",digits=dgts1-1))
  VARIANZA = c(formatC(var.MLE,format="E",digits=dgts1-1),
               formatC(var.MVUE,format="E",digits=dgts1-1),
               formatC(var.MIR,format="E",digits=dgts1-1),"-")
  ICS = c(IC.MLE,MVUE.boots,MIR.boots,RM.boots)
  
  LSA = list(Deltas = DELTAS,
             IC90 = ICS,
             Sesgo = SESGO,
             Varianza = VARIANZA)
  
  resume10 = structure(.Data=LSA,class = "data.frame", 
                       row.names = c("MLE","MVUE","MIR","RM"))
  
  
  secundary =  list("MSE.MLE" = MSE.MLE,
                    "EFF.MVUE" = c(round(eff.MVUE,dgts1)),
                    "MSE.MIR" = c(round(MSE.MIR,dgts1)),
                    "ERR_1" = c(round(V_R-V_T,dgts1)),
                    "ERR_2" = round(V_R-CV_RT,dgts1),
                    "ERR_3" = round(CV_RT,dgts1))
  
  test <- list("Resultados"=resume10, 
               "Graficos"=list(p1,p2,p3,p4),
               "Secundarios"=secundary)
  
  return(test)
}



