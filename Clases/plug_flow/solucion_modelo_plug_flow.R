library(deSolve)
library(ggplot2)

Do = 2
Dn = 1.0
An = 5
cf = 1; tf = 1 # Como son variables relativas a la capacidad máxima, su valor máximo es 1
sz1 = 1.2

# 1. Ecuación con Coordinada como Derivada ---------------------------------------
mod1 <- function(coord, y , parms){
  with(as.list(c(y, parms)),{
    dr = - (Dn/3)*((1-C)/r)
    dC = (Dn*Do*r*(1-C))-(2*An*C)
    return(list(c(dr, dC)))
  })
}
y <- c(r = 1, C = 0)
parms <- c(Dn = Dn, Do = Do,An = An)
coord <- seq(0,cf, 0.01)
out <- ode(y, coord, mod1, parms)

results = data.frame(coord = as.numeric(out[,"time"]),
                     r = as.numeric(out[,"r"]),
                     C = as.numeric(out[,"C"]))

results$F = as.numeric(1-((results[,"r"])^3)-(results[,"C"]/Do))
cols = c("F" = "green4","C" = "blue4","r" = "red4")

G1 = ggplot() +
  geom_line(data=results, aes(x=results$coord,y = results$r,col="r"),size=sz1)+
  geom_line(data=results, aes(x=results$coord,y = results$C,col="C"),size=sz1)+
  geom_line(data=results, aes(x=results$coord,y = results$F,col="F"),size=sz1)+
  labs(x="Coordenada (z)",y="Parámetro",
       subtitle=paste("Grupos Adimensionales vs Coordenada Do =",Do,
  "Dn =",Dn,"An =",An))+
  scale_y_continuous(breaks=seq(0,1,by=0.1))+
  scale_x_continuous(breaks=seq(0,2,by=0.2))+
  coord_cartesian(xlim=c(0,cf),ylim=c(0,1))+
  theme(panel.grid = element_blank(),
        legend.text = element_text(colour = "black"),
        panel.background=element_rect(colour = "black",fill = "white"),
        panel.border = element_rect(colour = "black",fill = NA),
        strip.background = element_rect(colour="black",fill="green1"),
        strip.text.x = element_text(face="bold"))+
  scale_colour_manual(name="Parámetro",values=cols); G1


# 2. Ecuación con Tiempo como Derivada ---------------------------------------

mod2 <- function(time, y , parms){
  with(as.list(c(y, parms)),{
    dr = - (Dn/2)*((1-C)/r)
    dC = ((3/2)*Dn*Do*r*(1-C))-(2*An*C)
    return(list(c(dr, dC)))
  })
}

y <- c(r = 1, C = 0)
parms <- c(Dn = Dn, Do = Do,An = An)
time <- seq(0,tf, 0.01)
out1 <- ode(y, time, mod1, parms)

results1 = data.frame(time = as.numeric(out1[,"time"]),
                     r = as.numeric(out1[,"r"]),
                     C = as.numeric(out1[,"C"]))

results1$F = as.numeric(1-((results1[,"r"])^3)-(results1[,"C"]/Do))
cols = c("F" = "green4","C" = "blue4","r" = "red4")

G2 = ggplot() +
  geom_line(data=results1, aes(x=results1$time,y = results1$r,col="r"),size=sz1)+
  geom_line(data=results1, aes(x=results1$time,y = results1$C,col="C"),size=sz1)+
  geom_line(data=results1, aes(x=results1$time,y = results1$F,col="F"),size=sz1)+
  labs(x="Tiempo Relativo (t)",y="Parámetro",
       subtitle=paste("Grupos Adimensionales vs Tiempo Do =",Do,
                      "Dn =",Dn,"An =",An))+
  scale_y_continuous(breaks=seq(0,1,by=0.1))+
  scale_x_continuous(breaks=seq(0,2,by=0.2))+
  coord_cartesian(xlim=c(0,tf),ylim=c(0,1))+
  theme(panel.grid = element_blank(),
        title = element_text(color = "red4"),
        legend.text = element_text(colour = "black"),
        panel.background=element_rect(colour = "black",fill = "white"),
        panel.border = element_rect(colour = "black",fill = NA),
        strip.background = element_rect(colour="black",fill="green1"),
        strip.text.x = element_text(face="bold"))+
  scale_colour_manual(name="Parámetro",values=cols); G2

