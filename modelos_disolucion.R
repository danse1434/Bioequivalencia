library(deSolve)
library(ggplot2)

Do = 2
Dn = 1.0
An = 5
cf = 1; tf = 1 # Como son variables relativas a la capacidad máxima, su valor máximo es 1
sz1 = 1.2

k_d = 0.032
k_p = 0.012
v = 0.300
s = 1.020
ASF = 1.02
Peff = 0.12

# 1. Ecuación con Coordinada como Derivada ---------------------------------------
mod1 <- function(coord, y , parms){
  with(as.list(c(y, parms)),{
    dD = (k_d*((s/v)-D))-(k_p*D)
    dA = Peff*ASF*A
    return(list(c(dD, dA)))
  })
}
y <- c(D = 0.423, A = 0.2)
parms <- c(k_d = k_d, s = s, v=v,k_p=k_p, Peff=Peff, ASF = ASF)
coord <- seq(0,50, 0.1)
out <- ode(y, coord, mod1, parms)

results = data.frame(coord = as.numeric(out[,"time"]),
                     D = as.numeric(out[,"D"]),
                     A = as.numeric(out[,"A"]))


ggplot() +
  geom_line(data=results, aes(x=results$coord,y = results$D),size=sz1,col="green3")+
  geom_line(data=results, aes(x=results$coord,y = results$A),size=sz1,col="red3")+
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
