

library("ggplot2")
library("grid")
library("png")

vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
setwd(file.path("F:","Documentos","Estudio - Documentos","Farmacocinética 2012-1",
                "Bioequivalencia (2018)","proyecto_seminario","compuestos",
                "data"))


pka1 = read.csv("./_folic_calculation/pKa.csv",sep=";",dec=".")
cols1 = c("(+1)" <- "green3",
          "(+0)" <- "purple2",
          "(-1)" <- "blue3",
          "(-2)" <- "red3")


G1 <- ggplot() + 
  geom_line(data=pka1, aes(x=pka1$pH, y = pka1$X1, col="(+1)"),size=1.2)+
  geom_line(data=pka1, aes(x=pka1$pH, y = pka1$X0, col="(+0)"),size=1.2)+
  geom_line(data=pka1, aes(x=pka1$pH, y = pka1$X.1, col="(-1)"),size=1.2)+
  geom_line(data=pka1, aes(x=pka1$pH, y = pka1$X.2, col="(-2)"),size=1.2)+
  scale_x_continuous(breaks = c(seq(0,14,by=1)))+
  coord_cartesian(xlim=c(1,7))+
  labs(x="pH", y ="Fracción Molar")+
  theme(panel.grid.major.x = element_line(colour="gray50"),
        legend.text = element_text(colour = "black"),
        panel.background=element_rect(colour = "black",fill = "white"),
        panel.border = element_rect(colour = "black",fill = NA),
        legend.position = "bottom",
        plot.caption = element_text(hjust=0))+
  scale_color_manual(values=cols1, name="Carga")


DS1 = data.frame(pH = c(1.2, 3.0, 4.5, 6.8), 
           DS = c(314, 3.24E3, 78.6, 0.773),
           ERR = c(5, 0.09E3, 0.8,0))

G2 = ggplot(data=DS1, aes(x=pH,y=DS))+
  geom_point(shape=22, col="red4",size=2)+
  geom_pointrange(aes(ymin=DS-ERR,ymax=DS+ERR))+
  geom_line(col="red4",size=1,linetype="solid")+  
  geom_hline(yintercept=250)+
  scale_x_continuous(breaks = c(seq(0,14,by=1)))+
  scale_y_continuous(trans="log10", 
                     breaks=c(1,10,100,1000),
                     minor_breaks=c(2,4,6,8,
                                    20,40,60,80,
                                    200,400,600,800,
                                    2000,4000,6000,8000))+
  coord_cartesian(xlim=c(1,7))+
  labs(title= "Ácido Fólico", subtitle = "Comportamiento Ácido-Base - Ratio D/S",
       y = "ratio D/S")+
  theme(panel.grid.major.x = element_line(colour="gray50"),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        legend.text = element_text(colour = "black"),
        panel.background=element_rect(colour = "black",fill = "white"),
        panel.border = element_rect(colour = "black",fill = NA))

grid.newpage()
pushViewport(viewport(layout = grid.layout(3, 1)))
print(G1, vp = vplayout(2:3, 1))
print(G2, vp = vplayout(1, 1))
# DEVICE SIZE PRINT: 4 X6 


# Mifepristona ------------------------------------------------------------
pka2 = read.csv("./_mifepristone_calculation/pKa.csv",sep=";",dec=".")

cols2 = c("(+1)" <- "green3",
          "(+0)" <- "purple2")

Y1 = ggplot() + 
  geom_line(data=pka2, aes(x=pka2$pH, y = pka2$Microspecies..1, col="(+0)"),size=1.2)+
  geom_line(data=pka2, aes(x=pka2$pH, y = pka2$Microspecies..2, col="(+1)"),size=1.2)+
  scale_x_continuous(breaks = c(seq(0,14,by=1)))+
  coord_cartesian(xlim=c(1,7))+
  labs(x="pH", y ="Fracción Molar")+
  theme(panel.grid.major.x = element_line(colour="gray50"),
        legend.text = element_text(colour = "black"),
        panel.background=element_rect(colour = "black",fill = "white"),
        panel.border = element_rect(colour = "black",fill = NA),
        legend.position = "bottom",
        plot.caption = element_text(hjust=0))+
  scale_color_manual(values=cols2, name="Carga")


DS2 = data.frame(pH = c(1.2, 4.5, 6.8), 
                 DS = c(43.8, 2.90E3, 0.2E6))

Y2 = ggplot(data=DS2, aes(x=pH,y=DS))+
  geom_point(shape=22, col="red4",size=2)+
  geom_line(col="red4",size=1,linetype="solid")+  
  geom_hline(yintercept=250)+
  scale_x_continuous(breaks = c(seq(0,14,by=1)))+
  scale_y_continuous(trans="log10", 
                     breaks=c(1,10,100,1000,1E4,1e5,1e6,1e7),
                     minor_breaks=c(2,4,6,8,
                                    20,40,60,80,
                                    200,400,600,800,
                                    2000,4000,6000,8000))+
  coord_cartesian(xlim=c(1,7))+
  labs(title= "Mifepristona", subtitle = "Comportamiento Ácido-Base - Ratio D/S",
       y = "ratio D/S")+
  theme(panel.grid.major.x = element_line(colour="gray50"),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        legend.text = element_text(colour = "black"),
        panel.background=element_rect(colour = "black",fill = "white"),
        panel.border = element_rect(colour = "black",fill = NA))

grid.newpage()
pushViewport(viewport(layout = grid.layout(3, 1)))
print(Y1, vp = vplayout(2:3, 1))
print(Y2, vp = vplayout(1, 1))
# DEVICE SIZE PRINT: 5 X 6 

# Rifabutina --------------------------------------------------------------
pka3 = read.csv("./_rifabutin_calculation/pKa.csv",sep=";",dec=".")

cols3 = c("(+2)" <- "green3",
          "(+1)" <- "red3",
          "(+0)" <- "blue3")


Y3 <- ggplot() + 
  geom_line(data=pka3, aes(x=pka3$pH, y = pka3[["Microspecies..4"]],col="(+2)"),size=1.2)+
  geom_line(data=pka3, aes(x=pka3$pH, y = pka3[["Microspecies..3"]],col="(+1)"),size=1.2)+
  geom_line(data=pka3, aes(x=pka3$pH, y = pka3[["Microspecies..5"]],col="(+0)"),size=1.2)+
  scale_x_continuous(breaks = c(seq(0,14,by=1)))+
  coord_cartesian(xlim=c(1,7))+
  labs(x="pH", y ="Fracción Molar")+
  theme(panel.grid.major.x = element_line(colour="gray50"),
        legend.text = element_text(colour = "black"),
        panel.background=element_rect(colour = "black",fill = "white"),
        panel.border = element_rect(colour = "black",fill = NA),
        legend.position = "bottom",
        plot.caption = element_text(hjust=0))+
  scale_color_manual(values=cols3, name="Carga")


DS3 = data.frame(pH = c(1.2, 4.5, 6.8), 
                 DS = c(0.31E3, 47.9, 2.82E3))

Y4 <- ggplot(data=DS3, aes(x=pH,y=DS))+
  geom_point(shape=22, col="red4",size=2)+
  geom_line(col="red4",size=1,linetype="solid")+  
  geom_hline(yintercept=250)+
  scale_x_continuous(breaks = c(seq(0,14,by=1)))+
  scale_y_continuous(trans="log10", 
                     breaks=c(1,10,100,1000,1E4,1e5,1e6,1e7),
                     minor_breaks=c(2,4,6,8,
                                    20,40,60,80,
                                    200,400,600,800,
                                    2000,4000,6000,8000))+
  coord_cartesian(xlim=c(1,7))+
  labs(title= "Rifabutina", subtitle = "Comportamiento Ácido-Base - Ratio D/S",
       y = "ratio D/S")+
  theme(panel.grid.major.x = element_line(colour="gray50"),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        legend.text = element_text(colour = "black"),
        panel.background=element_rect(colour = "black",fill = "white"),
        panel.border = element_rect(colour = "black",fill = NA))

grid.newpage()
pushViewport(viewport(layout = grid.layout(3, 1)))
print(Y3, vp = vplayout(2:3, 1))
print(Y4, vp = vplayout(1, 1))
# DEVICE SIZE PRINT: 5 X 6 

isoel3 = read.csv("./_rifabutin_calculation/isoelectric_point.csv",sep=";",dec=".")

Y3.3 <- ggplot(isoel3,aes(pH,Charge))+
  geom_line(col="blue4",size=1,linetype="solid")+
  scale_x_continuous(breaks = c(seq(0,14,by=1)))+
  coord_cartesian(xlim=c(1,7),ylim=c(-1,2))+
  labs(x="pH", y ="Carga")+
  theme(panel.grid.major.x = element_line(colour="gray50"),
        legend.text = element_text(colour = "black"),
        panel.background=element_rect(colour = "black",fill = "white"),
        panel.border = element_rect(colour = "black",fill = NA),
        legend.position = "bottom",
        plot.caption = element_text(hjust=0))+
  scale_color_manual(values=cols3, name="Carga")


solub3 = read.csv("./_rifabutin_calculation/solubility.csv",sep=";",dec=".")

Y3.4 <- ggplot(solub3,aes(pH,mg.ml))+
  geom_line(col="green4",size=1,linetype="solid")+
  scale_x_continuous(breaks = c(seq(0,14,by=1)))+
  coord_cartesian(xlim=c(1,7))+
  labs(x="pH", y ="Solubilidad Predicha (mg/mL)")+
  theme(panel.grid.major.x = element_line(colour="gray50"),
        legend.text = element_text(colour = "black"),
        panel.background=element_rect(colour = "black",fill = "white"),
        panel.border = element_rect(colour = "black",fill = NA),
        legend.position = "bottom",
        plot.caption = element_text(hjust=0))+
  scale_color_manual(values=cols3, name="Carga")


grid.newpage()
pushViewport(viewport(layout = grid.layout(3, 1)))
print(Y4, vp = vplayout(1, 1))
print(Y3.3, vp = vplayout(2, 1))
print(Y3.4, vp = vplayout(3, 1))
# DEVICE SIZE PRINT: 5 X 6 vertical

# SUCCIMERO  --------------------------------------------------------------
pka4 = read.csv("./_succimer_calculation/pKa.csv",sep=";",dec=".")

cols10 = c("(0)" <- "green3",
          "(-1)" <- "blue3", "(-2)" <- "red3")

Y10 = ggplot() + 
  geom_line(data=pka4, aes(x=pka4$pH, y = pka4[["Microspecies..1"]], col="(+0)"),size=1.2)+
  geom_line(data=pka4, aes(x=pka4$pH, y = pka4[["Microspecies..3"]], col="(-1)"),size=1.2)+
  geom_line(data=pka4, aes(x=pka4$pH, y = pka4[["Microspecies..4"]], col="(-2)"),size=1.2)+
  scale_x_continuous(breaks = c(seq(0,14,by=1)))+
  coord_cartesian(xlim=c(1,7))+
  labs(x="pH", y ="Fracción Molar")+
  theme(panel.grid.major.x = element_line(colour="gray50"),
        legend.text = element_text(colour = "black"),
        panel.background=element_rect(colour = "black",fill = "white"),
        panel.border = element_rect(colour = "black",fill = NA),
        legend.position = "bottom",
        plot.caption = element_text(hjust=0))+
  scale_color_manual(values=cols10, name="Carga")


DS10 = data.frame(pH = c(1.2, 4.5, 6.8), 
                 DS = c(96, 0.35E3, 85))

Y11 = ggplot(data=DS10, aes(x=pH,y=DS))+
  geom_point(shape=22, col="red4",size=2)+
  geom_line(col="red4",size=1,linetype="solid")+  
  geom_hline(yintercept=250)+
  scale_x_continuous(breaks = c(seq(0,14,by=1)))+
  scale_y_continuous(trans="log10", 
                     breaks=c(100,200,400,600,800,1000))+
  coord_cartesian(xlim=c(1,7),ylim=c(50,1000))+
  labs(title= "Succímero", subtitle = "Comportamiento Ácido-Base - Ratio D/S",
       y = "ratio D/S")+
  theme(panel.grid.major.x = element_line(colour="gray50"),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        legend.text = element_text(colour = "black"),
        panel.background=element_rect(colour = "black",fill = "white"),
        panel.border = element_rect(colour = "black",fill = NA))

grid.newpage()
pushViewport(viewport(layout = grid.layout(3, 1)))
print(Y10, vp = vplayout(2:3, 1))
print(Y11, vp = vplayout(1, 1))
# DEVICE SIZE PRINT: 5 X 6 vertical

