# MODELO ACAT
# 

library("ggplot2"); theme_set(theme_classic())

setwd(file.path("F:","Documentos","Estudio - Documentos","Farmacocinética 2012-1","Bioequivalencia (2018)",
                "·Clases","modelo ACAT"))
acat <- read.csv(file = "segundoPLT.csv",sep=",",header = T)[,-c(1,13:(13+11))]

cols1 <- c("Estomago" = "red3",
          "Duodeno 1" = "green4",
          "Duodeno 2" = "green2",
          "Duodeno 3" = "green2",
          "Yeyuno 1" = "yellow4",
          "Yeyuno 2" = "yellow3",
          "Yeyuno 3" = "yellow2",
          "Ileon" = "blue3",
          "Colon" = "blue2",
          "Absorbido" = "gold2")

g1_plot = ggplot(acat)+
  geom_line(aes(x=acat[,1],y=acat[,2],col="Estomago"))+
  geom_line(aes(x=acat[,1],y=acat[,3],col="Duodeno 1"))+
  geom_line(aes(x=acat[,1],y=acat[,4],col="Duodeno 2"))+
  geom_line(aes(x=acat[,1],y=acat[,5],col="Duodeno 3"))+
  geom_line(aes(x=acat[,1],y=acat[,6],col="Yeyuno 1"))+
  geom_line(aes(x=acat[,1],y=acat[,7],col="Yeyuno 2"))+
  geom_line(aes(x=acat[,1],y=acat[,8],col="Yeyuno 2"))+
  geom_line(aes(x=acat[,1],y=acat[,9],col="Ileon"))+
  scale_color_manual(values=cols1, name="Compartimento")+
  xlab("Tiempo (horas)")+
  ylab("Cantidad en Compartimento (mg)")+
  coord_cartesian(x=c(0,10))+
  theme(panel.border = element_rect(colour="black",fill=NA),
        panel.grid = element_line(colour="gray60"),
        plot.background = element_rect(fill="white"))

ggsave(filename = "modelo CAT simple.pdf",plot = g1_plot,device = "pdf",path = "./images",
       width = 6, height = 4, units="in")

g2_plot = g1_plot +
  geom_line(aes(x=acat[,1],y=acat[,10],col="Absorbido"),linetype="dashed",size=1.1)

ggsave(filename = "modelo CAT absorbida.pdf",plot = g2_plot,device = "pdf",path = "./images",
       width = 6, height = 4, units="in")
