

library("ggplot2")
library("grid")
library("png")

vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
setwd(file.path("F:","Documentos","Estudio - Documentos","Farmacocinética 2012-1",
                "Bioequivalencia (2018)","proyecto_seminario","compuestos",
                "data"))

# aciclovir  --------------------------------------------------------------
pka5 = read.csv("./_ACICLOVIR_calculation/pKa.csv",sep=";",dec=".")

cols10 = c("(+2)" <- "green3",
           "(+1)" <- "blue3", 
           "(+0)" <- "red3")

Y21 = ggplot() + 
  geom_line(data=pka5, aes(x=pka5$pH, y = pka5[["Microspecies..1"]], col="(+0)"),size=1.2)+
  geom_line(data=pka5, aes(x=pka5$pH, y = pka5[["Microspecies..2"]], col="(+1)"),size=1.2)+
  geom_line(data=pka5, aes(x=pka5$pH, y = pka5[["Microspecies..3"]], col="(+1)"),size=1.2)+
  geom_line(data=pka5, aes(x=pka5$pH, y = pka5[["Microspecies..6"]], col="(+2)"),size=1.2)+
  scale_x_continuous(breaks = c(seq(0,14,by=1)))+
  scale_y_continuous(breaks = c(seq(0,100,by=10)))+
  coord_cartesian(xlim=c(1,8))+
  labs(x="pH", y ="Fracción Molar")+
  theme(panel.grid.major.x = element_line(colour="gray80"),
        legend.text = element_text(colour = "black"),
        panel.background=element_rect(colour = "black",fill = "white"),
        panel.border = element_rect(colour = "black",fill = NA),
        legend.position = "bottom",
        plot.caption = element_text(hjust=0))+
  scale_color_manual(values=cols10, name="Carga")


DS20 = data.frame(pH = c(1.2, 4.5, 5.8, 6.8, 7.4), 
                  DS1 = c(57, 77, 87, 83, 80), # Dosis 200mg
                  DS2 = c(114, 154, 174, 167, 160), # Dosis 400mg
                  DS3 = c(229, 308, 348, 333, 320)) # Dosis 800mg

cols11 = c("200mg" <- "purple2",
           "400mg" <- "yellow4", 
           "800mg" <- "orange3")

Y22 = ggplot()+
  geom_line(col="red4",size=1,linetype="solid")+  
  geom_point(data=DS20,aes(x=DS20$pH,y=DS20$DS1,col="200mg"))+
  geom_line(data=DS20,aes(x=DS20$pH,y=DS20$DS1,col="200mg"))+
  geom_point(data=DS20,aes(x=DS20$pH,y=DS20$DS2,col="400mg"))+
  geom_line(data=DS20,aes(x=DS20$pH,y=DS20$DS2,col="400mg"))+
  geom_point(data=DS20,aes(x=DS20$pH,y=DS20$DS3,col="800mg"))+
  geom_line(data=DS20,aes(x=DS20$pH,y=DS20$DS3,col="800mg"))+
  geom_hline(yintercept=250, linetype="dashed")+
  scale_x_continuous(breaks = c(seq(0,14,by=1)))+
  scale_y_continuous(trans="log10", 
                     breaks=c(30, 40, 50, 100,200,400,600,800,1000))+
  coord_cartesian(xlim=c(1,8),ylim=c(30,1000))+
  labs(title= "Aciclovir", subtitle = "Comportamiento Ácido-Base - Ratio D/S",
       y = "ratio D/S")+
  theme(panel.grid.major.x = element_line(colour="gray80"),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        legend.text = element_text(colour = "black"),
        panel.background=element_rect(colour = "black",fill = "white"),
        panel.border = element_rect(colour = "black",fill = NA),
        legend.position = "bottom")+
  scale_color_manual(values=cols11, name="Dosis Definida")

grid.newpage()
pushViewport(viewport(layout = grid.layout(3, 1)))
print(Y21, vp = vplayout(2:3, 1))
print(Y22, vp = vplayout(1, 1))
# DEVICE SIZE PRINT: 5 X 6 vertical
