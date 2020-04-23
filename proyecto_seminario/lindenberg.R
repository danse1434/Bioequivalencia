library("ggplot2")
library("ggrepel")


setwd(file.path("F:","Documentos","Estudio - Documentos","Farmacocinética 2012-1",
                "Bioequivalencia (2018)","proyecto_seminario"))

libderberg_1 = read.csv("lindenberg_1.csv",header = T, sep = ",",dec = ".")

set.seed(2018)


text1 = "Clasificación Propuesta: Lindenberg M, y colaboradores 2004"
text2 = "Fármacos con datos de permeabilidad y solubilidad confiables"

annot = "Adaptado de: Lindenberg M, Kopp S, Dressman JB. Classification of orally administered drugs on the World Health Organization Model list 
of Essential Medicines according to the biopharmaceutics classification system. Eur J Pharm Biopharm. 2004;58(2):265-78."

J1 = ggplot(data=libderberg_1, aes(x,y,group=SCB,  fill=factor(SCB),color = "black"))+
  facet_wrap(.~SCB)+
  geom_label_repel(aes(label = Droga), 
                   size = 3.5,
                   segment.colour = NA,
                   color="white") +
  labs(title= text1, subtitle = text2, caption=annot)+
  theme(panel.grid = element_blank(),
        legend.text = element_text(colour = "black"),
        panel.background=element_rect(colour = "black",fill = "#FFFFE0"),
        panel.border = element_rect(colour = "black",fill = NA),
        strip.background = element_rect(colour="black",fill="gray80"),
        strip.text.x = element_text(face="bold",size=12),
        axis.title = element_blank(),axis.text = element_blank(),
        legend.position = "none",
        plot.caption = element_text(hjust=0))+
  scale_fill_manual(values = c("blue4","red2","green4","pink2"))+
  scale_color_manual(values = c("blue4","red2","green4","pink2"))
       
ggsave(filename = "LEGEND1.pdf",plot = J1,device = "pdf",width = 9,height = 7)
       

libderberg_2 = read.csv("lindenberg_2.csv",header = T, sep = ",",dec = ".")

text3 = "Fármacos con datos de permeabilidad y solubilidad incompletos"

J2 = ggplot(data=libderberg_2, aes(x,y,group=SCB,  fill=factor(SCB),color = "black"))+
  facet_wrap(.~SCB)+
  geom_label_repel(aes(label = Droga), 
                   size = 3.5,
                   segment.colour = NA,
                   color="white") +
  labs(title= text1, subtitle = text3, caption=annot)+
  theme(panel.grid = element_blank(),
        legend.text = element_text(colour = "black"),
        panel.background=element_rect(colour = "black",fill = "#FFFFE0"),
        panel.border = element_rect(colour = "black",fill = NA),
        strip.background = element_rect(colour="black",fill="gray80"),
        strip.text.x = element_text(face="bold",size=12),
        axis.title = element_blank(),axis.text = element_blank(),
        legend.position = "none",
        plot.caption = element_text(hjust=0))+
  scale_fill_manual(values = c("blue4","red2","green4","pink2"))+
  scale_color_manual(values = c("blue4","red2","green4","pink2"))

ggsave(filename = "LEGEND2.pdf",plot = J2,device = "pdf",width = 9,height = 7)

# Diagrama de Venn
libderberg_3 = read.csv("lindenberg_3.csv",header = T, sep = ",",dec = ".")
neworder <- c("I/II","I/III","II/IV","III/IV")

library(plyr) 
libderberg_3_1 <- arrange(transform(libderberg_3,
                           SCB=factor(SCB,levels=neworder)),SCB)

text4 = "Fármacos con datos de permeabilidad y solubilidad inconclusos"

J3 = ggplot(data=libderberg_3, aes(x,y,group=SCB,  fill=factor(SCB),color = "black"))+
  facet_wrap(.~SCB)+
  geom_label_repel(aes(label = Droga), 
                   size = 3.5,
                   segment.colour = NA,
                   color="white",force=2) +
  labs(title= text1, subtitle = text4, caption=annot)+
  theme(panel.grid = element_blank(),
        legend.text = element_text(colour = "black"),
        panel.background=element_rect(colour = "black",fill = "#DBFFFF"),
        panel.border = element_rect(colour = "black",fill = NA),
        strip.background = element_rect(colour="black",fill="gray20"),
        strip.text.x = element_text(face="bold",size=12,colour="white"),
        axis.title = element_blank(),axis.text = element_blank(),
        legend.position = "none",
        plot.caption = element_text(hjust=0))

ggsave(filename = "LEGEND3.pdf",plot = J3,device = "pdf",width = 9,height = 7)

