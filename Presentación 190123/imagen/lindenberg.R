library("ggplot2")
library("ggrepel")


setwd(file.path("F:","Documentos","Estudio - Documentos","Farmacocinética 2012-1",
                "Bioequivalencia (2018)","Presentación 190123","imagen"))

libderberg_1 = read.csv("data.csv",header = T, sep = ",",dec = ".")

set.seed(2018)

text1 = "Clasificación Propuesta: Cabrera-Perez, y colaboradores 2018"
text2 = "Clasificación Permeabilidad Humana"

annot = "Cabrera MÁ, Pham-The H, Fernández Cervera M, Hernández-Armengol R, Miranda-Pérez de Alejo C, Brito-Ferrer Y. Integrating theoretical and 
experimental permeability estimations for provisional biopharmaceutical classification: Application to the WHO essential medicines. 
Biopharm Drug Dispos. 2018;39(June):354-68." 


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
       
ggsave(filename = "Clasificacion_1.pdf",plot = J1,device = "pdf",width = 9,height = 7)

text3 = "Clasificación por sistema de votación mayoritario"

J2 = ggplot(data=libderberg_1, aes(x,y,group=SCB_1,  fill=factor(SCB_1),color = "black"))+
  facet_wrap(.~SCB_1)+
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

ggsave(filename = "Clasificacion_2.pdf",plot = J2,device = "pdf",width = 9,height = 7)