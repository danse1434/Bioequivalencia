### EJEMPLOS DE ANÁLISIS DE VARIANZA EN BIOEQUIVALENCIA
library(ggplot2)
library(lme4)
library(readr)
library(psycho)
library(lmerTest)
setwd(file.path("F:","Documentos","Estudio - Documentos",
                "Farmacocinética 2012-1","Bioequivalencia (2018)","·Clases"))


aov_example <- read.csv("aov_example.csv")

aov_example[,1] <- as.factor(aov_example[,1])
aov_example[,2] <- as.factor(aov_example[,2])
aov_example[,3] <- as.factor(aov_example[,3])
aov_example[,4] <- as.factor(aov_example[,4])

# ANOVA DE TRES FACTORES 
a1 <- aov(AUC ~ Formulation+Period+Sequence,data=aov_example)
# ANOVA DE MEDIDAS REPETIDAS (DOS FACTORES INTRAINDIVIDUALES, UN FACTOR INTERINDIVIDUAL) 
a2 <- aov(AUC ~ Formulation+Period+Sequence+Error(ID/(Formulation+Period)),data=aov_example)
# RESUMEN DE RESULTADOS ANOVA
summary(a1); summary(a2)
# MODELO MIXTO COMPARABLE A ANOVA DE MEDIDAS REPETIDAS
b2 <- lmer(AUC ~ Period + Formulation + (1|ID) + offset(as.numeric(Sequence)), data=aov_example)
print(b2)
anova(b2)
# CONTRASTES LINEALES Y RESUMEN DE MODELOS MIXTOS
res2 <- analyze(b2); print(res2)
res3 <- get_contrasts(b2, "Formulation + Period")
# GRÁFICOS COMPARATIVOS DE EFECTOS
ggplot(res3$means,aes(x = Period,y=Mean,group=1)) +
  geom_line() +
  geom_pointrange(aes(ymin=CI_lower, ymax=CI_higher)) +
  ylab("Mean AUC") +
  xlab("Period") +
  theme_bw()

ggplot(res3$means,aes(x = Formulation,y=Mean,group=Formulation)) +
  geom_line() +
  geom_pointrange(aes(ymin=CI_lower, ymax=CI_higher)) +
  ylab("Mean AUC") +
  xlab("Sequence") +
  theme_bw()


