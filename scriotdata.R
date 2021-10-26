dat1 <- read.table('clipboard',dec = ',',header = TRUE)
names(dat1)
attach(dat1)
str(dat1)
library(tidyverse)
library(learnr) 
library(dplyr)
library(readxl)
library(readr)
library(plotrix)
library(epiDisplay)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(agricolae)
library(daewr)
library(Hmisc)

tapply(germrate, trs, mean)## checkking the differents means of the treatment

tapply(germrate, trs, sd)## see the ecart between within group
#This is a recap
# construct graphical summaries to assess the trends and differences in groups within the data
#boxplot
library(dplyr)
dat1 %>%
  group_by(trs)%>%
  summarise(germrate=mean(germrate))
boxplot(germrate~trs,data=dat1, xlab=" treatment_type", ylab="germination_rate")
##using ggplot
##library(ggplot2)
##ggplot(data=dat1,aes(x=trs,y=germrate))+geom_boxplot()
## one-way anaalyse of variance using lm test
#shapiro.test(dat1$germrate)
Model1 <- lm(germrate ~ trs, data = dat1)
#summary(Model1)
anova(Model1)
#alternative 2 using aov function 
#Model2 <- aov(germrate ~ trs,data = dat1 )
#summary(Model2)
# since the pvalue is smaller than 0.05, hence we run 
#the pairwise multiple comparision tests
# multiple comparison tests (Post Hoc tests
##ls1=TukeyHSD(Model2,"trs")
##
ls = LSD.test(Model1,"trs")
ls
bar.group(ls$groups,ylim=c(0,100))

#pairwise.t.test(germrate, trs, p.adj = "none")
#pairwise.t.test(germrate, trs, p.adj = "bonf")
#pairwise.t.test(germrate, trs, p.adj = "holm")
pairwise.t.test(germrate, trs, p.adjust.method =p.adjust.methods)

#--------------------------------------------------------
#Tukey's HSD (HONESTLY SIGNIFICANT DIFFERENCE)test
Model2 <- aov(germrate ~ trs,data = dat1 )
Model2.tukey<-TukeyHSD(Model2,ordered=T)
Model2.tukey
#
# Residual anlysis
germ.resid <- residuals(Model1) # Pour extraire et afficher les residus du modele

germ.fited <- fitted(Model1) # Pour extraire et afficher les valeurs ajustees (estimees) de "dep.soins" par le modele

par(mfrow = c(1, 3))  # creation d'un cadre pour afficher trois graphes
hist(germ.resid, main = "Histogramme")  # Histogramme des residus (1er graphe)
qqnorm(germ.resid)  # quantiles normaux (2e graphe)
qqline(germ.resid, lty = 2, col = "blue")  # droite des quantiles normaux
plot(germ.fited, germ.resid, xlab = "Fitted values", 
     ylab = "Residuals")  # residus en fonction des valeurs estimees (3e graphe)
abline(h = 0, lty = 2)  # droite des quantiles normaux
text(germ.fited, germ.resid, pos = 4) # Ajout des numeros des observations sur le 3e graphique
# HO: Normalite des residus, H1: Non normalite des residus
shapiro.test(germ.fited) 
# 4.4.3. Test de l'homogeneite des residus: Test de breusch-Pagan
# # HO: Homogeneite des residus, H1: Non homogeneite des residus
library(lmtest)  # Chargement du package 'lmtest'
bptest(Model1)  # Realisation du test de breusch-Pagan
# 4.4.4. Test d'autocorrelation des residus: test de Durbin-Waston
dwtest(Model1)  # Realisation du test de Durbin-Watson
# 4.4.5. Test de la nullite de la moyenne des residus - Test t de conformite d'une moyenne
# HO: la moyenne des residus est nulle, H1: la moyenne des residus est nulle
t.test(germ.resid, mu = 0) 
# 5. Linearite de la relation (Ramsey reset test)
resettest(Model1, power = 1:2, type = "regressor")  # (Test reset de non linearite)
#-------------------------------------------------------------
# Check for homoscedasticity (constant variance)
plot(Model1)
par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
# Plot the model information


#--------------------------------------------------------------------
#Tukey test, only performed if we find significant differences between the treatments
library(multcompView)
TUKEY <- TukeyHSD(x=Model2, 'trs',conf.level=0.95)
TUKEY
#Preplaned Comparisons tested using contrasts
-----------------------------
#Student Newman-Keuls method can be made using the Snk.test function 
#in the R package agricolae (de Mendiburu, 2012a). 
library(agricolae)
compare <- SNK.test(Model2, "trs", alpha = 0.05 )
print(compare)




