####Independent Project

###Zooplankton Abundance in the Bering Sea
###NOAA NCEI Ascension Number 0211098
install.packages("dplyr")
library("dplyr")
install.packages("car")
library("car")
install.packages("tidyverse")
library("tidyverse")
install.packages("rstatix")
library("rstatix")
install.packages("pwr")
library("pwr")
install.packages("pwr2")
library("pwr2")
install.packages("ggpubr")
library("ggpubr")


##Apriori Analysis
pwr.2way(a=2, b=2, alpha=0.05, size.A=12, size.B=45, f.A=0.3, f.B=0.3)

##Normality: Shapiro-Wilk Raw Data Residuals
rawres <- read.table(file = "clipboard", sep = "\t", header = TRUE)
powerres <- read.table(file = "clipboard", sep = "\t", header = TRUE)
logres <- read.table(file = "clipboard", sep = "\t", header = TRUE)
shapiro.test(powerres$ResidualP)
shapiro.test(rawres$ResidualR)
shapiro.test(logres$ResidualL)
#log10 transformation is normal p > 0.05


##Homogeneity of Variances: Raw/Power Data Residuals - Levene Test
leveneTest(ResidualL ~ Group, data = logres)
#log10 has equal variances p > 0.05

#Insert Transformed Dataset
planktonlog <- read.table(file = "clipboard", sep = "\t", header = TRUE)

#Normality plot
with(planktonlog, hist(resid(aov(Density ~ Month))))
with(planktonlog, hist(resid(aov(Density ~ Size))))

#QQ plot
with(planktonlog, qqnorm(resid(aov(Density ~ Month))))
with(planktonlog, qqnorm(resid(aov(Density ~ Size))))

#2 way crossed ANOVA (Model 1)
planktonlog$Month <- factor(planktonlog$Month)
planktonlog$Size <- factor(planktonlog$Size)
planlog.aov <- aov(Density ~ Month*Size, data = planktonlog)
summary(planlog.aov)

#follow up: Tukey test
TukeyHSD(planlog.aov, "Month")
TukeyHSD(planlog.aov, "Size")
TukeyHSD(planlog.aov, "Month:Size")

#Plotting ANOVA results
ggboxplot(planktonlog, x = "Size", y = "Density", color = "Month",
          palette = c("#00AFBB", "#E7B800", "#FF0000"))
     