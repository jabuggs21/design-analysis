####HW 2

install.packages("dplyr")
library("dplyr")
install.packages("car")
library("car")
install.packages("tidyverse")
library("tidyverse")
install.packages("rstatix")
library("rstatix")




###Question 1
##Assumption Check
#Normality: Raw Data Residuals - Shapiro-Wilk Test
ctrl <- read.table(file = "clipboard", sep = "\t", header = TRUE)
blind <- read.table(file = "clipboard", sep = "\t", header = FALSE)
mag <- read.table(file = "clipboard", sep = "\t", header = FALSE)
both <- read.table(file = "clipboard", sep = "\t", header = FALSE)
shapiro.test(ctrl$Residual..data...mean.)
shapiro.test(blind$V1)
shapiro.test(mag$V1)
shapiro.test(both$V1)

#Homogeneity of Variances: Raw/Power Data Residuals - Levene Test
rawres <- read.table(file = "clipboard", sep = "\t", header = TRUE)
powres <- read.table(file = "clipboard", sep = "\t", header = TRUE)
leveneTest(ResidualR ~ Group, data = rawres)
leveneTest(ResidualP ~ Group, data = powres)

##1 factor ANOVA (Model 1)
q1 <- read.table(file = "clipboard", sep = "\t", header = TRUE)
q1.aov <- aov(Time ~ Treatment, data = q1)
summary(q1.aov)

#follow-up: Tukey Test
TukeyHSD(q1.aov)


###Question 2
##2 factor crossed ANOVA (Model 1)
q2 <- read.table(file = "clipboard", sep = "\t", header = TRUE)
q2$Exposure <- factor(q2$Exposure)
q2$Species <- factor(q2$Species)
q2.aov <- aov(biomass ~ Exposure*Species, data = q2)
summary(q2.aov)

#follow up: Tukey test
TukeyHSD(q2.aov, "Exposure")
TukeyHSD(q2.aov, "Species")
TukeyHSD(q2.aov, "Exposure:Species")


###Question 3
##2 factor crossed ANOVA (Mixed Model)
##NOTE: Mixed models require recalculation of F statistics as needed
q3 <- read.table(file = "clipboard", sep = "\t", header = TRUE)
q3$Weeks <- factor(q3$Weeks)
q3$Ashfall <- factor(q3$Ashfall)
q3.aov <- aov(Diversity ~ Weeks*Ashfall, data = q3)
summary(q3.aov)

#follow up: Tukey test
TukeyHSD(q3.aov, "Weeks")
TukeyHSD(q3.aov, "Ashfall")
TukeyHSD(q3.aov, "Weeks:Ashfall")


###Question 4
##1 factor ANOVA (Model 1)
q4 <- read.table(file = "clipboard", sep = "\t", header = TRUE)
q4.aov <- aov(Otter ~ Killer, data = q4)
summary(q4.aov)

#follow-up: Tukey Test
TukeyHSD(q4.aov)
