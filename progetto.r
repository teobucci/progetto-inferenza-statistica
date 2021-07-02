library( car )
library( faraway )
library( leaps )
library(MASS)
library( GGally)
library(rgl)
library(dplyr)
library(data.table)
library(ggplot2)
library(corrplot)
library(RColorBrewer)


f <- file.choose()
cereal <- read.csv(f)

View(cereal)
# Dimensioni
dim(cereal)
# Overview delle prime righe
head(cereal)

#Look at the main statistics for each covariate:
summary(cereal)

x11()
ggpairs(cereal)

g = lm( rating ~ .-type-mfr-name, data = cereal )

summary( g )   #sembra fico

plot(g,which=1)#NO OMOSCHEDASTICITÃ, noto nadamento parabolico 
shapiro.test(g$residuals) #OK
plot(dataADJ$GDP,dataADJ$Happi)
abline(a=reg$coefficients[1],b=reg$coefficients[2])#Non Ã¨ una bella interpolazione