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
bodyfat <- read.csv(f)

View(bodyfat)
# Dimensioni
dim(bodyfat)
# Overview delle prime righe
head(bodyfat)

#Look at the main statistics for each covariate:
summary(bodyfat)

x11()
ggpairs(bodyfat)

g = lm( rating ~ .-type-mfr-name, data = bodyfat )

summary( g )   #sembra fico

plot(g,which=1)#NO OMOSCHEDASTICIT?, noto nadamento parabolico?
shapiro.test(g$residuals) #OK
plot(dataADJ$GDP,dataADJ$Happi)
abline(a=reg$coefficients[1],b=reg$coefficients[2])#Non è una bella interpolazione