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
wine <- read.csv(f)

View(wine)
# Dimensioni
dim(wine)
# Overview delle prime righe
head(wine)

#Look at the main statistics for each covariate:
summary(wine)

ggpairs(wine)

g = lm( pH ~ ., data = wine )

summary( g )   #sembra fico

plot(g,which=1)#NO OMOSCHEDASTICITÃ, noto nadamento parabolico 
shapiro.test(g$residuals) #OK
plot(dataADJ$GDP,dataADJ$Happi)
abline(a=reg$coefficients[1],b=reg$coefficients[2])#Non Ã¨ una bella interpolazione