library( car )
library( faraway )
library( leaps )
library(MASS)
library(rgl)
library(dplyr)
library(data.table)
library(ggplot2)
library( GGally)
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
shapiro.test(g$residuals) #not OK

x11()
qqnorm( g$res, ylab = "Raw Residuals", pch = 16 )
qqline( g$res )

x11()
b = boxcox(g)
best_lambda = b$x[ which.max( b$y ) ]
best_lambda