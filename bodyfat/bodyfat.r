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

# setto la working directory a quella del file sorgente
library("rstudioapi")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# carico il dataset
bodyfat = read.csv("bodyfat.csv", header = TRUE)

View(bodyfat)
# Dimensioni
dim(bodyfat)
# Overview delle prime righe
head(bodyfat)

#Look at the main statistics for each covariate:
summary(bodyfat)

x11()
ggpairs(bodyfat)

g = lm( BodyFat ~ ., data = bodyfat )

summary( g )   #sembra fico

plot(g,which=1)#NO OMOSCHEDASTICIT?, noto nadamento parabolico?
shapiro.test(g$residuals) #OK

x11()
qqnorm( g$res, ylab = "Raw Residuals", pch = 16 )
qqline( g$res )

x11()
b = boxcox(g)
best_lambda = b$x[ which.max( b$y ) ]
best_lambda