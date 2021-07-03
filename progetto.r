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
scoliosi <- read.csv(f)

View(scoliosi)
# Dimensioni
dim(scoliosi)
# Overview delle prime righe
head(scoliosi)

print(sapply(scoliosi,function(x) any(is.na(x)))) 
print(sapply(scoliosi, typeof)) 

print(sapply(cereal,function(x) any(is.na(x)))) 
print(sapply(cereal, typeof)) 

#Look at the main statistics for each covariate:
summary(scoliosi)

x11()
ggpairs(scoliosi)

g = lm( lumbar_lordosis_angle ~ .-class, data = scoliosi )

summary( g )   #sembra fico

plot(g,which=1)#NO OMOSCHEDASTICITÃ, noto nadamento parabolico 

shapiro.test(g$residuals)

x11()
qqnorm( g$res, ylab = "Raw Residuals", pch = 16 )
qqline( g$res )

x11()
b = boxcox(g)
best_lambda = b$x[ which.max( b$y ) ]
best_lambda
