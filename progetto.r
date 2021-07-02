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
scoliosi <- read.csv(f)

View(scoliosi)
# Dimensioni
dim(scoliosi)
# Overview delle prime righe
head(scoliosi)

print(sapply(scoliosi,function(x) any(is.na(x)))) 
print(sapply(scoliosi, typeof)) 

#Look at the main statistics for each covariate:
summary(scoliosi)

x11()
ggpairs(scoliosi)

g = lm( lumbar_lordosis_angle ~ .-class, data = scoliosi )

summary( g )   #sembra fico

plot(g,which=1)#NO OMOSCHEDASTICITÃ, noto nadamento parabolico 
shapiro.test(g$residuals) #OK
plot(dataADJ$GDP,dataADJ$Happi)
abline(a=reg$coefficients[1],b=reg$coefficients[2])#Non Ã¨ una bella interpolazione