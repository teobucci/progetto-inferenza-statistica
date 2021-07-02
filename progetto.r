f <- file.choose()
wine <- read.csv(f)

View(wine)
# Dimensioni
dim(wine)
# Overview delle prime righe
head(wine)

#Look at the main statistics for each covariate:
summary(wine)

g = lm( pH ~ ., data = wine )

summary( g )   #sembra fico
