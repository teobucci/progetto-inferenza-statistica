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
scoliosi = read.csv("column_3C_weka.csv", header = TRUE)

#na.omit(scoliosi)
View(scoliosi)
# Dimensioni
dim(scoliosi)
# Overview delle prime righe
head(scoliosi)

#controllo se ci sono degli NA
print(sapply(scoliosi,function(x) any(is.na(x)))) 
print(sapply(scoliosi, typeof)) 

#Look at the main statistics for each covariate:
summary(scoliosi)

x11()
ggpairs(scoliosi)

g1 = lm( lumbar_lordosis_angle ~ .-class, data = scoliosi )

summary( g1 )   #ci sono na
#mostro che ? lin dipendente
g2 = lm(sacral_slope~ .-class-lumbar_lordosis_angle, data = scoliosi )

summary( g2 )

X = scoliosi [c(-3,-7)]
cor( X )

x11()
corrplot(cor(X), method='number')
x11()
corrplot(cor(X), method='color')
x11()
heatmap( cor( X ), Rowv = NA, Colv = NA, symm = TRUE, keep.dendro = F)
#image( as.matrix( cor( X ) ), main = 'Correlation of X' )

ggpairs(X)





g = lm( lumbar_lordosis_angle ~ .-class-sacral_slope, data = scoliosi )

summary( g )


plot(g,which=1)#NO OMOSCHEDASTICIT?
shapiro.test(g$residuals) #no normalit?

qqnorm( g$res, ylab = "Raw Residuals", pch = 16 )
qqline( g$res )

#SBAGLIATO
#boxcox sul primo, con tutto dentro
# b = boxcox(g)
# best_lambda = b$x[ which.max( b$y ) ]
# best_lambda
# 
# gb = lm( (lumbar_lordosis_angle^best_lambda -1)/best_lambda ~ .-class, data=scoliosi )
# summary( gb )
# 
# plot(gb,which=1)#NO OMOSCHEDASTICIT?, noto nadamento parabolico?
# shapiro.test(gb$residuals) 
# 
# qqnorm( gb$res, ylab = "Raw Residuals", pch = 16 )
# qqline( gb$res )

#residui studentizzati
stud = rstandard( g )

watchout_ids_stud = which( abs( stud ) > 2 )
watchout_stud = stud[ watchout_ids_stud ]
watchout_stud


plot( g$fitted.values, stud, ylab = "Studentized Residuals", main = "Studentized Residuals", pch = 16 )
points( g$fitted.values[watchout_ids_stud], 
        stud[watchout_ids_stud], col = 'pink', pch = 16 )
abline( h = c(-2,2), lty = 2, col = 'orange' )
legend('topright', col = c('pink'), 
       c('Studentized Residual'), pch = rep( 16, 3 ), bty = 'n' )


gl = lm( lumbar_lordosis_angle ~ .-class-sacral_slope, scoliosi, subset = ( abs( stud ) < 2) )
summary( gl )

plot(gl,which=1)# non male
shapiro.test(gl$residuals) #non sono normali

x11()
qqnorm( gl$res, ylab = "Raw Residuals", pch = 16 )
qqline( gl$res )


#uso boxcox
x11()
b = boxcox(gl)
best_lambdagl = b$x[ which.max( b$y ) ]
best_lambdagl

gb = lm( (lumbar_lordosis_angle^best_lambdagl -1)/best_lambdagl ~ .-class-sacral_slope, data=scoliosi,subset = ( abs(res) < 40 ) )
summary( gb )

plot(gb,which=1)#noto omoschedasticita dei residui
shapiro.test(gb$residuals) #ho normalita

qqnorm( gb$res, ylab = "Raw Residuals", pch = 16 )
qqline( gb$res )

gk = lm( (lumbar_lordosis_angle^best_lambdagl -1)/best_lambdagl ~ .-class-sacral_slope-pelvic_radius, data=scoliosi,subset = ( abs(res) < 40 ) )
summary( gk )

plot(gk,which=1)#noto omoschedasticita dei residui
shapiro.test(gk$residuals) #ho normalita

qqnorm( gk$res, ylab = "Raw Residuals", pch = 16 )
qqline( gk$res )

#ANOVA

my_colors = brewer.pal( length( levels( scoliosi$class ) ), 'Set2')  #estraggo tanti colori

x11()
boxplot( scoliosi$lumbar_lordosis_angle ~ scoliosi$class , xlab = 'class', ylab = 'lordosis',
         main = 'lumbar lordosis angle according to class', col = my_colors 
         )
abline( h = mean( scoliosi$lumbar_lordosis_angle ) )  # linea sulla media globale dell angolo bla bla

tapply( scoliosi$lumbar_lordosis_angle, scoliosi$class, length )#60-100-150
tapply( scoliosi$lumbar_lordosis_angle, scoliosi$class, mean )#35-43-64

Ps = tapply( scoliosi$lumbar_lordosis_angle,scoliosi$class , function( x ) ( shapiro.test( x )$p ) )
Ps
#non abbiamo ipotesi di normalita
Var = tapply( scoliosi$lumbar_lordosis_angle,scoliosi$class , var )
Var  #95-152-268
scoliosi$class=factor(scoliosi$class,ordered=F)
leveneTest(scoliosi$lumbar_lordosis_angle, scoliosi$class)
#rifiuto ipotesi nulla, non c'è omoschedasticità

reg=lm(scoliosi$lumbar_lordosis_angle~scoliosi$class,data=scoliosi)
summary(reg)

anB=boxcox(reg)
best_lambda=anB$x[which.max(anB$y)]
best_lambda


