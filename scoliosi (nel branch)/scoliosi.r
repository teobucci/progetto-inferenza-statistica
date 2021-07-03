library(car)
library(faraway)
library(leaps)
library(MASS)
library(GGally)
library(rgl)
library(dplyr)
library(data.table)
library(ggplot2)
library(corrplot)
library(RColorBrewer)
library(AID)
library(onewaytests)

# setto la working directory a quella del file sorgente
library("rstudioapi")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# carico il dataset
scoliosi = read.csv("column_3C_weka.csv", header = TRUE)

# visualizza il dataset
View(scoliosi)

# che dimensioni ha
dim(scoliosi)
# 310 osservazioni e 7 covariate, di cui 1 categorica

# Overview delle prime righe
head(scoliosi)

#controllo se ci sono degli NA
print(sapply(scoliosi,function(x) any(is.na(x)))) 
print(sapply(scoliosi, typeof)) 
# tutto FALSE, non ci sono NA, altrimenti avremmo fatto na.omit(scoliosi)

#sommario del dataset
summary(scoliosi)

x11()
# faccio il mega ccpairg per avere un'idea dei dati
ggpairs(scoliosi)

# generiamo il primo modello lineare, come risposta "lumbar_lordosis_angle"
# escludiamo "class" che è la categorica
g1 = lm( lumbar_lordosis_angle ~ .-class, data = scoliosi )

# esaminiamolo
summary( g1 )

# R^2_adj iniziale abbastanza buono come punto di partenza 0.5272
# molto significativo "pelvic_incidence"
# p-value dell'F-test 2.2e-16, c'è evidenza per dire che qualche covariate
# sia poco significativa

# ci sono NA in corrispondenza di "sacral_slope", cercando su Google
# scopriamo che è indice di lineare indipendenza tra le covariate, procediamo
# quindi subito con l'analisi di questo aspetto

# prevediamo "sacral_slope" in funzione di tutto il resto, tranne la nostra
# risposta originale e la categorica
g2 = lm(sacral_slope~ .-class-lumbar_lordosis_angle, data = scoliosi )

# vediamo cosa otteniamo
summary( g2 )
# osserviamo che R^2_adj è esattamente 1, osservando i beta dei parametri
# scopriamo che 
# sacral_slope + pelvic_tilt = pelvic_incidence
# cercando su internet nei siti di ambito medico abbiamo conferma di questa cosa
# di conseguenza escludiamo questa covariata

# prima di farlo, vediamo la correlazione tramite dei grafici
# X è un sotto-dataset escludendo la risposta e la categorica
X = scoliosi [c(-3,-7)]
cor( X )

x11()
corrplot(cor(X), method='number')
# le correlazioni sono 1, 0.63, 0.81, a conferma di quanto detto

x11()
corrplot(cor(X), method='color')

x11()
heatmap( cor( X ), Rowv = NA, Colv = NA, symm = TRUE, keep.dendro = F)
#image( as.matrix( cor( X ) ), main = 'Correlation of X' )


# procediamo con il nostro modello escludendo la "sacral_slope" e la categorica
g = lm( lumbar_lordosis_angle ~ .-class-sacral_slope, data = scoliosi )

summary( g )
# come ci aspettavamo  l'R^2_adj è invariato a 0.5272, così come p-value 2.2e-16

# controlliamo 
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

gb = lm( (lumbar_lordosis_angle^best_lambdagl -1)/best_lambdagl ~ .-class-sacral_slope, data=scoliosi,subset = ( abs(stud) < 2 ) )
summary( gb )

plot(gb,which=1)#noto omoschedasticita dei residui
shapiro.test(gb$residuals) #ho normalita

qqnorm( gb$res, ylab = "Raw Residuals", pch = 16 )
qqline( gb$res )

gk = lm( (lumbar_lordosis_angle^best_lambdagl -1)/best_lambdagl ~ .-class-sacral_slope-pelvic_radius, data=scoliosi,subset = ( abs(stud) < 2 ) )
summary( gk )

plot(gk,which=1)#noto omoschedasticita dei residui
shapiro.test(gk$residuals) #ho normalita

qqnorm( gk$res, ylab = "Raw Residuals", pch = 16 )
qqline( gk$res )

#ANOVA

my_colors = brewer.pal( length( levels( scoliosi$class ) ), 'Set1')  #estraggo tanti colori

x11()
boxplot( scoliosi$lumbar_lordosis_angle ~ scoliosi$class , xlab = 'class', ylab = 'lordosis',
         main = 'lumbar lordosis angle according to class', col = my_colors )
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
bartlett.test(scoliosi$lumbar_lordosis_angle, scoliosi$class)

#modello buono, ma non valido
reg=lm(scoliosi$lumbar_lordosis_angle~scoliosi$class,data=scoliosi)
summary(reg)

#uso box cox
anB=boxcox(reg,lambda = seq(-3,3,by=0.01))
best_lambda=anB$x[which.max(anB$y)]
best_lambda

Ps2 = tapply( (scoliosi$lumbar_lordosis_angle^best_lambda -1)/best_lambda ,scoliosi$class , function( x ) ( shapiro.test( x )$p ) )
Ps2
#adesso ho normalita bitches
leveneTest( (scoliosi$lumbar_lordosis_angle^best_lambda -1)/best_lambda ,scoliosi$class )
#pvalue alto, ho omoschedasticita, H0 varianze intragruppi omoschedastiche
bartlett.test(  (scoliosi$lumbar_lordosis_angle^best_lambda -1)/best_lambda ,scoliosi$class )
#stesso risultato

#FUNZIONE TEO INTERNET
boxcoxfr(scoliosi$lumbar_lordosis_angle, scoliosi$class, option = "both", lambda = seq(-3, 3, 0.01), lambda2 = NULL, 
         tau = 0.05, alpha = 0.05, verbose = TRUE)

gA = lm( (scoliosi$lumbar_lordosis_angle^best_lambda -1)/best_lambda ~ class-sacral_slope, data=scoliosi )

summary(gA)  #meglio, tutti significativi
anova(gA)  #pvalue basso, rifiuto hp tutte le medie sono uguali


#4)Modello reg : studio dei punti influenti
x11()
influencePlot( gk, id.method = "identify", main = "influential Plot",
               sub = "Circle size is proportial to Cook's Distance" )


influenti_nomi=row.names(influencePlot( reg, main = "influential Plot"))#, id=list(method="identify")))
influenti=c()
for (names in influenti_nomi)
  influenti=c(influenti,which(scoliosi$class==names))
influenti
scoliosi$class[influenti]

Levscol = scoliosi[-influenti,]
Levscol = Levscol[seq(4,9,1)]

#5) int conf e prev
gp=lm(lumbar_lordosis_angle~pelvic_incidence,data=scoliosi)
grid = seq( min(pelvic_incidence), max(pelvic_incidence), 2 )

# automatic prediction
y.pred = predict( gk, data.frame( pelvic_incidence = grid ), interval = "confidence", se = T )

names( y.pred )

# y.pred$fit[ ,1 ] # predicted values \hat{y}_{new}.
# y.pred$fit[ ,2 ] # LB confidence interval for y_{new}.
# y.pred$fit[ ,3 ] # UB confidence interval for y_{new}.

y.pred$fit

##Plot the CI of predictions.
x11()
matplot( grid, cbind( y, y.inf, y.sup ), lty = c( 1, 2, 2 ), 
         col = c( 1, 'blue', 'blue' ), type = "l", xlab = "pelvic_incidence",
         ylab = "lumbar_lordosis_angle", main = 'IC per la media della risposta' )
points( pelvic_incidence, lumbar_lordosis_angle, col = "black", pch = 16 )

#__6.c__  Compute the Prediction Interval for the one new observation. In this case the standard errors are:

y.pred2 = predict( gp, data.frame( pelvic_incidence = grid ), interval = "prediction", se = T )
# fornisce direttamente gli estremi inf e sup, che prima abbiamo costruito a mano (in un altro caso)

y.pred2$fit[ ,1 ] # predicted values \hat{y}_{new}.
y.pred2$fit[ ,2 ] # LB prediction interval for y_{new}.
y.pred2$fit[ ,3 ] # UB prediction interval for y_{new}.



x11()
matplot( grid, y.pred2$fit, lty = c( 1, 2, 2 ), col = c( 1, 2, 2 ), type = "l",
         xlab = "pelvic_incidence", ylab = "lumbar_lordosis_angle", main = 'IP per singole osservazioni' )
points( pelvic_incidence, lumbar_lordosis_angle, col = "blue", pch = 16 )


##__6.d__ Compare the Intervals obtained at __6.b__ and __6.c__.
x11()
matplot( grid, y.pred2$fit, lty = c( 1, 2, 2 ), col = c( 1, 2, 2 ), type = "l", xlab = "pelvic_incidence", ylab = "lumbar_lordosis_angle", 
         main = "IC per la media e IP per singole osservazioni" )
lines( grid, y.pred$fit[ , 2 ] , col = "blue", lty = 2, xlab = "pelvic_incidence", ylab = "lumbar_lordosis_angle" )
lines( grid, y.pred$fit[ , 3 ] , col = "blue", lty = 2, xlab = "pelvic_incidence", ylab = "lumbar_lordosis_angle" )
points( pelvic_incidence, lumbar_lordosis_angle, col = "black", pch = 16 )