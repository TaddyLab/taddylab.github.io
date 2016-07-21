
###### Amazon Sales ######

names(amazon <- read.csv("amazon.csv", colClasses=c("numeric", "factor",  "numeric",  
                                         "factor", "factor", "factor", "factor", "factor", "factor")))

n <- nrow(amazon)
mains <- lm(log(bought) ~ ., data=amazon) 
plot(mains$fitted, mains$resid, bg=c(2,3)[amazon$connect], pch=21, main="full main effects model")

## problems!  Why?  Really low values (penny charges).
amazon[mains$resid< -6,]

## redo everything without purchases < $0.10
amazon <- amazon[amazon$bought>=0.1,]
summary( mains <- lm(log(bought) ~ ., data=amazon) )

## now simplify
inc = as.numeric(amazon$income)
inc[inc <= 2] <- "poor"
inc[inc <= 4] <- "mid"
inc[inc <= 6] <- "upper"
inc[inc == 7] <- "rich"
amazon$income <- relevel(as.factor(inc),"poor")

mainsold <- mains
summary( mains <- lm(log(bought) ~ ., data=amazon)  )
anova(mains, mainsold)

## simplify further
incregionrace <- lm(log(bought) ~ region + income + race + connection, data=amazon)
anova(incregionrace, mains)
# can we get away without race? 
incregion <- lm(log(bought) ~ region + income +  connection, data=amazon)
anova(norace, incregionrace)
# Region only
regiononly <- lm(log(bought) ~ region + connection, data=amazon)
anova(regiononly, incregionrace)
# An interaction model
interact <- lm(log(bought) ~ region*connection, data=amazon)
anova(regiononly, interact)

# full and null models
null <- lm(log(bought)~1, data=amazon)
full <- lm(log(bought)~.+.^2, data=amazon)

BIC <- c(mains=extractAIC(mains, k=log(n))[2],
         incregionrace=extractAIC(incregionrace, k=log(n))[2],
         incregion=extractAIC(incregion, k=log(n))[2],
         interact=extractAIC(interact, k=log(n))[2],
         regiononly=extractAIC(regiononly, k=log(n))[2],
         null=extractAIC(null, k=log(n))[2],
         full=extractAIC(full, k=log(n))[2])

# Model probabilities
eBIC <- exp(-.5*(BIC-min(BIC)))
round(probs <- eBIC/sum(eBIC), 2)

par(mfrow=c(1,2))
plot(regiononly$fitted, regiononly$resid, bg=c(2,3)[amazon$connect], pch=21, main="connection and region model")
qqnorm(rstudent(regiononly), main="connection and region model")

# Having broadband increases your purchases on amazon by about 50%
summary(regiononly)
exp(0.422)
summary(incregion)
exp(0.404)
summary(incregionrace)
exp(0.403)


##### Police Stops #####

traffic <- read.csv("WSPTrafficStops.csv")
Stops <- traffic$Stops
Radar <- traffic$Radar

# (i)
par(mfrow=c(2,3))
plot(Radar, Stops, pch=20, main="Stops vs Radar") # obvious non-constant variance
##
# Clearly, this is a candidate for the log-log transform
radarlm <- lm(log(Stops) ~ log(Radar))
plot(log(Stops), log(Radar), pch=20, col=4, main="log(stops) vs log(radar)")
# The relationship appears linear, which fits with our proposition that
# Radar is providing a baseline for the "at risk of traffic stop" population.
plot(radarlm$fitted.values, radarlm$residuals, pch=20, col=4,
     xlab="Yhat", ylab = "e", main="fitted vs residuals") # things look much better
qqnorm(rstudent(radarlm), pch=20, col=4) # check normality
abline(a=0, b=1, lty=2)
hist(rstudent(radarlm), col=4)
# We can also check error independence for the (spatially ordered) APAs
plot(traffic$APA, radarlm$residuals, pch=20, col=4,
     xlab="APA", ylab = "e", main="Error by APA")
# It looks like we fail this assumption (perhaps lower numbers are urban,
# closer to seattle?), but that is a more advanced topic (time-series lecture).
##
# From the elasticity interpretation of beta1,
# the increased % in active Stops per 1% more radar stops is
confint(radarlm)[2,]
# An elasticity of 1 seems like a decent assumption, which is intuitive

# (ii)
# First, plot the data with colors to show race.
# Use relevel() to make white drivers the intercept for discrimination
par(mfrow=c(1,3))
Race <- relevel(traffic$Race, "White")
plot(log(Radar), log(Stops), pch=20, col=Race)
legend("topleft", legend=levels(Race), fill=1:6)
# We want to see if there are race effects beyond
# the number of stops predicted by our Radar baseline
summary(racelm <- lm(log(Stops) ~ log(Radar) + Race))
anova(radarlm, racelm) # Race Matters!
# Residuals still look fine 
plot(racelm$fitted.values, racelm$residuals, pch=20, col=Race,
     xlab="Yhat", ylab = "e", main="fitted vs residuals")
# To see what we fit, take a look at the prediction lines for each race:
plot(log(Radar), log(Stops), col=8, cex=.5)
abline(a=racelm$coef[1], b=racelm$coef[2])
for(i in 1:5){ abline(a=racelm$coef[1]+racelm$coef[2+i], b=racelm$coef[2], col=i+1) }
# The lines look basically the same, but it is hard to really tell...
# Use the partial F-test to see if the "racial bias" effect is significant
# H0: log(Stops) ~ log(Radar)
# H1: log(Stops) ~ log(Radar) + Race
anova(radarlm, racelm)
# So the race effect is significant at .05 level (even though
# none of the individual t-statistics are signif).
#
# In the plot, only Native Americans systymatically differed from the line.
# But in the regression ouput, Hispanics also looked fairly significant.
# So lets try with both
NatAm <- Race=="NativeAmerican"
Hisp <- Race=="Hispanic"
summary( biaslm <- lm(log(Stops) ~ log(Radar) +  NatAm + Hisp ) )
# Compare this to the previous model, to see if the "racial bias"
# only affects Native Americans, or if we need the full model
# H0: log(Stops) ~ log(Radar) + NatAm + Hisp
# H1: log(Stops) ~ log(Radar) + Race
anova(biaslm, racelm)
## Looks like the native american and hispanic regression (null H0) explains things
## We can't really assume discrimination, however,
## since those residuals look to be correlated in ADA...
## Elasticity is still about 1.

xx <- 1:10
par(mfrow=c(1,3))
plot(log(Radar)[Hisp], log(Stops)[Hisp], col=5, main="Hispanic Drivers")
lines(xx, .5795 + 1.01589*xx + .15057, col=5, lwd=2)
plot(log(Radar)[NatAm], log(Stops)[NatAm], col=6, main="Native American Drivers")
lines(xx, .5795 + 1.01589*xx + .24111, col=6, lwd=2)
plot(log(Radar)[!(Hisp|NatAm)], log(Stops)[!(Hisp|NatAm)],  main="Other Drivers")
lines(xx, .5795 + 1.01589*xx, col=1, lwd=2)


#######  Wine Quality ########

wine <- read.csv("winequality.csv")
names(wine)
winecols <- c(2,7)[wine$col] # red and yellow
par(mfrow=c(3,4), mai=c(.5,.5,.3,.3))
for(i in c(2:12)){ plot(wine[,i], wine$quality, xlab=names(wine)[i], col=winecols, pch=20) }
plot(wine$quality ~ wine$color, col=c(2,7))

## From plots, you should be able to see a need for log transforms
## and a couple high-leverage outlier x-values.
#
# I remove 3 very high-density (desert?) and 2 high citric wines
wine <- wine[(wine$density < 1.01)&(wine$citric<=1),]  
par(mfrow=c(3,4), mai=c(.5,.5,.2,.1))
# what needs to be is now on log scale:
plot(wine$fixed.acidity, wine$quality, pch=20, col=winecols)
plot(wine$volatile.acidity, wine$quality, pch=20, col=winecols)
plot(wine$citric.acid, wine$quality, pch=20, col=winecols)
plot(log(wine$residual.sugar), wine$quality, pch=20, col=winecols)
plot(log(wine$chlorides), wine$quality, pch=20, col=winecols)
plot(log(wine$free.sulfur.dioxide), wine$quality, pch=20, col=winecols)
plot(log(wine$total.sulfur.dioxide), wine$quality, pch=20, col=winecols)
plot(wine$density, wine$quality, pch=20, col=winecols)
plot(wine$pH, wine$quality, pch=20, col=winecols)
plot(log(wine$sulphates), wine$quality, pch=20, col=winecols)
plot(wine$alcohol, wine$quality, pch=20, col=winecols)
plot(wine$color, wine$quality, col=c(2,7))


train <- 1:5000 # sample(1:nrow(wine), 5000) is better, but I want consistency for class
wine <- cbind(wine[,c(1:4,9,10,12,13)], log(wine[,c(5:8,11)]))

# Fit the full model:
summary(fullreg <- lm(quality ~ ., data=wine[train,]))
# We get away with dropping citric acid
summary(mainreg <- lm(quality ~ . - citric.acid, data=wine[train,]))
# Let's consider building interactions:
summary(interreg <- step(mainreg, ~.+.^2, direction="forward", k=log(5000))) 
# And finally simplify
summary(finalreg <- lm(update(formula(interreg), ~.-chlorides-residual.sugar), data=wine[train,] )) 

# look at the residuals and fit for each model:
par(mfrow=c(2,4), mai=c(.5,.5,.2,.1))
plot(fullreg$fitted, fullreg$resid, col=winecols, pch=20)
abline(h=0, col=4, lty=2)
plot(mainreg$fitted, mainreg$resid, col=winecols, pch=20)
abline(h=0, col=4, lty=2)
plot(interreg$fitted, interreg$resid, col=winecols, pch=20)
abline(h=0, col=4, lty=2)
plot(finalreg$fitted, finalreg$resid, col=winecols, pch=20)
abline(h=0, col=4, lty=2)
qqnorm(rstudent(fullreg), col=winecols, pch=20, main="")
abline(a=0,b=1,col=4)
qqnorm(rstudent(mainreg), col=winecols, pch=20, main="")
abline(a=0,b=1,col=4)
qqnorm(rstudent(interreg), col=winecols, pch=20, main="")
abline(a=0,b=1,col=4)
qqnorm(rstudent(finalreg), col=winecols, pch=20, main="")
abline(a=0,b=1,col=4)

## Model Comparison
models <- list(full=fullreg, main=mainreg, inter=interreg, final=finalreg)
# model probabilities: 
bic <- sapply(models, extractAIC, k=log(5000))[2,]
ebic <- exp(-.5*(bic-min(bic)))
round(ebic/sum(ebic),4) # final reg rules!
# out-of-sample prediction:
preds <- sapply(models, predict, newdata=wine[-train,])
error <- apply(preds, 2, '-', wine$quality[-train])
round(mse <- apply(error^2, 2, mean),5) # Both final and inter do pretty well

# The final model is best.  Color has a (negative for white) main
# effect, as well as interaction with free.sulfur.dioxide (+) and
# volitile.acidity (-).  Thus the same chemico profile wine will be
# lower quality for whites, and the positive main effect of free.sulfur
# and negative effect of volitile acidity are both magnified for white wine.
