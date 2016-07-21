#### ******** Lecture Code ********** ####

## Pickup data visualization  
pickup <- read.csv("pickup.csv")
names(pickup)

## R's summary function is pretty clever
summary(pickup)

# Histograms
par(mfrow=c(1,3)) # break the plot into 1x3 matrix (mfrow="multiframe row-wise")
hist(pickup$year, col=grey(.4), border=grey(.9))
hist(pickup$miles, col=grey(.4), border=grey(.9))
hist(pickup$price, col=grey(.4), border=grey(.9))

# Scatterplots
par(mfrow=c(1,2))
plot(price~year, col=make, data=pickup, pch=20)
plot(price~miles, col=make, data=pickup, pch=20)
legend("topright", fill=1:3, legend=levels(pickup$make), cex=.7) 

# Boxplot
par(mfrow=c(1,1))
plot(log(price)~make, data=pickup, col=8)


#### Housing data: just price (in $100,000) vs size (in 1000 sq.ft.) ######
size <- c(.8,.9,1,1.1,1.4,1.4,1.5,1.6,1.8,2,2.4,2.5,2.7,3.2,3.5)
price <- c(70,83,74,93,89,58,85,114,95,100,138,111,124,161,172)
plot(size, price, pch=20)
print( n <- length(size) )

## Simple regression
reg <- lm(price ~ size) 
b1 <- cor(price,size)*sd(price)/sd(size)
b0 <- mean(price) - mean(size)*b1
cbind(b0,b1)


#### ******** Example 1: Rent Data ********* #####

rent <- read.csv("rent.csv")

par(mfrow=c(1,3))
boxplot(Rent ~ Bathrooms, data=rent)
boxplot(Rent ~ AC, data=rent)
boxplot(Rent ~ Parking, data=rent)

## Bathrooms looks like the most influential factor
plot(Rent ~ Rooms, data=rent, pch=20, col=as.factor(Bathrooms))
plot(Rent ~ YearBuilt, data=rent, pch=20, col=as.factor(Bathrooms))
plot(Rent ~ SqFt, data=rent, pch=20, col=as.factor(Bathrooms))
legend("topright", fill=1:3, legend=levels(as.factor(rent$Bathrooms)), title="Baths") 

## Sq Ft looks the most influential, and it also 
## correlates well with the # of bathrooms
par(mfrow=c(1,3)) 
boxplot(Rent, col=7, xlab="marginal", data=rent)
boxplot(Rent ~ Bathrooms, xlab="Bathrooms",
        main = "Rent Distribution", col=7, data=rent)
boxplot(Rent ~ Rooms, xlab="Rooms", col=7, data=rent)
# Looks like rent increases with the # of (bath)rooms

## There are some very high SqFt places which look like outliers
## removing these in R is very easy, but you could also just do it in excel
rent <- rent[rent$SqFt < 25]
## run the regression with 'lm'
reg <- lm(Rent ~ SqFt, data=rent)
summary(reg)

## Forecasting
yhat = reg$coef[1] + 14.8*reg$coef[2]
## clunky, but useful later with high-dimensional data
predict(reg, newdata=list(SqFt=14.8), se.fit=TRUE, interval="prediction")



#### ******** Example 2: Newspaper Circulation ********* #####

news <- read.csv("newspaper.csv") 
# plot the data points
plot(Sunday~Daily, pch=20, data=news)
# alternatively, plot the data with labels...
plot(Sunday~Daily, col=0, xlim=c(100,1300), data=news)
text(x=news$Daily, y=news$Sunday, labels=news$Newspaper, cex=.8)
## Things look linear, and this makes sense:
## more daily readers leads to more potential sunday readers
# (ii) The regression and confidence intervals:
newsreg <- lm(Sunday ~ Daily, data=news)
confint(newsreg, level=.95)
# Huge variability in the intercept, but the slope is solidly >1.
## (iii)
# H0: B1 = 0; HA: B1 != 0
# The test statistic is 1.33971/0.07075 = 18.935.
# P(Z_32 > 18.935) = 0 (basically), so you would reject the null.
# That is, daily circulation is significantly correlated with sunday.
## (iv)
# You can use the predict function to get everything:
predict(newsreg, newdata=data.frame(Daily=200),
        se.fit=TRUE, interval="prediction", level=.95)
# in the output, you have:
#    - the prediction mean and 95\% interval ($fit)
#    - the standard error of the mean ($se.fit)
#    - the degrees of freedom ($df)
#    - the standard error of the residuals ($residual.scale)


##### ******** Example 3: Classic Height Regression ******** ######

height <- read.csv("MBA-hgt.csv")
## check out the correlation
cor(height)
## plots
colors <- c(4,6) ## blue and purple
par(mfrow=c(1,2))
plot(SHGT ~ FHGT, pch=20, col=colors[Female+1], data=height)
plot(SHGT ~ MHGT, pch=20, col=colors[Female+1], data=height)
legend("topleft", col=colors, lwd=2, legend=c("male","female"))
## It looks like male and female heights
## have different intercepts in each case
## Seems that mom is the better linear predictor
summary(reg.male <- lm(SHGT ~ MHGT, data=height[height$Female==0,]))
summary(reg.female <- lm(SHGT ~ MHGT, data=height[height$Female==1,]))
abline(reg.male, col=4)
abline(reg.female, col=6)
## I am 6ft and my mom is 5'2.  Residual is thus:
72 - (42 + .4494*62)

#### ******** Example 4: Crime Rates ******** #####

crime <- read.csv("crime.csv")
par(mfrow=c(1,5))
## loops are super useful in R.  Here, we just use it to save time.
for(i in 1:5){ plot(crime[,1] ~ crime[,i+1], pch=20, xlab=names(crime)[i+1], ylab="Crime Rate") }
## correlation
cor(crime)
# There is a high correlation between CR and POLICE,
# but it is most plausible that CR causes high POLICE;
# hence the police budget is a sort of derivitive response
# and POLICE is not very useful for understanding crime.
summary( crimereg <- lm(CR ~ INC, data=crime) )
# The t-value for the LS slope coefficient (.018) leads to
# a p-value of 0.002, which is significant at the alpha=0.05 level.
par(mfrow=c(1,1))
plot(crimereg$fitted, crimereg$resid, pch=20)
# No obvious problems; looks like a decent fit
predict(crimereg, newdata=data.frame(INC=2800),
        interval="prediction", level=.9)
## The interval includes negative crime rates!
#  This exposes the danger of prediction at the boundaries.
#  Perhaps log(CR) would be a better thing to look at...


##### ***** Example 5: CAPM regression ***** #####

mkt <- read.csv("mktmodel.csv")
stocks <- mkt[,-1]
SP500 <- mkt$SP500

## some fancy plotting
plot(SP500, col=0, ## Just get the plot up
     xlab = "Month", ylab = "Returns",
     main = "Monthly returns for 1992-1996",
     ylim=range(unlist(mkt)))
colors <- rainbow(30)  ## 30 different colors
for(i in 1:30){ lines(stocks[,i], col=colors[i], lty=2) }
lines(SP500, lwd=2)

### Get the correlations
mcor <- cor(stocks, SP500)
mcor[order(mcor),] # sort them in increasing order
## The popular saying is that "GE is the market"...
## this is because of the diversity of GE holdings.

#### plot the alphas and betas
mreg <- lm(as.matrix(stocks) ~ SP500)
plot(mreg$coef[2,], mreg$coef[1,], col=0, xlab="beta", ylab="alpha", main="Market CAPM")
text(mreg$coef[2,], mreg$coef[1,], labels = names(stocks))
## recall what I said about high-dimensional stats and the scatterplot... 
