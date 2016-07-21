##### **** Orange Juice **** #####

OJ <- read.csv("OJ.csv")
OJ$minutead <- factor(OJ$minutead)
OJ$dmnckad <- factor(OJ$dmnckad)
OJ$tropicad <- factor(OJ$tropicad)

## plot the data 
par(mfrow=c(2,3))
cols=c(8,4)
plot(log(minutevol) ~ log(minuteprice), col=minutead, pch=20, cex=.75, data=OJ)
legend("topright", fill=1:2, legend=c("No Ad", "Ad"), title ="MinuteMaid", bty="n")
plot(log(minutevol) ~ log(tropicprice), col=minutead, pch=20, cex=.75, data=OJ)
plot(log(minutevol) ~ log(dmnckprice), col=minutead, pch=20, cex=.75, data=OJ)
plot(log(minuteprice) ~ log(dmnckprice), col=minutead, pch=20, cex=.75, data=OJ)
plot(log(minuteprice) ~ log(tropicprice), col=minutead, pch=20, cex=.75, data=OJ)
plot(log(tropicprice) ~ log(dmnckprice), col=minutead, pch=20, cex=.75, data=OJ)
## minute price and volume are clearly negatively correlated
## both brand prices are postiviely correlated with minute price,
## as well as with minute vol 

## Price elasticity of -2.6 ignoring other brands.  So sales decrease by 2.6% after 1% price rise.
summary( Minutelm <- lm(log(minutevol) ~ log(minuteprice), data=OJ) )

## Clear significance for everything; both tropic price and dmnck price act to increase price.
## This seems like a natural product of competition.
summary( Pricelm <- lm(log(minuteprice) ~ log(tropicprice) + log(dmnckprice), data=OJ) )

## Full price regression; everything is very significant, so other prices do have an effect on sales
## Increase the competition's prices, and I'll sell more juice.
summary( Complm <- lm(log(minutevol) ~ log(minuteprice) + log(tropicprice) + log(dmnckprice), data=OJ) )

## The elasticity has jumped to -4.1 from -2.6 after adding tropic and dmnck prices.
## However, we know that both tropic and dmnck are functions of minute price:
summary(dlm <- lm(log(dmnckprice)~log(minuteprice), data=OJ))
summary(tlm <- lm(log(tropicprice)~log(minuteprice), data=OJ))
## In both case, we get log(minuteprice) is some constant plus about 1/2 log(otherbrandprice).
## So, to re-write the elasticities for Fulllm,
## log(vol) = constant + -4.1*log(minp) +1.6*log(tropicp) + 1.3*log(dmnckp)
##          = constant + -4.1*log(minp) + 1.6*(constant + log(minp)/2) + 1.3*(constant + log(minp)/2)
##          = constant + (-4.1 + 1.6/2 + 1.3/2)*log(minp) = constant - 2.65*log(minp).
## Hence, writing other brand prices as functions of minp, we get back something near our original elasticity.

## Finally, incorporate Ad effects (including interaction)
## Price elasticity of -2.6 ignoring other brands.  So sales decrease by 2.6% after 1% price rise.
summary( Adlm <- lm(log(minutevol) ~ log(minuteprice)*minutead, data=OJ) )
## The price elasiticity is "steeper" with ads; this is because your buying market now includes those unfamiliar with your brand

## A full model with lots of interactions
summary( Fulllm <- lm(log(minutevol) ~ log(minuteprice)*minutead*(log(tropicprice)*tropicad + log(dmnckprice)*dmnckad), data=OJ) )
## Looks like you can get rid of some variables (competing brand prices). We'll talk about this next week...

## Residual plots:
par(mfrow=c(1,3))
plot(Fulllm$fitted, rstudent(Fulllm), pch=20, col=OJ$minutead,
     xlab="Fitted Values", ylab = "residual")
hist(rstudent(Juicelm), col=8, xlab="studentized residuals")
qqnorm(rstudent(Juicelm), main="Normal Q-Q plot for r")
abline(a=0, b=1, col=8, lty=2)
## looks great!


##### **** Income and Vote **** #####

vote <- read.csv("Election2008byState.csv") 
vote$BOshare <- vote$OBAMA/(vote$OBAMA+vote$MCCAIN)
summary(regINC <- lm(BOshare ~ INC, data=vote))

## Fit the simple model and look at residuals
par(mfrow=c(1,2))
plot(BOshare~INC,col=0, xlab="INC", main="vote vs Income", data=vote)
abline(regINC, lty=2, col=2)
text(vote$INC, vote$BOshare, labels=vote$STATE)
plot(regINC$fitted, rstudent(regINC), col=0,
     xlab="fitted",
     main="residuals vs fitted")
abline(h=0, col=2, lty=2)
text(regINC$fitted, rstudent(regINC), labels=vote$STATE)
## DC is a clear outlier (not really a state)
## Remove it:
vote <- vote[vote$STATE!="DC",]
## and re-fit the model
summary(regINC <- lm(BOshare ~ INC, data=vote))
## DC has almost no leverage, so the results don't change

## look to see which variables are correlated with the residuals
par(mfrow=c(1,3))
plot(regINC$resid~AGE,col=0, data=vote)
text(vote$AGE, regINC$resid, labels=vote$STATE)
plot(regINC$resid~POPDENS,col=0, data=vote)
text(vote$POPDENS, regINC$resid, labels=vote$STATE)
plot(regINC$resid~CHURCH,col=0, data=vote)
text(vote$CHURCH, regINC$resid, labels=vote$STATE)

# CHURCH looks very influential (and it is collinear with Age)
summary(ChurchIncLM <- lm(BOshare ~ INC+CHURCH, data=vote))
par(mfrow=c(1,2))
plot(vote$BO, ChurchIncLM$fitted, col=0, xlab="BOshare", ylab="fitted")
text(vote$BO, ChurchIncLM$fitted, labels=vote$STATE)
plot(ChurchIncLM$fitted, rstudent(ChurchIncLM), col=0,
     xlab="fitted",
     main="residuals vs fitted")
abline(h=0, col=2, lty=2)
text(ChurchIncLM$fitted, rstudent(ChurchIncLM), labels=vote$STATE)
## Wyoming is a big outlier; not sure why.
## Even including popdens does not get rid of it.
## It has low leverage anyways.

### At the end of the day, it appears that BO's expected vote share
# relative to McCain increased about a third of a percent
# for every $1000 increase in a states median wage
ChurchIncLM$coef[2]*1000

## A Twist: Compare to finer income dynamics
income <- read.csv("Income-ObamaVoteShare.csv")
minc <- income$INCOME
plot(income$USA, type="l", lwd=2, xlim=c(1.25,8.25), main="Vote share by income group",
     xlab="Income in $1000", ylab="BOvote", ylim=c(0,100), xaxt="n")
for(i in 3:5) lines(income[,i], col=i-1, lwd=2)
text(x=8.25, y=income[8,2:5], labels=names(income)[2:5])
axis(1, at=1:8, labels=minc)
## We get the opposite result of the slope from our state
# income regression model; this is an instance of "simpson's paradox".
# To see the paradox in action, consider this example of
# within group positive slope and between group negative slope:
x <- 1:5
y1 <- -3 - x
y2 <- 8 - 3*x
plot(x, y1, pch=20, col=2, xlim=c(1,5), ylim=range(c(y1,y2)), ylab="y")
lines(x, y1, col=2)
points(x, y2, pch=20, col=4)
lines(x, y2, col=4)
btwgroup.slope <- mean(y2)-mean(y1)
abline(a = mean(y1)-btwgroup.slope*mean(x), b= btwgroup.slope, lty=2)
# Andrew Gelman's book "Red State, Blue State, Rich State, Poor State"
# discusses the problem in specific context of income and voting patterns.
