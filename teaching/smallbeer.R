## small beer dataset
beer <- read.csv("smallbeer.csv", 
	colClasses=c(rep("factor",3),rep("numeric",2)))
print(nrow(beer))

## dumbfit
( allforone <- lm(log(units) ~ log(price), data=beer) )
oneforall <- lm(log(units) ~ log(price)*item, data=beer)
hist(coef(oneforall)) ## super noisy zeros

## build some regression designs
library(gamlr)
xitem <- sparse.model.matrix(~item-1, data=beer)
xweek <- sparse.model.matrix(~week-1, data=beer)

# parse the item description text 
library(tm)
descr <- Corpus(VectorSource(as.character(beer$description)))
xtext <- DocumentTermMatrix(descr)
xtext <- sparseMatrix(i=xtext$i,j=xtext$j,x=as.numeric(xtext$v>0), # convert from stm to Matrix format
              dims=dim(xtext),dimnames=dimnames(xtext))

# wrap them together
xx <- cBind(xweek, xitem, xtext)
xtreat <- cBind(1,xtext,xweek)
colnames(xtreat)[1] <- "(baseline)"
zebra <- match(levels(beer$item),beer$item)
xtest <- xtext[zebra,]
rownames(xtest) <- beer$description[zebra]

# fit the naive ML
naiveml <- gamlr(x=cBind(xtreat*log(beer$price), xx), 
				y=log(beer$units),
				free=1, standardize=FALSE)
naiveb <- coef(naiveml)
naiveel <- drop(naiveb[2,1] + xtest%*%naiveb[(1:ncol(xtext))+2,] )
hist(naiveel)

# Orthogonal ML insteal
# OML steps 1-2
pfit <- gamlr(x=xx, y=log(beer$price), lmr=1e-5, standardize=FALSE)
qfit <- gamlr(x=xx, y=log(beer$units), lmr=1e-5, standardize=FALSE)
# Calculate residuals
lpr <- drop(log(beer$price) - predict(pfit, xx))
lqr <- drop(log(beer$units) - predict(qfit, xx))
# Run 3rd ML step to get gammas
ofit <- gamlr(x=(lpr*xtreat), y=lqr, standardize=FALSE, free=1)
gams <- coef(ofit)[-1,]

# translate into elasticities and plot
el <- drop(gams[1] + xtest%*%gams[(1:ncol(xtext))+1])
hist(el, xlab="OML elasticities", xlim=c(-6,1), col="lightblue", main="",breaks=7)

# high and low sensitivity brands
names(sort(el)[1:5])
names(sort(-el)[1:5])
