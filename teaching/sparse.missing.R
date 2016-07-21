
## example of imputing missing values as zero in sparse survey data

D <- data.frame(Qa=factor(c(1,2,1,2,3,NA)),Qb=factor(c(1,2,1,1,1,2)))
print(D)

## drops a row
library(Matrix)
dim(sparse.model.matrix(~.-1, data=D[,'Qa',drop=FALSE])) 

## turn off this behavior
options(na.action=na.pass) 
Qa <- sparse.model.matrix(~.-1, data=D[,'Qa',drop=FALSE])
dim(Qa)

Qb <- sparse.model.matrix(~.-1, data=D[,'Qb',drop=FALSE],)

X <- cBind(Qa,Qb)
print(X)


## To do this at once for all questions 
## code NA as the reference level for each factor 
nadummy <- function(x){
	if(!is.factor(x)) return(x)
	lev <- levels(x)
	x <- as.character(x) # drop factor info
	x[is.na(x)] <- "!"
	x <- factor(x, levels=c("!",lev)) 
}
X <- sparse.model.matrix(~., data=lapply(D,nadummy))[,-1]
print(X)

## don't forget to turn NA omission back on
options(na.action=na.omit) 

