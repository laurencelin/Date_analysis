
##------------------------------------ vectorize process ------------------------------------##
vectorMin = function(aa,bb){
	sapply(seq_along(aa),function(ii){min(aa[ii],bb[ii])})
}#function

movingAverage = function(x,n=5){
	filter(x,rep(1/n,n), sides=2)
}

can.be.numeric <- function(x) {
    stopifnot(is.atomic(x) || is.list(x)) # check if x is a vector
    numNAs <- sum(is.na(x))
    numNAs_new <- suppressWarnings(sum(is.na(as.numeric(x))))
    return(numNAs_new == numNAs)
}


##------------------------------------ for convenience ------------------------------------##
keyTable = function(data){
	return <- cbind(1:ncol(data), colnames(data) )
}

makeTransparent<-function(someColor, alpha=255)
{
  newColor<-col2rgb(someColor)
  apply(newColor, 2, function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2],
    blue=curcoldata[3],alpha=alpha/100*255, maxColorValue=255)})
}

NSE=function(obs,y){
	#assume no zero/NA/Inf, filtered, clean
	obsM = mean(obs,na.rm=T)
	return <- 1 - sum((obs-y)^2,na.rm=T)/sum((obs-obsM)^2,na.rm=T)
}

normalize=function(x){
	xmean=mean(x)
	return <- (x-xmean)/sd(x)
}











