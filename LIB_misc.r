

vectorMin = function(aa,bb){
	sapply(seq_along(aa),function(ii){min(aa[ii],bb[ii])})
}#function

movingAverage = function(x,n=5){
	filter(x,rep(1/n,n), sides=2)
}

can.be.numeric <- function(x) {
    stopifnot(is.atomic(x) || is.list(x)) # check if x is a vector
    numNAs <- sum(is.na(x))
    numNAs_new <- suppressWarnings( sum(is.na(as.numeric(x))) )
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

rescale = function(x){
	return <- (x-min(x))/(max(x) - min(x))	
}

##------------------------------------ special functions ------------------------------------##
fivedayblockbaseflow = function(x){
	## 5 day block
	num5dblock=as.integer(length(x)/5)
	blockID = c(rep(1: num5dblock,each=5),rep(num5dblock+1,(length(x)-5* num5dblock) ))
	blockIDx_ = c(rep(1:5, num5dblock),1:(length(x)-5* num5dblock) )
	blockIDx = which(blockIDx_==3) ##<<< ----- missing one at the end
	if(length(blockIDx)<max(blockID)){ blockIDx = append(blockIDx, length(x)) }
	
	minima = tapply(x,blockID, min, na.rm=T) #grpMins(x, blockID)
	baseline = rep(NA,length(minima))
	for(i in 2:(length(baseline)-1)){
		if(0.9*minima[i]<minima[i-1] | 0.9*minima[i]<minima[i+1] ){baseline[i]=minima[i]}
	}#i
	i=length(baseline);baseline[i]=minima[i]; #if(0.9*minima[i]<minima[i-1]){baseline[i]=minima[i]}
	i=1; baseline[i]=minima[i]; #if(0.9*minima[i]<minima[i+1]){baseline[i]=minima[i]}
	
	#plot(x,type='l')
	#points(blockIDx, baseline,col="red")
	
	cond = !is.na(baseline)
	baseflowPt = baseline[cond]
	baseflowPtx = blockIDx[cond]
	tmp =approx(x= baseflowPtx,y=baseflowPt,xout=1:length(x),yleft=baseflowPt[1],yright=baseflowPt[length(baseflowPt)] )
	baseflow=tmp$y
	for(i in 1:length(baseflow)){
		if(!is.na(x[i])){if(baseflow[i]>x[i]){baseflow[i]=x[i]}}
	}

	return <- baseflow
	#lines(baseflow,lty=2,col="green")
}








