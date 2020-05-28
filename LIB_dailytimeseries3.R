
## ---- test inputs
# beginning = as.Date('1999-3-15')
# ending = as.Date('2000-12-1')


## ---- labeling many organization information for the daily time series
dailyTimeSeries = function(x,beginning=NA,ending=NA){
	# x is a vector of dates in incrasing order
	if( is.na(beginning) ) beginning = x[1]
	if( is.na(ending) ) ending = x[length(x)]
	period = seq.Date(from=beginning, to=ending ,by="day") 
	filter = match(x, period)
	
	periodVAR = as.POSIXlt(period)
	
	periodVAR_name = c('year','month','day','doy','wday','wy','woy','woy7')
	grpTable = data.frame(year=periodVAR$year+1900); 
	grpTable$month = periodVAR$mon+1 ##-----<< annually cyclic >>
	grpTable$yy_month = match(grpTable$year*100+grpTable$month, unique(grpTable$year*100+grpTable$month))
	grpTable$day = periodVAR$mday #1-31
	
	## --- day of year
	grpTable$doy = periodVAR$yday+1 #1-366
	
	## --- weekday, week of year, 7-day grouping
	grpTable$wday = periodVAR$wday+1 #1-7 (weekdays)
	
	tmp = as.numeric(strftime(periodVAR,'%V'))+100*as.numeric(strftime(periodVAR,'%G'))+1000000*grpTable$year
	#grpTable$woy = week of the year does not make sense for analysis
	grpTable$yy_woy = match(tmp,unique(tmp))   
	
	grpTable$woy7 = grpTable$doy%/%7+1 ##-----<< annually cyclic >>
	grpTable$yy_woy7 = match(grpTable$year*100+grpTable$woy7, unique(grpTable$year*100+grpTable$woy7))
	
	## --- water year always starts from Oct 1.
	start_month=10; grpTable$wy = grpTable$year + ifelse(grpTable$month>=start_month,0,-1) # offset, start_month=10
	
	## -- 52 week year cycle
	grpTable$w52yy = seq_len(dim(grpTable)[1])%/%(52*7)+1
	
	## --- 
	grpTable$ydecimal = grpTable$year+ grpTable$doy/sapply(grpTable$year,function(xx){ifelse(xx%%400==0,366,ifelse(xx%%4==0 & xx%%100!=0, 366, 365))})

		
	return <- grpTable[filter,]
	
}#function
	


## ---- convert cbind(day,month,year) to Date vector
v2Date = function(x, format='%d-%m-%Y'){
	return <- as.Date(sapply( seq_len(dim(x)[1]), FUN=function(i,x){ return <-paste(x[i,1], x[i,2], x[i,3],sep="-")  },x ), format)
}#function

## ---- find the intersection period among time series
intersectDate = function(xlist){
	# xlist as list of time series
	if( length(xlist)>1 ){
		if(length(xlist)>2){
			tmp = intersect(xlist[[1]], xlist[[2]])
			for(i in 3:length(xlist)){
				tmp = intersect(tmp, xlist[[i]])
			}#i
			if(length(tmp)==0) return <- NA
			else return <- as.Date(tmp, origin="1970-01-01" )
		}else return <- as.Date(intersect(xlist[[1]], xlist[[2]]), origin="1970-01-01" )
	}else return <- xlist[[1]]
}#

## ---- find the union period among time series
unionDate = function(xlist){
	# xlist as list of time series
	tmp <- as.Date( unique(unlist(xlist)), origin="1970-01-01" )
	return <- seq.Date(from=min(tmp), to=max(tmp) ,by="day")
}#

## ---- extend time series
LeapCond = function(x){ return <- x%%400==0 | (x%%4==0 & x%%100!=0) }
extendingTimeSeries = function(inputDates,NumOfYear=100){
	
    #inputDates = rain.date # for debug
    minusFromLeap = (1:366)[-60] # feb 29
    plusToLeap = c(1:60,60,61:365)
	dataYears = as.numeric(format(inputDates,'%Y'))
	dataYearsIndex = tapply(seq_along(dataYears), dataYears,function(x){return <- x})
	countDayYear = tapply(inputDates, dataYears,length); countDayYear
	completedYears = as.numeric(names(countDayYear)[countDayYear>=365])
	repeatedTimes = ceiling(NumOfYear/length(completedYears))
	completedYears_LeapCond = LeapCond(completedYears)
	
    incompletedYears = as.numeric(names(countDayYear)[countDayYear<365])
    
	# ... proposed time 
	proposedYears = rev(seq(completedYears[length(completedYears)],length.out=(repeatedTimes*length(completedYears)), by=-1))
	proposedYears_LeapCond = LeapCond(proposedYears)
	
	# ... find the missing years
	missingCond = -match(completedYears,proposedYears)
	missingPattern = rev(proposedYears_LeapCond[missingCond]); missingPatternLen = length(missingPattern)
	dataPattern = rev(completedYears_LeapCond); dataPatternLen = length(dataPattern)
    # rev(proposedYears[missingCond])
    # rev(completedYears)
    # try to match the leap year patterns
    
	trials = do.call(cbind, lapply(0:(dataPatternLen-1), function(i){
		do.call(cbind, lapply(0:(dataPatternLen-1), function(j){
			if( (1+i)<(dataPatternLen-j) ){
				tmp = dataPattern[(1+i):(dataPatternLen-j)]
				repTime = ceiling(missingPatternLen/length(tmp))
                cbind(
                    rep(tmp,repTime)[seq_len(missingPatternLen)],
                    missingPattern)
				return <- c(
					sum(rep(tmp,repTime)[seq_len(missingPatternLen)] == missingPattern) / missingPatternLen,
					length(tmp),
					i,j, repTime)
			}else{
				return <- c(0,0,i,j,0)
			}
		}) )#j
	}))#i
    # trials[1,] = ZEROs -> what does it mean?
    passed_trials = trials[,which(trials[1,]>=max(trials[1,])), drop=F] #trials[,trials[1,]>0]
	starti = passed_trials[,order(passed_trials[2,],decreasing=T)][3,1]
	endj = passed_trials[,order(passed_trials[2,],decreasing=T)][4,1]
	repTim = passed_trials[,order(passed_trials[2,],decreasing=T)][5,1]
	repatingYears = rev((rep(rev(completedYears)[(1+starti):(dataPatternLen-endj)], repTim))[seq_len(missingPatternLen)])
    repatingYears_LeapCond = LeapCond(repatingYears)
    repatingYearsAdjust_Cond = rev(missingPattern) - repatingYears_LeapCond
    # cbind(repatingYears_LeapCond, rev(missingPattern))
    # cbind(repatingYears, repatingYearsAdjust_Cond)
    
    ## find incompletedYears (future years) that are great than completedYears
    tobeIncludedincompletedYears = incompletedYears[incompletedYears>max(completedYears)]
    allYears = c(repatingYears,completedYears,tobeIncludedincompletedYears)
    allYears_dataYearsIndex_adjust = c(repatingYearsAdjust_Cond,rep(F,length(completedYears)),rep(F,length(tobeIncludedincompletedYears)))
    finalIndex = do.call(c,lapply(seq_along(allYears), function(ii){
        if(allYears_dataYearsIndex_adjust[ii]<0){
            return <- dataYearsIndex[[ toString(allYears[ii]) ]][minusFromLeap]
        }else if(allYears_dataYearsIndex_adjust[ii]<0){
            return <- dataYearsIndex[[ toString(allYears[ii]) ]][plusToLeap]
        }else{
            return <- dataYearsIndex[[ toString(allYears[ii]) ]]
        }
    }))#do.call
    
    return <- list(
		extended_Index = finalIndex,	 # finalIndex
		extended_Dates = rev( seq( inputDates[finalIndex[length(finalIndex)]], length.out=length(finalIndex), by=-1) )
	)#list
}#

## ---- select days for plotting
monthlyTick = function(dd){
	x <- as.POSIXlt(dd)
	return <- dd[x$mday==1]
}#function
	
quarterlyTick = function(dd){
	x <- as.POSIXlt(dd)
	return <- dd[x$mday==1 & x$mon%in%(c(1,4,7,10)-1) ]
}#function
	
annualTick = function(dd){
	x <- as.POSIXlt(dd)
	return <- dd[x$mday==1 & x$mon==0]
}#function


quantileCorrection = function(obs, pred, obsIndex=2, predIndex=2){
	# obs1 is data.frame with Date object
	# pred is a data.frame with Date object
	# breakdown time into DOY
	plotTime = intersectDate(list(obs$date, pred$date)) 
	obs.dtsm = match(plotTime, obs$date)
	pred.dtsm = match(plotTime, pred$date)
	
	obsDOY = as.POSIXlt(obs$date[obs.dtsm])$yday+1
	predMatchedDOY = as.POSIXlt(pred$date[pred.dtsm])$yday+1
	predDOY = as.POSIXlt(pred$date)$yday+1
	
	correctedPred = rep(NA,dim(pred)[1])
	for(ii in 1:366){
		cond = abs(obsDOY-ii) <=15 | abs(obsDOY-ii-366) <=15
		dayGroup = unique(obsDOY[cond])
		
		obsData = obs[obs.dtsm, obsIndex][obsDOY %in% dayGroup]
		predData = pred[, predIndex][predDOY==ii] #<<------- full series!!
		
		ECDF = ecdf(pred[pred.dtsm, predIndex][predMatchedDOY %in% dayGroup])
		correctedPred[predDOY==ii] = sapply(predData,function(xx){ quantile(obsData, ECDF(xx)) })
		
	}# for ii for each DOY
	
	return <- correctedPred
	
}#function
