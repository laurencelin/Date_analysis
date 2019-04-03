
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


