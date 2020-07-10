options(stringsAsFactors = FALSE)

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

makeTransparent<-function(someColor, alpha=255){
  newColor<-col2rgb(someColor)
  apply(newColor, 2, function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2],
    blue=curcoldata[3],alpha=alpha/100*255, maxColorValue=255)})
}

## https://www.dataanalytics.org.uk/make-transparent-colors-in-r/
## Transparent colors
## Mark Gardener 2015
## www.dataanalytics.org.uk
t_col <- function(color, percent = 50, name = NULL) {
	rgb.val <- col2rgb(color)
	return <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
	             max = 255,
	             alpha = (100 - percent) * 255 / 100,
	             names = name)
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

most = function(x){
	tmp = table(x);
	return <- as.numeric(names(tmp)[which.max(tmp)])
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



##------------------------------------ GIS map coloring functions ------------------------------------##
coloringMultiMAPS = function(maps,COLORS=c('red','white','blue'), DELTASIZE=NULL, BREAKS=NULL, NBLOCKS=NULL, MAXV=NULL, MINV=NULL){
    # map is list of maps
    ALL = do.call(c,lapply(seq_along(maps),function(ii){ 
    	cond = !is.na(maps[[ii]])
    	return <- maps[[ii]][cond]
    }))
    if(!is.null(MAXV)){ ALL = ALL[ALL <= MAXV] }
    if(!is.null(MINV)){ ALL = ALL[ALL >= MINV] }
    
    myoptions = c(!is.null(DELTASIZE), !is.null(BREAKS), !is.null(NBLOCKS))
    
    if(sum(myoptions==c(T,F,F))==3){
    	# use DELTASIZE
        theLOW = floor(min(ALL,na.rm=T)/DELTASIZE)*DELTASIZE
        theHIGH = ceiling(max(ALL,na.rm=T)/DELTASIZE)*DELTASIZE
        breaks_ = seq(theLOW,theHIGH,DELTASIZE);
        
    }else if(sum(myoptions==c(T,F,F))==3){
    	# use breaks_
        theLOW = min(ALL,na.rm=T)
        theHIGH = max(ALL,na.rm=T)
        breaks_ = BREAKS;
        if(min(breaks_) > theLOW) breaks_ = c(theLOW,breaks_)
        if(max(breaks_) < theHIGH) breaks_ = c(breaks_,theHIGH)
        
    }else if(sum(myoptions==c(F,F,T))==3){
    	# use nblocks
        theLOW = min(ALL,na.rm=T)
        theHIGH = max(ALL,na.rm=T)
        breaks_ = seq(theLOW,theHIGH,length.out=NBLOCKS)

    }else{
    	# use none
        theLOW = min(ALL,na.rm=T)
        theHIGH = max(ALL,na.rm=T)
        breaks_ = seq(theLOW,theHIGH,length.out=300)
    }
    
        
    
    if(theLOW<0 & theHIGH>0){
        # special case for difference
        ZEROCOLOR = which(COLORS=='white')
        if( length(ZEROCOLOR)>0 & ZEROCOLOR>1 ){
            if(length(COLORS)>ZEROCOLOR){
                colorFUN = colorRampPalette(COLORS[ZEROCOLOR:length(COLORS)])
            }else{
                colorFUN = colorRampPalette(c(COLORS[ZEROCOLOR],'blue'))
            }#
            colorlist = colorFUN( sum(breaks_>=0) );
            
            INVERTED_COLORS = COLORS[1:ZEROCOLOR]
            colorFUN = colorRampPalette( INVERTED_COLORS)
            inverted_colorlist = colorFUN( sum(breaks_<0) );
            colorlist = c(inverted_colorlist,colorlist)
        }else{
            colorFUN = colorRampPalette( COLORS)
            colorlist = colorFUN( sum(breaks_>=0) );
            
            INVERTED_COLORS = sapply(rev(COLORS),function(x){ do.call(rgb, as.list(abs(col2rgb(x)-255)/255)) })
            colorFUN = colorRampPalette( INVERTED_COLORS)
            inverted_colorlist = colorFUN( sum(breaks_<0) );
            colorlist = c(inverted_colorlist,rgb(1,1,1),colorlist)
        }
    }else{
        colorFUN = colorRampPalette( COLORS)
        colorlist = colorFUN(length(breaks_));
    }#
    
    result = hist(ALL, breaks=breaks_, border=colorlist);
    return <- data.frame(bp=result$breaks, col=sapply(colorlist,function(x){ paste(col2rgb(x),collapse=':')}) );
}#function

colorBarImage = function(cs,digits=1,vertical=F){
    colorscheme_RCOL = sapply(cs$col, function(x){do.call(rgb, as.list(as.numeric(unlist(strsplit(x,split=':')))/255)) })
    xx = seq_along(cs$bp)
    xx_mark = quantile(xx/max(xx),probs=c(0,0.25,0.5,0.75,1))
    if(vertical){
        dev.new(width=4,height=1);
        par(mar=c(1,1,3,1))
        image(as.matrix(xx),col=colorscheme_RCOL,yaxt='n',xaxt='n')
        axis(3, at=xx_mark, labels=round(quantile(cs$bp,probs=c(0,0.25,0.5,0.75,1)),digits),las=2)
    }else{
        dev.new(width=4,height=1);
        par(mar=c(3,1,1,1))
        image(as.matrix(xx),col=colorscheme_RCOL,yaxt='n',xaxt='n')
        axis(1, at=xx_mark, labels=round(quantile(cs$bp,probs=c(0,0.25,0.5,0.75,1)),digits),las=1)
    }
}#function

mapVisual = function(basemap,title=NULL, cs=NULL, xres=1, yres=1){
	if(is.null(cs)){
		dev.new();par(mar=c(1,1,1,1));image(basemap,asp=yres/xres,xaxt='n',yaxt='n',bty='n',main=title); 
	}else{
		#colorBarImage(cs);
		colorscheme_RCOL = sapply(cs$col, function(x){do.call(rgb, as.list(as.numeric(unlist(strsplit(x,split=':')))/255)) })
		newvalue = apply(basemap,2,function(xx){ sapply(xx,function(yy){ ifelse(is.na(yy),NA,which.min(abs(cs$bp-yy))) }) })
		colrange = unique(as.vector(newvalue)); colrange = sort(colrange[!is.na(colrange)]); # = newvalueID
		fin = apply(newvalue,2,function(xx){ match(xx, colrange) })
		
		# newvalue = do.call(cbind, lapply(seq_len(dim(basemap)[2]),function(xx){ 
			# sapply(basemap[,xx],function(yy){ifelse(is.na(yy),NA,which.min(abs(cs$bp-yy))) }) 
		# }))
		
		#print(colrange)
		dev.new();
		par(mar=c(0,0,1,0));
		image(
			fin,
			asp=yres/xres,main=title,
			xaxt='n',yaxt='n',bty='n',
			col=colorscheme_RCOL[colrange]);
	}# end of if
}#function




