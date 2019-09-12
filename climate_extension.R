source('https://raw.githubusercontent.com/laurencelin/Date_analysis/master/LIB_dailytimeseries3.R')


rain = read.table('stan.rain',skip=1)
rain.date = seq(as.Date('1992-9-2'),length.out=dim(rain)[1],by=1) # "1992-09-02" "2017-12-31"
tmin = read.table('stan.tmin',skip=1)# "1992-09-02" "2017-12-31"
tmax = read.table('stan.tmax',skip=1)# "1992-09-02" "2017-12-31"
tavg = read.table('stan.tavg',skip=1)# "1992-09-02" "2017-12-31"

extend_index = extendingTimeSeries(rain.date)
header = gsub(' 0',' ',format(extend_index$extended_Dates[1],'%Y %m %d 1'))

write(header, 'stan_ext.rain')
write.table(rain[extend_index$extended_Index,],'stan_ext.rain',row.names=F,col.names=F, append=T)

write(header, 'stan_ext.tmin')
write.table(tmin[extend_index$extended_Index,],'stan_ext.tmin',row.names=F,col.names=F, append=T)

write(header, 'stan_ext.tmax')
write.table(tmax[extend_index$extended_Index,],'stan_ext.tmax',row.names=F,col.names=F, append=T)

write(header, 'stan_ext.tavg')
write.table(tavg[extend_index$extended_Index,],'stan_ext.tavg',row.names=F,col.names=F, append=T)





