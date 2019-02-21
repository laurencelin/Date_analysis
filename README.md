# Date analysis
Given a series of Dates, for example,

w = read.table(...)
w$date = as.Date(paste(w[,'day'], w[,'month'], w[,'year'],sep="-"),format="%d-%m-%Y")
dailyTimeSeries(w$date) # we get the following information

[pic]
