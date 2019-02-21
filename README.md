# Date analysis
Given a series of Dates, for example,

  w = read.table(...)

  w$date = as.Date(paste(w[,'day'], w[,'month'], w[,'year'],sep="-"),format="%d-%m-%Y")

  dailyTimeSeries(w$date) # we get the following information

      year month yy_month day doy wday yy_woy woy7 yy_woy7   wy ydecimal
      
  1   1998    10        1  15 288    5      1   42       1 1998 1998.789
  
  7   1998    10        1  21 294    4      2   43       2 1998 1998.805
  
  16  1998    10        1  30 303    6      3   44       3 1998 1998.830
  
  20  1998    11        2   3 307    3      4   44       3 1998 1998.841

