time_subtract = function(time1=c(0,0,0),time2=c(1,1,1)){
  hour1 = time1[1]
  minute1 = time1[2]
  second1 = time1[3]
  hour2 = time2[1]
  minute2 = time2[2]
  second2 = time2[3] 
  
  totseconds1 = second1 + (60*minute1) + (60*60*hour1)
  totseconds2 = second2 + (60*minute2) + (60*60*hour2)
  
  dif = abs(totseconds1-totseconds2)
  print(paste("Difference in Seconds:",dif))
  return(dif)
  
}

time_subtract(c(0,3,15),c(0,8,25))
time_subtract(c(0,2,39),c(0,8,56))
time_subtract(c(0,18,56),c(0,52,15))
time_subtract(c(13,27,36),c(14,37,50))
time_subtract(c(15,56,35),c(16,08,11))
