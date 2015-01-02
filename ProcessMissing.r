#read in the data
twitter_sep = read.csv(file="CodeSpace/Java/CrowdsourcingPrediction/twitters9.csv",header=FALSE,sep="\t")
twitter_oct = read.csv(file="CodeSpace/Java/CrowdsourcingPrediction/twitters10.csv",header=FALSE,sep="\t")


taxi_start_sep = read.csv(file="CodeSpace/Java/CrowdsourcingPrediction/taxi_start9.csv",header=FALSE,sep="\t")
taxi_end_sep = read.csv(file="CodeSpace/Java/CrowdsourcingPrediction/taxi_end9.csv",header=FALSE,sep="\t")

bicycle_start_sep = read.csv(file="CodeSpace/Java/CrowdsourcingPrediction/bicycle_start9.csv",header=FALSE,sep="\t")
bicycle_end_sep = read.csv(file="CodeSpace/Java/CrowdsourcingPrediction/bicycle_end9.csv",header=FALSE,sep="\t")


diff = as.difftime("01:00:00","%H:%M:%S");
times = ctime + c(0:719) * diff


generate_missing_data <- function(vec)
{
  btime = strptime("2013-09-01 00:00:00","%Y-%m-%d %H:%M:%S",tz="EDT")
  
  diff.hour = as.difftime("01:00:00","%H:%M:%S")
  times = as.POSIXlt(btime + c(0:(length(vec)-1)) * diff.hour)
    
  newvec = c()
  
  for( i in 1:length(vec)){
    
    if(is.na(vec[i])==FALSE)
    {
      newvec[i] = vec[i]
    }
    
    else{
      newvec[i] = sample_missing(vec, i, times[i])
    }
  }
  
  return (newvec)
}


sample_missing <- function(vec, ind, time){
  sample = c();
  n = length(vec)
  diff.hour = as.difftime("01:00:00","%H:%M:%S")
  
  time = as.POSIXlt(time,tz="EDT",origin= '1970-01-01 00:00.00 UTC');
  
  
  #5
  if(time$wday == 5){
    #move forward
    iter.ind = ind + 24 * 7
    while(iter.ind  <= n){
      if( is.na(vec[iter.ind ]) == FALSE)
        sample = append(sample, vec[iter.ind ])
      
      iter.ind  = iter.ind  + 24 * 7
    }
    
    #move backward
    iter.ind  = ind - 24 * 7
    while(iter.ind  > 0){
      if( is.na(vec[iter.ind ]) == FALSE)
        sample = append(sample, vec[iter.ind ])
      
      iter.ind  = iter.ind  - 24 * 7
    }
  }
  
  
  #6-0
  else if(time$wday==6 || time$wday==0){
    #move forward
    iter.ind = ind
    iter.time = time
    while(iter.ind <=n){
      if((iter.time$wday == 6 || iter.time$wday == 0) && is.na(vec[iter.ind])==FALSE )
      {
        sample = append(sample,vec[iter.ind])
      }
      
      iter.ind = iter.ind + 24
      iter.time =as.POSIXlt(time + diff.hour * (iter.ind - ind),tz="EDT",origin= '1970-01-01 00:00.00 UTC' )
    }
    
    #move backward
    iter.ind = ind
    iter.time = time
    while(iter.ind > 0){      
      if((iter.time$wday == 6 || iter.time$wday == 0) && is.na(vec[iter.ind])==FALSE )
      {
        sample = append(sample,vec[iter.ind])
      }
      
      iter.ind = iter.ind - 24
      iter.time =as.POSIXlt(time + diff.hour * (iter.ind - ind),tz="EDT",origin= '1970-01-01 00:00.00 UTC' )
    }
    
  }
  
  #1-4  
  else{
    #move forward
    iter.ind = ind
    iter.time = time
    while(iter.ind <=n){
      weekday = iter.time$wday
      if(weekday >0 && weekday< 5)
      {
        if(is.na(vec[iter.ind])==FALSE )
        {
          sample = append(sample,vec[iter.ind])
        }
      }
      
      iter.ind = iter.ind + 24
      iter.time =as.POSIXlt(time + diff.hour * (iter.ind - ind),tz="EDT",origin= '1970-01-01 00:00.00 UTC' )
    }
    
    #move backward
    iter.ind = ind
    iter.time = time
    while(iter.ind > 0){
      weekday = iter.time$wday
      if(weekday >0 && weekday< 5)
      {
        if(is.na(vec[iter.ind])==FALSE )
        {
          sample = append(sample,vec[iter.ind])
        }
      }
      
      iter.ind = iter.ind - 24
      iter.time =as.POSIXlt(time + diff.hour * (iter.ind - ind),tz="EDT",origin= '1970-01-01 00:00.00 UTC' )
    }
    
  }
  
  sample_mean = mean(sample)
  sample_sd = sd(sample)
  min = sample_mean - sample_sd
  if(min < 0)
    min = 0
  
  max = sample_mean + sample_sd
  
  rand.val = runif(1,min,max) 
  return (rand.val)

}


