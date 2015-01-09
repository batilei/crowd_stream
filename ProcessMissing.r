#read in the data
twitter_sep = data.matrix(read.csv(file="./data/twitters9.csv",header=FALSE,sep="\t"))
twitter_oct = data.matrix(read.csv(file="./data/twitters10.csv",header=FALSE,sep="\t"))


taxi_start_sep = data.matrix(read.csv(file="./data/taxi_start9.csv",header=FALSE,sep="\t"))
taxi_end_sep = data.matrix(read.csv(file="./data/taxi_end9.csv",header=FALSE,sep="\t"))

bicycle_start_sep = data.matrix(read.csv(file="./data/bicycle_start9.csv",header=FALSE,sep="\t"))
bicycle_end_sep = data.matrix(read.csv(file="./data/bicycle_end9.csv",header=FALSE,sep="\t"))


process_twitter_matrix <- function(t_mat1, t_mat2, maxlength){
  t_mat = cbind(t_mat1,t_mat2)
  row = nrow(t_mat)
  
  t_mat = t( apply(t_mat,1,replaceWithNA) )
  
  t_mat = t( apply(t_mat,1,generate_missing_data) )
  
  t_mat = t_mat[,1:maxlength]
  
  return (t_mat)
}

replaceWithNA <- function(vec)
{
  vec[which(vec== 0)] = NA
  return (vec)
}

generate_missing_data <- function(vec)
{
  btime = strptime("2013-09-01 00:00:00","%Y-%m-%d %H:%M:%S",tz="America/New_York")
  
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
      if(i > 1)
        newvec[i] = 0.2 * newvec[i-1] + 0.8 * newvec[i]
    }
  }
  
  return (newvec)
}


sample_missing <- function(vec, ind, time){
  sample = c();
  n = length(vec)
  diff.hour = as.difftime("01:00:00","%H:%M:%S")
  
  time = as.POSIXlt(time,tz="America/New_York",origin= '1970-01-01 00:00.00 UTC');
  
  
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
      iter.time =as.POSIXlt(time + diff.hour * (iter.ind - ind),tz="America/New_York",origin= '1970-01-01 00:00.00 UTC' )
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
      iter.time =as.POSIXlt(time + diff.hour * (iter.ind - ind),tz="America/New_York",origin= '1970-01-01 00:00.00 UTC' )
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
      iter.time =as.POSIXlt(time + diff.hour * (iter.ind - ind),tz="America/New_York",origin= '1970-01-01 00:00.00 UTC' )
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
      iter.time =as.POSIXlt(time + diff.hour * (iter.ind - ind),tz="America/New_York",origin= '1970-01-01 00:00.00 UTC' )
    }
    
  }
  
  sample_mean = mean(sample)
  sample_sd = sd(sample)
  min = sample_mean - sample_sd
  if(min < 0)
    min = 0
  
  max = sample_mean + sample_sd
  
  rand.val = rnorm(sample_mean, 0.5 * sample_sd) 
  return (rand.val)

}


testfun <- function(vec,val)
{
  for(i in 1:length(vec))
    vec[i] = val;
  
  return (vec)
}