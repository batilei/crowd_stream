#read in the data
twitter = data.matrix(read.csv(file="./data/twitters.csv",header=FALSE,sep="\t"))


taxi_start = data.matrix(read.csv(file="./data/taxi_start.csv",header=FALSE,sep="\t"))
taxi_end = data.matrix(read.csv(file="./data/taxi_end.csv",header=FALSE,sep="\t"))

bicycle_start = data.matrix(read.csv(file="./data/bicycle_start.csv",header=FALSE,sep="\t"))
bicycle_end= data.matrix(read.csv(file="./data/bicycle_end.csv",header=FALSE,sep="\t"))

#generate the missing value for the twitter data set.
t_mat = process_twitter_matrix(twitter)


process_twitter_matrix <- function(t_mat){
    
  t_mat = t( apply(t_mat,1,replaceWithNA) )
  
  t_mat = t( apply(t_mat,1,generate_missing_data) )
    
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
  if(is.na(sample_sd))
  {
    print("deviation is NA")
    sample_sd = 1
  } 
  sample_sd = 0.5 * sample_sd
  if(sample_sd < 1)
    sample_sd = 1
  
  rand.val = rnorm(1,mean = sample_mean, sd = sample_sd)
  return (rand.val)
}



#normalization for tests
ntaxi = taxi_start[8,800:900]
nbike = bicycle_start[8,800:900]
ntwit = t_mat[8,800:900]

ntaxi = (ntaxi-mean(ntaxi))/sd(ntaxi)
nbike = (nbike - mean(nbike)) / sd(nbike)
ntwit = (ntwit - mean(ntwit)) / sd(ntwit)

plot(ntaxi,type='l',ylim = c(-2,4))
lines(nbike,col = 'red')
lines(ntwit,col='blue')