library(vars)

#process the data and divide them into training dataset and validate dataset.

#step1: generate the training dataset, remove the data in the weekends
generate.train.dataset <- function(original.data.mat, beg.time, train.period)
{
  r = nrow(original.data.mat)
  
  train.dataset = matrix(nrow=r,ncol=0)
  
  diff.hour = as.difftime("01:00:00","%H:%M:%S")
  
  for(j in 1:train.period){
    
    time = as.POSIXlt(beg.time + (j-1) * diff.hour)
    
    if(time$wday > 0 && time$wday < 5 )
    {
      train.dataset = cbind(train.dataset, original.data.mat[,j])
    }
  }
  
  return (train.dataset)
}

btime = strptime("2013-09-01 00:00:00","%Y-%m-%d %H:%M:%S",tz="America/New_York")
train.period = 24*57

twit.train.mat = generate.train.dataset(t_mat,btime,train.period)
bicycle.train.mat = generate.train.dataset(bicycle_start,btime,train.period)
taxi.train.mat = generate.train.dataset(taxi_start,btime,train.period)

train.time.list = gen.workday.time.list(btime,train.period)


#step2 generate the validate dataset
generate.validate.dataset <- function(original.data.mat, beg.time, valid.btime.ind, predict.len)
{
  r = nrow(original.data.mat)
  c= ncol(original.data.mat)
  
  validate.dataset = matrix(nrow=r,ncol=0)
  
  diff.hour = as.difftime("01:00:00","%H:%M:%S")
  
  k = 0
  for(j in valid.btime.ind:c){
    
    time = as.POSIXlt(beg.time + (j-1) * diff.hour)
    
    if(time$wday > 0 && time$wday < 5 )
    {
      validate.dataset = cbind(validate.dataset, original.data.mat[,j])
      
      k=k+1
      if(k == predict.len)
        break
    }
  }
  
  return (validate.dataset)
}

valid.period = 24 * 4
twit.valid.mat = generate.validate.dataset(t_mat,btime,train.period+1,valid.period)
bicycle.valid.mat = generate.validate.dataset(bicycle_start,btime,train.period+1,valid.period)
taxi.valid.mat = generate.validate.dataset(taxi_start,btime,train.period+1,valid.period)

valid.btime = (btime + train.period * as.difftime("01:00:00","%H:%M:%S"))
valid.time.list = gen.workday.time.list(valid.btime,valid.period)




#generate the time list for axis
gen.workday.time.list <- function(beg.time,period){
  
  diff.hour = as.difftime("01:00:00","%H:%M:%S")
  time.list = list()
  
  k = 1
  for(i in 1:period){
    time = as.POSIXlt(beg.time + (i-1) * diff.hour)
    
    if(time$wday > 0 && time$wday < 5)
    {
      time.list[[k]] = time
      k = k + 1
    }
  }
  
  return (time.list)
}

#generate the time vector from the time list
gen.workday.time.vec <-function(beg.time, period, interval){
    
  time.vec = c()
  
  k = 0
  for(i in 1:period){
    time = as.POSIXlt(beg.time + (i-1) * diff.hour)
    
    if(time$wday > 0 && time$wday < 5)
    {
      k = k + 1
      if(k %% interval == 1){
        time.vec = append(time.vec,format(time,format="%m-%d-%Y"))
      }
    }
  }

  return (time.vec)
}

#compute the var model for each grid
var.list = list()

ngrids = nrow(bicycle_start)

for(i in 1:ngrids){
  mix.mat = data.matrix(data.frame(twit.train.mat[i,], bicycle.train.mat[i,], taxi.train.mat[i,]))
  var.list[[i]] =  VAR(mix.mat,p=12,type="const")
}

#return the predict result of the next "ahead" hours for grid["grid.ind"], by.unit is prediction time interval unit
#pre.result$fcst[[1]][2] is twitter's number in the second hour
#pre.result$fcst[[2]][1] is bicycle's number in the next 1 hour
#pre.result$fcst[[3]][3] is taxi's number in the third hour
var.predict.grid <- function(grid.ind, ahead, by.unit){
  
  pred.mat = matrix(nrow = 3, ncol = 0)
  
  mix.mat = data.matrix(data.frame(twit.train.mat[grid.ind,], bicycle.train.mat[grid.ind,], taxi.train.mat[grid.ind,]))
  
  
  times = ahead / by.unit
  for(i in 1:times){
    
    var.model = VAR(mix.mat,p=24,type="const")
    
    pre.result = predict(var.model, n.ahead = by.unit)
    
    for(j in 1:by.unit)
    {
      pred.mat = cbind(pred.mat, c(pre.result$fcst[[1]][j],pre.result$fcst[[2]][j],pre.result$fcst[[3]][j]))
      
      mix.mat = rbind(mix.mat, c(twit.valid.mat[grid.ind,(i-1)*by.unit+j],bicycle.valid.mat[grid.ind,(i-1)*by.unit+j],taxi.valid.mat[grid.ind,(i-1)*by.unit+j]))
    }
    
  }
  
  return (pred.mat)
}


plot.predict.result <- function(grid.ind){
    
  #prev.len = 0
  pred.len = 24 * 4
  train.len = ncol(twit.train.mat)
  
  pred.mat.1 = var.predict.grid(grid.ind,pred.len,1)
  pred.mat.1[which(pred.mat.1<0)] = 0
  
  pred.mat.24 = var.predict.grid(grid.ind,pred.len,24)
  pred.mat.24[which(pred.mat.24<0)] = 0
  
  max.value = max(taxi.valid.mat[grid.ind,])
  
  #set up the plot
  plot.new()
  plot.window(xlim=c(1,100), ylim=c(0,1.2*max.value))
  
  #plot twitter train data source
  #lines(1:prev.len, twit.train.mat[grid.ind, (train.len-prev.len+1):train.len],col="blue")
  
  #plot twitter validated data(by 1 hour)
  lines(1:pred.len,twit.valid.mat[grid.ind,],col="blue")
  
  #plot twitter predicted data(by 1 hour)
  lines(1:pred.len,pred.mat.1[1,],lty=5,col="blue")
  
  #plot twitter predicted data(by 24 hour)
  lines(1:pred.len,pred.mat.24[1,],lty=3,col="blue")
  
  #--------------------------------------------------------------------------------------
  
  #plot bicycle train data source
  #lines(1:prev.len, bicycle.train.mat[grid.ind, (train.len-prev.len+1):train.len],col="red")
  
  #plot bicycle validated data(1 by 1 hour)
  lines(1:pred.len, bicycle.valid.mat[grid.ind,],col="red")
  
  #plot bicycle predicted data(1 by 1 hour)
  lines(1:pred.len, pred.mat.1[2,],lty=5,col="red")
  
  #plot bicycle predicted data(by 24 hour)
  lines(1:pred.len, pred.mat.24[2,],lty=3,col="red")
  
  
  #--------------------------------------------------------------------------------------
  
  #plot taxi original data source
  #lines(1:prev.len, taxi.train.mat[grid.ind, (train.len-prev.len+1):train.len],col=33)
  
  #plot taxi validated data(1 by 1 hour)
  lines(1:pred.len,taxi.valid.mat[grid.ind,],col=33)
  
  #plot taxi predicted data(1 by 1 hour)
  lines(1:pred.len,pred.mat.1[3,],lty=5,col=33)
  
  #plot taxi predicted data(by 24 hour)
  lines(1:pred.len,pred.mat.24[3,],lty=3,col=33)
  

  interval = 24
  time.seq = seq(from=1,to=pred.len,by=interval)
  labels.time.vec = gen.workday.time.vec(valid.time.list[[1]], pred.len, interval)
  axis(1,at=time.seq,labels=labels.time.vec)
  axis(2)
  title(main="The Prediction of Different Data Sources",xlab="time",ylab="# of people")
  legend("topright",c("taxi","bicycle","twitter","real data","predict by 1hour","predict by 24hour"),col=c(33,"red","blue",33,33,33), lty=c(1,1,1,1,5,3),cex=0.6,x.intersp=0.3,ncol=2)
  #box()
}



#For test
time = list()
time[[1]] = strptime("2013-09-01 00:00:00","%Y-%m-%d %H:%M:%S",tz="America/New_York")
time[[2]] = time[[1]] + as.difftime("01:00:00","%H:%M:%S")
time[[3]] = NA
time[[4]] = NA
time[[5]] = time[[1]] + as.difftime("01:00:00","%H:%M:%S")

labs = c()
for(i in 1:5)
  labs[i] = format(time[[i]],format="%d-%m-%y %H:%M:%S")

plot.new()
plot.window(xlim=c(1,5), ylim=c(0,10))
lines(1:3,3:5)

axis(1,at=1:5,labels=labs)
axis(2)
box()