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
    
    if(time$wday != 6 && time$wday != 0)
    {
      train.dataset = cbind(train.dataset, original.data.mat[,j])
    }
  }
  
  return (train.dataset)
}

btime = strptime("2013-09-01 00:00:00","%Y-%m-%d %H:%M:%S",tz="America/New_York")
train.period = 24*29 - 1

twit.train.mat = generate.train.dataset(t_mat,btime,train.period)
bicycle.train.mat = generate.train.dataset(bicycle_start_sep,btime,train.period)
taxi.train.mat = generate.train.dataset(taxi_start_sep,btime,train.period)

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
    
    if(time$wday != 6 && time$wday != 0)
    {
      validate.dataset = cbind(validate.dataset, original.data.mat[,j])
      
      k=k+1
      if(k == predict.len)
        break
    }
  }
  
  return (validate.dataset)
}

valid.period = 24
twit.valid.mat = generate.validate.dataset(t_mat,btime,train.period + 1,valid.period)
bicycle.valid.mat = generate.validate.dataset(bicycle_start_sep,btime,train.period+1,valid.period)
taxi.valid.mat = generate.validate.dataset(taxi_start_sep,btime,train.period+1,valid.period)

valid.btime = (btime + (train.period+1) * as.difftime("01:00:00","%H:%M:%S"))
valid.time.list = gen.workday.time.list(valid.btime,valid.period)



#generate the time list for axis
gen.workday.time.list <- function(beg.time,train.period){
  
  diff.hour = as.difftime("01:00:00","%H:%M:%S")
  time.list = list()
  
  k = 1
  for(i in 1:train.period){
    time = as.POSIXlt(beg.time + (i-1) * diff.hour)
    
    if(time$wday > 0 && time$wday < 6)
    {
      time.list[[k]] = time
      print(k)
      k = k + 1
    }
  }
  
  return (time.list)
}






#compute the var model for each grid
var.list = list()

ngrids = nrow(bicycle_start_sep)

for(i in 1:ngrids){
  mix.mat = data.matrix(data.frame(twit.train.mat[i,], bicycle.train.mat[i,], taxi.train.mat[i,]))
  var.list[[i]] =  VAR(mix.mat,p=24,type="const")
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
    
  prev.len = 48
  pred.len = 24
  train.len = ncol(twit.train.mat)
  
  pred.mat = var.predict.grid(grid.ind,pred.len,1)
  pred.mat[which(pred.mat<0)] = 0
  
  #set up the plot
  plot.new()
  plot.window(xlim=c(1,100), ylim=c(0,1000))
  
  #plot twitter original data source
  lines(1:prev.len, twit.train.mat[grid.ind, (train.len-prev.len+1):train.len],col="blue")
  
  #plot twitter validated data(1 by 1 hour)
  lines((prev.len+1):(prev.len+pred.len),twit.valid.mat[grid.ind,],lty=2,col="blue")
  
  #plot twitter predicted data(1 by 1 hour)
  lines((prev.len+1):(prev.len+pred.len),pred.mat[1,],lty=6,col="blue")
  
  
  #plot bicycle original data source
  lines(1:prev.len, bicycle.train.mat[grid.ind, (train.len-prev.len+1):train.len],col="red")
  
  #plot bicycle validated data(1 by 1 hour)
  lines((prev.len+1):(prev.len+pred.len), bicycle.valid.mat[grid.ind,],lty=2,col="red")
  
  #plot bicycle predicted data(1 by 1 hour)
  lines((prev.len+1):(prev.len+pred.len), pred.mat[2,],lty=6,col="red")
  
  #plot taxi original data source
  lines(1:prev.len, taxi.train.mat[grid.ind, (train.len-prev.len+1):train.len],col=33)
  
  #plot taxi validated data(1 by 1 hour)
  lines((prev.len+1):(prev.len+pred.len),taxi.valid.mat[grid.ind,],lty=2,col=33)
  
  #plot taxi predicted data(1 by 1 hour)
  lines((prev.len+1):(prev.len+pred.len),pred.mat[3,],lty=6,col=33)
  
  axis(1)
  axis(2)
  box()
}





#For test
time = list()
time[[1]] = strptime("2013-09-01 00:00:00","%Y-%m-%d %H:%M:%S",tz="America/New_York")
time[[2]] = time[[1]] + as.difftime("01:00:00","%H:%M:%S")
time[[3]] = NA
time[[4]] = NA
time[[5]] = time[[1]] + as.difftime("01:00:00","%H:%M:%S")


plot.new()
plot.window(xlim=c(1,5), ylim=c(0,100))
lines(1:3,3:5)

axis(1,at=1:5,labels=c(time[[1]],time[[2]],time[[3]],time[[4]],time[[5]]))
axis(2)
box()