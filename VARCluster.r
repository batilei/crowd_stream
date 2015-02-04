library(abind)
library(vars)


ngrids = 15
ngrid.col = 3
ngrid.row = 5

#for training
mix.mat.list = list()
mix.mat.list[[1]] = twit.train.mat
mix.mat.list[[2]] = bicycle.train.mat
mix.mat.list[[3]] = taxi.train.mat


#for validation
validate.mat.list = list()
validate.mat.list[[1]] = twit.valid.mat
validate.mat.list[[2]] = bicycle.valid.mat
validate.mat.list[[3]] = taxi.valid.mat

nset = 3

#generate the hash value of sub datasets needed to be visited,including dataset k in grid[grid.ind] itself
neighbor <- function(grid.ind, k){
  hash.dataset = c()
  
  row = floor((grid.ind-1) / ngrid.col) + 1
  col = grid.ind %% ngrid.col
  if(col == 0)
    col = ngrid.col
  
  r.offset = c(0,0,0,1,-1)
  c.offset = c(0,1,-1,0,0)
  
  for(i in 1:5){
  
    r = row + r.offset[i]
    c = col + c.offset[i]
    
    if(r<=ngrid.row && r>=1 && c>=1 && c<=ngrid.col){
    
    n.grid.ind = (r-1) * ngrid.col + c
    hash.beg =  (n.grid.ind - 1) * nset + 1
    hash.end = hash.beg + nset - 1
    

    hash.dataset = c(hash.dataset,hash.beg:hash.end)
    
    }
  }
  
  self.hash = (grid.ind-1) * nset + k
  hash.dataset = hash.dataset[ -which(hash.dataset == self.hash)]
    
  return (hash.dataset)
  
}
  

#Cluster the datasets in or near the grids[grid.ind], here we use the granger test
#We hash the cluster result by setting value = (grid.ind-1) * nset + i, so 4 = 1 * 3 + 1 means the no.1 dataset in grid 1.
cluster.datasets.granger <- function(grid.ind){
  
  ncluster = 1
  clusters = list()
  visit.sets = c()  
  
  #use twitter as the first sub dataset
  for(k in 1:nset){
    
  #check whether this dataset has been visited
  self.hash = k + (grid.ind-1) * nset
    
  if(is.element(self.hash, visit.sets))
    next
  
  #cat("self.hash:",self.hash,"\n")
  
  neighbor.hashs = neighbor(grid.ind, k)
    
  cluster.vec = c(self.hash)#the vector store all the sub dataset indices  

  mix.mat = data.matrix(data.frame(mix.mat.list[[k]][grid.ind,]))
  names = c(paste('X',(grid.ind-1) * nset + k,sep='')) 
  colnames(mix.mat) = names
      
  #test all the other sub datasets
  for(hash in neighbor.hashs){

    neigh.grid.ind = floor((hash - 1) / nset) + 1
    j = hash %% nset #which kind of dataset
    if(j==0)
      j=nset
    
    tmp.mix.mat = cbind(mix.mat, mix.mat.list[[j]][neigh.grid.ind,] )
    colnames(tmp.mix.mat) = c(names,paste('X',(neigh.grid.ind-1) * nset + j,sep=''))
    
    #cat("hash value: ",hash,"\n")
    
    var.model = VAR(tmp.mix.mat,p=4,type="const")
    #get the p-value
    granger.test = causality(var.model,cause = paste('X',(neigh.grid.ind-1) * nset + j,sep=''))$Granger
    
    #if pass the test, update the cluster status
    if(granger.test$p.value < 0.05){#reject the null hypothesis that there is no causality
       mix.mat = tmp.mix.mat
       names = colnames(mix.mat)
       
       cluster.vec = c(cluster.vec, hash)
    }
  }
  
  clusters[[ncluster]] = cluster.vec
  ncluster = ncluster+1
  visit.sets = c(visit.sets,cluster.vec)
  }
  
  return (clusters)
}


#Cluster the datasets in or near the grids[grid.ind], here we use the partial F-test
#We hash the cluster result by setting value = (grid.ind-1) * nset + i, so 4 = 1 * 3 + 1 means the no.1 dataset in grid 1.
cluster.datasets.partialF <- function(grid.ind){
  
  clusters = list()
  #vars = list()
  
  #use twitter as the first sub dataset
  for(k in 1:nset){
    
    ncluster = 1
    bics = c()
    #check whether this dataset has been visited
    self.hash = k + (grid.ind-1) * nset
        
    neighbor.hashs = neighbor(grid.ind, k)
    
    cluster.vec = c(self.hash)#the vector store all the sub dataset indices  
    
    mix.mat = cbind(mix.mat.list[[k]][grid.ind,])
    names = c(paste('X',(grid.ind-1) * nset + k,sep='')) 
    colnames(mix.mat) = names
    
    var.model = mix.mat[,1]
    
    #test all the other sub datasets
    for(hash in neighbor.hashs){
      
      neigh.grid.ind = floor((hash - 1) / nset) + 1
      j = hash %% nset #which kind of dataset
      if(j==0)
        j=nset
      
      #test if they are pearson correlated, if no, skip this dataset.
      tmp.mix.mat = cbind(mix.mat, mix.mat.list[[j]][neigh.grid.ind,] )
      colnames(tmp.mix.mat) = c(names,paste('X',(neigh.grid.ind-1) * nset + j,sep=''))
            
      var.model.new = VAR(tmp.mix.mat,p=4,type="const")
      
      #if pass the partial F test, update the cluster status
      flag = pass.partial.Ftest(var.model, var.model.new, ncluster)
      if(flag){
        ncluster = ncluster + 1
        
        mix.mat = tmp.mix.mat
        names = colnames(mix.mat)
        var.model = var.model.new
        bics = c(bics, BIC(var.model$varresult[[1]]))
        #cat("BIC",bics[length(bics)],'\n')
        cluster.vec = c(cluster.vec, hash)
      }
    }
    
    min_k = which(bics == min(bics))
    
    cluster.vec = cluster.vec[1:(min_k+1)]
    
    clusters[[k]] = cluster.vec
    #vars[[k]] = var.model
    #aic.model = stepAIC(var.model$varresult[[1]])
  }
  
  return (clusters)
}


#var.model1 is reduced model
#var.model2 is full model
pass.partial.Ftest <- function(var.model1,var.model2, ncluster)
{
  flag = NA
  
  if(class(var.model1) == 'varest'){
    
    flag = FALSE    
    anova.class = anova(var.model1$varresult[[1]],var.model2$varresult[[1]])
    if(anova.class$"Pr(>F)"[2] < 0.05)      
    {
      flag = TRUE
      #cat("BIC:",BIC(var.model1$varresult[[1]]) , BIC(var.model2$varresult[[1]]),'\n')
      
    }
  }

  else{
    p = 4
    n = length(var.model1)
    y = var.model1[(p+1):n]
    x1 = var.model1[p:(n-1)]
    x2 = var.model1[(p-1):(n-2)]
    x3 = var.model1[(p-2):(n-3)]
    x4 = var.model1[(p-3):(n-4)]
    
    lm.class = lm(y~x1+x2+x3+x4)
    
    anova.class = anova(lm.class,var.model2$varresult[[1]])
    
    if(anova.class$"Pr(>F)"[2] < 0.05)
      flag = TRUE
    else
      flag = FALSE
  }
  
  return (flag)
}



#return the predict result of the next "ahead" hours for grid["grid.ind"], by.unit is prediction time interval unit
#pre.result$fcst[[1]][2] is twitter's number in the second hour
#pre.result$fcst[[2]][1] is bicycle's number in the next 1 hour
#pre.result$fcst[[3]][3] is taxi's number in the third hour
var.predict.grid.dynamic <- function(grid.ind, ahead, by.unit){
  
  pred.mat = matrix()#store the prediction value of each cluster/type
  clusters = cluster.datasets.partialF(grid.ind)
    
  for(c in 1:nset){#go over each cluster
    
    #go over the sub dataset in each cluster
    cluster = clusters[[c]]
    
    m = length(cluster)
    
    tmp.train.mat = cbind(get.train.data.by.hash(cluster[1]))
    names = c(paste('X',cluster[1],sep=''))
    
    if(2<=m){
      for(k in 2:m)
      {
        tmp.train.mat = cbind(tmp.train.mat, get.train.data.by.hash(cluster[k]))
        names = c(names,paste('X',cluster[k],sep=''))
      }
    }
    
    colnames(tmp.train.mat) = names
    
    times = ahead / by.unit
    pred.vec = c()
    
    for(i in 1:times){
      
      var.model = VAR(tmp.train.mat,p=4,type="const")
      
      pre.result = predict(var.model, n.ahead = by.unit)
      
      for(j in 1:by.unit)
      {
        pred.vec = c(pred.vec, c(pre.result$fcst[[1]][j]))
        
        tmp.train.mat = rbind(tmp.train.mat, get.validate.values.at.time(cluster, (i-1)*by.unit+j))
      }
      
    }
    
    pred.vec[ which(pred.vec < 0) ] = 0
    
    if(c > 1)
    {
      pred.mat = cbind(pred.mat,pred.vec)
    }
    else
    {
      pred.mat = cbind(pred.vec)
    }
  }
  
  return (pred.mat)
}

#given a set of dataset hashs, return the value of these datasets at specific time.
get.validate.values.at.time <- function(clusters,time){
  
  values = c()
  
  for( h in clusters)
  {
    sub.dataset = get.validate.data.by.hash(h)
    values = c(values, sub.dataset[time])
  }
  
  return (values)
}

#Given  the hash valuie cluster.hash, return the spesific dataset at the specific grid
get.validate.data.by.hash <- function(cluster.hash){
  
  type.id = cluster.hash %% nset
  if(type.id == 0)
    type.id = nset
  
  grid.ind = floor((cluster.hash - 1) / nset) + 1
  
  return (validate.mat.list[[type.id]][grid.ind,])
}



#Given  the hash valuie cluster.hash, return the spesific dataset at the specific grid
get.train.data.by.hash <- function(cluster.hash){
  
  type.id = cluster.hash %% nset
  if(type.id == 0)
    type.id = nset
  
  grid.ind = floor((cluster.hash - 1) / nset) + 1
  
  return (mix.mat.list[[type.id]][grid.ind,])
}


#return the prediction accuracy of two methods for different datasets.
validate.predict.accuracy <- function(grid.ind, ahead, by.unit){
  pred.mat = var.predict.grid.dynamic(grid.ind, ahead, by.unit)
  
  accuracy = c()
  ##############################################################################
  #twit
  twit.validate = validate.mat.list[[1]][grid.ind,1:ahead]
  accuracy[1] = sqrt(sum((pred.mat[,1]-twit.validate)^2) / sum(twit.validate^2))
  
  #bike
  bike.validate = validate.mat.list[[2]][grid.ind,1:ahead]
  accuracy[2] = sqrt(sum((pred.mat[,2]-bike.validate)^2) / sum(bike.validate^2))
  
  
  #taxi
  taxi.validate = validate.mat.list[[3]][grid.ind,1:ahead]
  accuracy[3] = sqrt(sum((pred.mat[,3]-taxi.validate)^2) / sum(taxi.validate^2))
  
  
  ##############################################################################
  #twit
  twit.single =predict.single.value(mix.mat.list[[1]][grid.ind,],validate.mat.list[[1]][grid.ind,],ahead,4)
  accuracy[4] = sqrt(sum((twit.single-twit.validate)^2) / sum(twit.validate^2))
  
  #bike
  bike.single = predict.single.value(mix.mat.list[[2]][grid.ind,],validate.mat.list[[2]][grid.ind,],ahead,4)
  accuracy[5] = sqrt(sum((bike.single-bike.validate)^2) / sum(bike.validate^2))
  
  
  #taxi
  taxi.single = predict.single.value(mix.mat.list[[3]][grid.ind,],validate.mat.list[[3]][grid.ind,],ahead,4)
  accuracy[6] = sqrt(sum((taxi.single-taxi.validate)^2) / sum(taxi.validate^2))
  
  
  
  return (accuracy)
}


#predict by single value
predict.single.value <- function(train.vec, validate.vec, pred.len, lag)
{
  ntrain = length(train.vec)
  train.data = data.frame('x1'=train.vec[(ntrain-1):(lag+1-1)])
  if(lag > 1)
  {
    for(i in 2:lag)
      train.data[[paste('x',i,sep='')]] = train.vec[(ntrain-i):(lag+1-i)]
  }
  y = train.vec[ntrain:(lag+1)]
  lm.model = lm(y~.,data = train.data)
  
  input.vec = c()
  for(i in 1:lag)
    input.vec[i] = train.vec[ntrain-i+1]
  
  pred.vec = c()
  
  for(i in 1:pred.len){
    val = lm.model$coefficients[1]
    
    for(j in 1:lag)
      val = val + lm.model$coefficients[j+1] * input.vec[j]
    
    pred.vec[i] = val
    
    input.vec = input.vec[-lag]
    input.vec = c(validate.vec[i], input.vec)
  }
  
  return (pred.vec)
}

# compute all the predictio accuracy in advance
get.all.prediction <- function(ahead)
{
  preds.mat = matrix(nrow=6,ncol=ngrids)
  
  for(i in 1:ngrids)
  {
    pred.vec = validate.predict.accuracy(i, ahead, 1)
        
    preds.mat[,i]= 1 - pred.vec
    
  }
  
  return (preds.mat)
}

preds.mat = get.all.prediction(48)

#draw bar
draw.accuracy.bar <- function(type.id)
{
  names = c()
  preds.mat.type = matrix(ncol = ngrids, nrow = 2)
  for(i in 1:ngrids)
  {
    preds.mat.type[,i] = c(preds.mat[type.id,i],preds.mat[type.id+3,i])
    names = c(names,paste("grid",i,sep=''))
  }
  
  colnames(preds.mat.type) = names
  
  ###############################plot###########################################
  titles = c("Prediction Accuracy - Twits","Prediction Accuracy - Bike","Prediction Accuracy - Taxi")
  barplot(preds.mat.type, main=titles[type.id], ylab= "accuracy", xlab="different grids", ylim=c(0,1.05),
          beside=TRUE, col=rainbow(2))
  
  # Place the legend at the top-left corner with no frame  
  # using rainbow colors
  legend(x=1,y=1.1, c("Multiple_Source","Single_Source"), cex=0.6, 
         bty="n", fill=rainbow(2));
  
  #return (preds.mat)
}


#draw polyline
plot.predict.result <- function(grid.ind, ahead){
  
  #prev.len = 0
  pred.len = ahead
  
  #var prediction
  pred.mat = var.predict.grid.dynamic(grid.ind, ahead, 1)
  
  max.value = max(validate.mat.list[[3]][grid.ind,])
  
  #set up the plot
  plot.new()  
  plot.window(xlim=c(1,50), ylim=c(0,1.01*max.value))
  
  interval = 24
  time.seq = seq(from=1,to=pred.len,by=interval)
  
  btime = strptime("2013-09-01 00:00:00","%Y-%m-%d %H:%M:%S",tz="America/New_York")
  train.period = 24*57
  valid.btime = (btime + train.period * as.difftime("01:00:00","%H:%M:%S"))
  
  valid.time.list = gen.workday.time.list(valid.btime,pred.len)
  labels.time.vec = gen.workday.time.vec(valid.time.list[[1]], pred.len, interval)
  axis(1,at=time.seq,labels=labels.time.vec)
  axis(2)
  title(main="Prediction Results For Different Data Sources",xlab="time",ylab="# of people")
  legend("topleft",c("taxi","bicycle","twitter","multiple_source","single_source"),col=c(33,"red","blue",33,33), lty=c(1,1,1,3,5),cex=0.7,x.intersp=0.5,y.intersp=0.7, ncol=2)
  
  #--------------------------------------------------------------------------------------
  
  #plot the real value
  lines(1:pred.len,validate.mat.list[[1]][grid.ind,1:pred.len],col="blue")
  
  #plot twitter validated data(by 1 hour)
  lines(1:pred.len,pred.mat[1:pred.len,1],lty=3,col="blue")
  
  #plot twitter predicted data(by 1 hour)
  twit.single = predict.single.value(mix.mat.list[[1]][grid.ind,], validate.mat.list[[1]][grid.ind,], pred.len, 4)
  lines(1:pred.len,twit.single,lty=5,col="blue")
  
  #--------------------------------------------------------------------------------------
  
  #plot the real value
  lines(1:pred.len,validate.mat.list[[2]][grid.ind,1:pred.len],col="red")
  
  #plot bicycle validated data(1 by 1 hour)
  lines(1:pred.len, pred.mat[1:pred.len,2],lty=3,col="red")
  
  #plot bicycle predicted data(1 by 1 hour)
  bike.single = predict.single.value(mix.mat.list[[2]][grid.ind,], validate.mat.list[[2]][grid.ind,], pred.len, 4)
  lines(1:pred.len, bike.single,lty=5,col="red")
  
  #--------------------------------------------------------------------------------------
  
  #plot the real value
  lines(1:pred.len,validate.mat.list[[3]][grid.ind,1:pred.len])
  
  #plot taxi validated data(1 by 1 hour)
  lines(1:pred.len,pred.mat[1:pred.len,3],lty=3,col=33)
  
  #plot taxi predicted data(1 by 1 hour)
  taxi.single = predict.single.value(mix.mat.list[[3]][grid.ind,], validate.mat.list[[3]][grid.ind,], pred.len, 4)
  lines(1:pred.len,taxi.single,lty=5,col=33)
  
}

  



