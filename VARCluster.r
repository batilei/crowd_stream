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
  vars = list()
  
  #use twitter as the first sub dataset
  for(k in 1:nset){
    
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
        mix.mat = tmp.mix.mat
        names = colnames(mix.mat)
        var.model = var.model.new
        
        cluster.vec = c(cluster.vec, hash)
      }
    }
    
    clusters[[k]] = cluster.vec
    vars[[k]] = var.model
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
      cat("BIC:",BIC(var.model1$varresult[[1]]) , BIC(var.model2$varresult[[1]]),'\n')
      
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


#test causality
#####################################################################
mix.mat = data.matrix(data.frame(mix.mat.list[[1]][2,]))
names = c(paste('X', (2-1) * 3 + 1, sep='')) 
colnames(mix.mat) = names



tmp.mix.mat = cbind(mix.mat, mix.mat.list[[2]][2,] )
names = c(names,paste('X', (2-1)* 3 + 2,sep=''))
colnames(tmp.mix.mat) = names
var.model1 = VAR(tmp.mix.mat,p=6,type="const")


tmp.mix.mat = cbind(tmp.mix.mat, mix.mat.list[[3]][2,] )
names = c(names,paste('X', (2-1)* 3 + 3,sep=''))
colnames(tmp.mix.mat) = names
var.model2 = VAR(tmp.mix.mat,p=6,type="const")

anova.class = anova(var.model1$varresult[[1]],var.model2$varresult[[1]])

anova.class$"Pr(>F)"[2]

  #get the p-value
#granger.test = causality(var.model,cause = paste('X',(2-1) * 3 + 2,sep=""))$Granger
  



