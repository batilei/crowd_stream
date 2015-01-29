library(abind)

ngrids = 15
ngrid.col = 3
ngrid.row = 5

mix.mat.list = list()
mix.mat.list[[1]] = twit.train.mat
mix.mat.list[[2]] = bicycle.train.mat
mix.mat.list[[3]] = taxi.train.mat

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
  

#Cluster the datasets in or near the grids[grid.ind]
#We hash the cluster result by setting value = (grid.ind-1) * nset + i, so 4 = 1 * 3 + 1 means the no.1 dataset in grid 1.
cluster.datasets <- function(grid.ind){
  
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




#return the predict result of the next "ahead" hours for grid["grid.ind"], by.unit is prediction time interval unit
#pre.result$fcst[[1]][2] is twitter's number in the second hour
#pre.result$fcst[[2]][1] is bicycle's number in the next 1 hour
#pre.result$fcst[[3]][3] is taxi's number in the third hour
var.predict.grid <- function(grid.ind, ahead, by.unit){
  
  pred.vec = c()
  
  #change this line to spatial-temporal related datasets, not the specific datasets below.
  #mix.mat = data.matrix(data.frame(twit.train.mat[grid.ind,], bicycle.train.mat[grid.ind,], taxi.train.mat[grid.ind,]))
  
  
  times = ahead / by.unit
  for(i in 1:times){
    
    var.model = VAR(mix.mat,p=4,type="const")
    
    pre.result = predict(var.model, n.ahead = by.unit)
    
    for(j in 1:by.unit)
    {
      pred.vec = c(pred.vec, c(pre.result$fcst[[1]][j]))
      
      #change this line to spatial-temporal related datasets, not the specific datasets below.
      #mix.mat = rbind(mix.mat, c(twit.valid.mat[grid.ind,(i-1)*by.unit+j],bicycle.valid.mat[grid.ind,(i-1)*by.unit+j],taxi.valid.mat[grid.ind,(i-1)*by.unit+j]))
    }
    
  }
  
  return (pred.vec)
}



#test causality
#####################################################################
mix.mat = data.matrix(data.frame(mix.mat.list[[1]][2,]))
names = c(paste('X', (2-1) * 3 + 1, sep='')) 
colnames(mix.mat) = names



tmp.mix.mat = cbind(mix.mat,mix.mat.list[[2]][2,] )
colnames(tmp.mix.mat) = c(names,paste('X', (2-1)* 3 + 2,sep=''))
  
  
var.model = VAR(tmp.mix.mat,p=6,type="const")
  #get the p-value
granger.test = causality(var.model,cause = paste('X',(2-1) * 3 + 2,sep=""))$Granger
  



