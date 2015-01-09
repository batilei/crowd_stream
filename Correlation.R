library(Hmisc)
library(car)
library(urca)
library(stats)

#the spatial correlation
twit_aggr = apply(t_mat,1,sum)

bike_s_aggr = apply(bicycle_start,1,sum)
bike_e_aggr = apply(bicycle_end,1,sum)

taxi_s_aggr = apply(taxi_start,1,sum)
taxi_e_aggr = apply(taxi_end,1,sum)


mixed_df = data.frame(twit_aggr,bike_s_aggr,bike_e_aggr,taxi_s_aggr,taxi_e_aggr)
corr_mat = data.matrix(mixed_df)

rcorr(corr_mat)

scatterplotMatrix(~twit_aggr+bike_s_aggr+bike_e_aggr+taxi_s_aggr+taxi_e_aggr,data=mixed_df, 
      main="Scatterplot Matrix of Different Data Sources",smoother=FALSE)


#the temporal correlation
ts.tw = c()
bs.tw = c()
ts.bs = c()

ngrids = nrow(t_mat)
for( i in 1:ngrids){
  ts.tw[i] = cor(taxi_start[i,], t_mat[i,])
  bs.tw[i] = cor(bicycle_start[i,],t_mat[i,])
  ts.bs[i] = cor(taxi_start[i,],bicycle_start[i,])
}

temp.corr.mat = data.matrix(data.frame(ts.tw,bs.tw,ts.bs))

barplot(t(temp.corr.mat),
        beside=TRUE,
        main="Temporal Correlation between Different Data Sources In Each Grid",
        cex.main = 0.9,
        ylab = "the correlation value",
        xlab = "the grids(from 1...n)",
        ylim = c(-0.1,1),
        legend=c("taxi vs. twitter","bicycle vs. twitter","taxi vs. bicycle"),
        args.legend = list("topright", cex=0.8,bty = "n",xjust=0.6,yjust=0.65,x.intersp=0.3))




#the temporal cointegration

plot.cointegration()

plot.cointegration <-function(){
  
  ts.tw = compute.cointegration(taxi_start, t_mat)
  ts.tw.vec = urdf2vec(ts.tw)
  
  bs.tw = compute.cointegration(bicycle_start, t_mat)
  bs.tw.vec = urdf2vec(bs.tw)
  
  ts.bs = compute.cointegration(taxi_start, bicycle_start)
  ts.bs.vec = urdf2vec(ts.bs)
    
  cointe.mat = data.matrix(data.frame(ts.tw.vec,bs.tw.vec,ts.bs.vec))
  
  barplot(t(cointe.mat),
          beside=TRUE,
          main="The Cointegration Tests In Each Grid",
          ylim = c(0,-28),
          ylab = "value of the cointegration test statistic",
          xlab = "all the grids(from 1...n)",
          legend=c("taxi vs. twitter","bicycle vs. twitter","taxi vs. bicycle"),
          args.legend = list("topright", cex=0.8,bty = "n",xjust=0.65,yjust=0.6,x.intersp=0.3))
  
}

urdf2vec <- function(urdf){
  n = length(urdf)
  vec = c()
  for(i in 1:n){
    vec[i] = urdf[[i]]@teststat[1,1]
  }
  
  return (vec)
}

compute.cointegration <- function(dep.mat, indep.mat)
{
  #dep.mat = bicycle_end_sep
  #indep.mat = t_mat
  
  ngrids = nrow(dep.mat)
  uroot.test.value = list()

  for(i in 1:ngrids)
  {
   reg.line = lm(dep.mat[i,] ~ indep.mat[i,])
  
    residuals = reg.line$residuals
  
    uroot.test.value = append(uroot.test.value, ur.df(residuals,type="none"))
  }
  
  return (uroot.test.value)
}


#jonhan test
j.mat = data.matrix(data.frame(t_mat[1,],bicycle_start_sep[1,],taxi_start_sep[1,]))
summary(ca.jo(j.mat))