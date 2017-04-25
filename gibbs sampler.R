rm(list=ls(all=TRUE))
set.seed(6)

setwd('U:\\independent studies\\partition models\\no covariates')
source('gibbs sampler functions.R')
dat1=read.csv('fake data.csv',as.is=T)

max.regions=100
n.regions=50
uni.loc=unique(dat[,c('loc.id','LATNUM','LONGNUM')])
uni.loc=uni.loc[order(uni.loc$loc.id),]
nloc=nrow(uni.loc)

dist.mat=data.matrix(dist(uni.loc[,c('LONGNUM','LATNUM')]))
# x2=(uni.loc$LONGNUM[2]-uni.loc$LONGNUM[1:5])^2
# y2=(uni.loc$LATNUM[2] -uni.loc$LATNUM[1:5])^2
# sqrt(x2+y2)
# dist.mat[2,1:5]

centroid=sort(sample(1:nloc,size=n.regions))
tmp=get.regions(centroid,dist.mat)
region.assign=data.frame(loc.id=1:nloc,region=tmp)
theta=runif(n.regions)
#---------------------------------------
#priors

theta.a=theta.b=phi.a=phi.b=1
#---------------------------------------
ngibbs=10000
vec.regions=matrix(NA,ngibbs,nloc)
vec.logl=matrix(NA,ngibbs,1)
vec.n=matrix(NA,ngibbs,1)
param=list(region.assign=region.assign)
max.logl=-Inf
for (i in 1:ngibbs){
  z=unique(param$region.assign$region)
  n=length(z)
  print(c(i,n))
  
  tmp=update.regions(param)
  param$region.assign=tmp$region
  
  vec.regions[i,]=param$region.assign$region
  vec.logl[i]=tmp$prob
  vec.n[i]=n
  
  if (tmp$prob>max.logl) {
    max.logl=tmp$prob
    fim=param$region.assign
  }
}

