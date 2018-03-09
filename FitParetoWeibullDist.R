#This script performs a size distribution analysis combining power law 
#(Pareto distribution) and stretched exponential (Weibull) as #well as the truncated Pareto
#fits to size distribution data
#It requires a vector of size data (sizes) for a collection of individuals
#and the functions defined by Lai et al. in "ParetoAndWeibullFits.R"

source("ParetoAndWeibullFits.R")  #this file defines a large number of functions used in the fitting

PandW.fits<-function(sizes){
  
  #First some characteristics of the data for 
  #use in othe functions and plotting
  max.s<-max(sizes)
  min.s<-min(sizes)
  length.s<-length(sizes)
  bin.s<-seq(quantile(sizes, 0.15), quantile(sizes,0.85), length.out=50)
  X.s<-sort(unique(sizes))
  
  #first do the full fits to each
  Pfit<-pareto.fit.ml(sizes, min.s)
  tPfit<-pareto.fit.break.ml(sizes, min.s, max.s)
  Wfit<-weibull.fit(sizes, min.s)
  Pfit.d<-dpareto(X.s, min.s, Pfit$exponent)
  tPfit.d<-dpareto(X.s, min.s, tPfit$exponent)
  Wfit.d<-dweibull(X.s, Wfit$shape, Wfit$scale)
  
  #for the combined model, select a set of break points
  #then run pareto and weibull fits on either side of the break
  #compile loglikelihood values in a vector to compare across
  #break points
  PandW.LL<-vector("numeric",50)
  names(PandW.LL)<-bin.s
  for(i in 1:50){
    Pfit.low<-pareto.fit.break.ml(sizes, min.s, bin.s[i])
    Wfit.hi<-weibull.fit(sizes, bin.s[i])
    PandW.LL[i]<-Pfit.low$loglike+Wfit.hi$loglike  
  }
  best.br<-which.max(PandW.LL)

  Pfit.best<-pareto.fit.break.ml(sizes, min.s, best.br)
  Wfit.best<-weibull.fit(sizes, best.br)
  Pfit.low.d<-dpareto(X.s[X.s<best.br], min.s, Pfit.best$exponent)
  Wfit.hi.d<-dweibull(X.s[X.s>=best.br], best.br, Wfit.best$shape, Wfit.best$scale)
  
  
  out<-list(type="mixed.pw.fit", pareto.full=Pfit, pareto.full.fits=cbind(X.s,Pfit.d), tpareto.full=tPfit, tpareto.full.fits=cbind(X.s,tPfit.d), weibull.full=Wfit, weibull.full.fits=cbind(X.s,Wfit.d), pareto.mix=Pfit.best, pareto.mix.fits=cbind(X.s[X.s<best.br],Pfit.low.d), weibull.mix=Wfit.best, weibull.mix.fits=cbind(X.s[X.s>=best.br],Wfit.hi.d), LL.mix=PandW.LL)
  return(out)
}






