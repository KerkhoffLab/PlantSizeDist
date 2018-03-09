## Functions for estimating the exponent of Truncated Pareto distributions in a 
##closed interval with lower limit, upper limit (White et al.2008 Ecology 89:905-912)
mletp=function (data, xmin, xmax)
{ 
  data=na.omit(data)  ##data is the dbh vector 
  data=data[data>=xmin&data<=xmax]  ## draw the dbh data between lower limit, upper limit
  le = length(data) ## draw the data size
  a = min(data)     # draw real minimum dbh  
  b = max(data)     # draw real maximum dbh 
  k = mean(log(data)) #mean ln(x)
  y = seq(0, 5, 0.01) ##generate an  alternative exponent vector
  v = vector()
  exponent = vector()
  for (i in 1:500) {##generate  difference vector for alternative exponent vector between observed vaule and estimated value
    exponent[i] = y[i]
    f = b^(-y[i] + 1) * log(b) - a^(-y[i] + 1) * log(a)
    j = b^(-y[i] + 1) - a^(-y[i] + 1)
    h = -1/(-y[i] + 1)
    v[i] = h + f/j - k
  }
  e = y[which.min(abs(v))] ##draw best exponent which generate minimum difference
  se = (le^-0.5) * (((-e)^-2) - (((b/a)^e) * (log(b/a))^2)/((1 -
                                                               (b/a)^e)^2))^-0.5
  ## Solutions for the standard error (SE) of the estimated exponent 
  int1 = e + 1.96 * se #95% Confidence intervals for the estimated exponent 
  int2 = e - 1.96 * se #95% Confidence intervals for the estimated exponent 

  # Find the likelihood for exponent e
  eL<-likelihood.power(data, e)
  out <- list(type="tparetoestimate", exponent=round(e, 3), CI=c(round(int1, 2), round(int2,2)), likelihood=eL, lower=a, upper=b)
  return(out)
  #cat("   Exponent = ", round(e, 2), "\n")
  #cat("   95%CI1   = ", round(int1, 2), "\n")
  #cat("   95%CI2   = ", round(int2, 2), "\n")
  
}