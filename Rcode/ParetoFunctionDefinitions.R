#Function Definitions for Plant size distribution analyses
#Dillon et al. 2018

#Code augmented from R code  in Lai et al. 2013. Oikos 122:1636-1642 Appendix, 
#which builds on Clauset, et al. 2009  Siam Review 51:661-703

#Further borrows from White et al. 2008 Ecology 89:905-912

#This script can be Sourced to define all of the necessary functions used in the main
#analysis of size distributions fitting Pareto and distributions

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
  
    ## Find the likelihood for exponent e (modified by Kerkhoff to find loglikelihood then return rather than cat)
    eL<-likelihood.power(data, e)
    out <- list(type="tparetoestimate", exponent=round(e, 3), CI=c(round(int1, 2), round(int2,2)), loglikelihood=eL, lower=a, upper=b)
    return(out)
   	 ##cat("   Exponent = ", round(e, 2), "\n")
     ##cat("   95%CI1   = ", round(int1, 2), "\n")
		 ##cat("   95%CI2   = ", round(int2, 2), "\n")
	
}

#### log-likehood for Pareto distributions in a closed interval
#This is actually just the log-likelihood of the Pareto - not truncated
likelihood.power=function(x,exponent)
{  n = length(x)
  dmin=min(x)
 term1=n*log(exponent-1)+n*(exponent-1)*log(dmin)
 term2=-exponent*sum(log(x))
 obj.fn=term1+term2
 return(round(obj.fn, 2)) ##modified by Kerkhoff to return rather than cat
}

#### log-likehood for Truncated Pareto distributions in a closed interval
likelihood.tp=function(x,exponent)
{  n = length(x)
   a=min(x)
   b=max(x)
   oml<-1-exponent
   term1=n*log(oml)-n*log(b^oml-a^oml)
   term2=-exponent*sum(log(x))
   obj.fn=term1+term2
   return(round(obj.fn, 2)) ##modified by Kerkhoff to return rather than cat
}

## the following R codes copy from   Clauset, et al. 2009  Siam Review 51:661-703
#### Functions for continuous power law or Pareto distributions
# Revision history at end of file
### Standard R-type functions for distributions:
# dpareto		Probability density
# ppareto		Probability distribution (CDF)
# qpareto		Quantile function
# rpareto		Random variable generation
### Functions for fitting:
# pareto.fit			Fit Pareto to data
# pareto.fit.ml			Fit Pareto to data by maximum likelihood
#                               --- not for direct use, call pareto.fit instead
# pareto.loglike		Calculate log-likelihood under Pareto
# pareto.fit.regression.cdf	Fit Pareto data by linear regression on                     
#				log-log CDF (disrecommended)
#                               --- not for direct use, call pareto.fit instead
# loglogslope			Fit Pareto via regression, extract scaling
#				exponent
# loglogrsq			Fit Pareto via regression, extract R^2
### Functions for visualization:
# plot.eucdf.loglog		Log-log plot of the empirical upper cumulative
#				distribution function, AKA survival function
# plot.survival.loglog		Alias for plot.eucdf.loglog
### Back-stage functions, not intended for users:
# unique_values			Find the indices representing unique values
#				in a sorted list (used in regression fit)

# Probability density of Pareto distributions
# Gives NA on values below the threshold
# Input: Data vector, lower threshold, scaling exponent, "log" flag
# Output: Vector of (log) probability densities
dpareto <- function(x, threshold = 1, exponent, log=FALSE) {
  # Avoid doing limited-precision arithmetic followed by logs if we want
  # the log!
  if (!log) {
    prefactor <- (exponent-1)/threshold
    f <- function(x) {prefactor*(x/threshold)^(-exponent)}
  } else {
    prefactor.log <- log(exponent-1) - log(threshold)
    f <- function(x) {prefactor.log -exponent*(log(x) - log(threshold))}
  }
  d <- ifelse(x<threshold,NA,f(x))
  return(d)
}

# Cumulative distribution function of the Pareto distributions
# Gives NA on values < threshold
# Input: Data vector, lower threshold, scaling exponent, usual flags
# Output: Vector of (log) probabilities
ppareto <- function(x, threshold=1, exponent, lower.tail=TRUE, log.p=FALSE) {
  if ((!lower.tail) && (!log.p)) {
    f <- function(x) {(x/threshold)^(1-exponent)}
  }
  if ((lower.tail) && (!log.p)) {
    f <- function(x) { 1 - (x/threshold)^(1-exponent)}
  }
  if ((!lower.tail) && (log.p)) {
    f <- function(x) {(1-exponent)*(log(x) - log(threshold))}
  }
  if ((lower.tail) && (log.p)) {
    f <- function(x) {log(1 - (x/threshold)^(1-exponent))}
  }
  p <- ifelse(x < threshold, NA, f(x))
  return(p)
}

# Quantiles of Pareto distributions
# Input: vector of probabilities, lower threshold, scaling exponent, usual flags
# Output: Vector of quantile values
qpareto <- function(p, threshold=1, exponent, lower.tail=TRUE, log.p=FALSE) {
  # Quantile function for Pareto distribution
  # P(x) = 1 - (x/xmin)^(1-a)
  # 1-p = (x(p)/xmin)^(1-a)
  # (1-p)^(1/(1-a)) = x(p)/xmin
  # xmin*((1-p)^(1/(1-a))) = x(p)
  # Upper quantile:
  # U(x) = (x/xmin)^(1-a)
  # u^(1/(1-a)) = x/xmin
  # xmin * u^(1/(1-a)) = x
  # log(xmin) + (1/(1-a)) log(u) = log(x)
  if (log.p) {
    p <- exp(p)
  }
  if (lower.tail) {
    p <- 1-p
  }
  # This works, via the recycling rule
  # q<-(p^(1/(1-exponent)))*threshold
  q.log <- log(threshold) + (1/(1-exponent))*log(p)
  q <- exp(q.log)
  return(q)
}

# Generate Pareto-distributed random variates
# Input: Integer size, lower threshold, scaling exponent
# Output: Vector of real-valued random variates
rpareto <- function(n, threshold=1, exponent) {
  # Using the transformation method, because we know the quantile function
  # analytically
  # Consider replacing with a non-R implementation of transformation method
  ru <- runif(n)
  r<-qpareto(ru,threshold,exponent)
  return(r)
}

# Estimate scaling exponent of Pareto distribution
# A wrapper for functions implementing actual methods
# Input: data vector, lower threshold, method (likelihood or regression,
#        defaulting to former)
# Output: List indicating type of distribution ("exponent"), parameters,
#         information about fit (depending on method), OR a warning and NA
#         if method is not recognized
pareto.fit <- function(data, threshold, method="ml") {
  switch(method,
    ml = { return(pareto.fit.ml(data,threshold)) },
    regression.cdf = { return(pareto.fit.regression.cdf(data,threshold)) },
    { cat("Unknown method\n"); return(NA)}
  )
}

# Estimate scaling exponent of Pareto distribution by maximum likelihood
# Input: Data vector, lower threshold
# Output: List giving distribution type ("pareto"), parameters, log-likelihood
pareto.fit.ml <- function (data, threshold) {
  data <- data[data>=threshold]
  n <- length(data)
  x <- data/threshold
  alpha <- 1 + n/sum(log(x))
  loglike = pareto.loglike(data,threshold,alpha)
  fit <- list(type="pareto", exponent=alpha, xmin=threshold, loglike = loglike)
  return(fit)
}

# Estimate scaling exponent of Pareto distribution by maximum likelihood
# Input: Data vector, lower threshold
# Output: List giving distribution type ("pareto"), parameters, log-likelihood
pareto.fit.break.ml <- function (data, threshold,threshold2) {
  data <- data[data>=threshold&data<threshold2]
  n <- length(data)
  a=min(data)
  b=max(data)
  k=mean(log(data))
  y=seq(-7.5,-1.0,0.01)
  v=vector()
  ep=vector()
  for(i in 1:650)
{
ep[i]=y[i]
f=b^(y[i]+1)*log(b)-a^(y[i]+1)*log(a)
j=b^(y[i]+1)-a^(y[i]+1)
h=-1/(y[i]+1)
v[i]=h+f/j-k
}
alpha=-y[which.min(abs(v))]
loglike = pareto.loglike(data,threshold,alpha)
fit <- list(type="tpareto", exponent=alpha, xmin=threshold,xmax=threshold2, loglike = loglike)
return(fit)
}


# Calculate log-likelihood under a Pareto distribution
# Input: Data vector, lower threshold, scaling exponent
# Output: Real-valued log-likelihood
pareto.loglike <- function(x, threshold, exponent) {
  L <- sum(dpareto(x, threshold = threshold, exponent = exponent, log = TRUE))
  return(L)
}

# Log-log plot of the survival function (empirical upper CDF) of a data set
# Input: Data vector, lower limit, upper limit, graphics parameters
# Output: None (returns NULL invisibly)
plot.survival.loglog <- function(x,from=min(x),to=max(x),...) {
	plot.eucdf.loglog(x,from,to,...)
}
plot.eucdf.loglog <- function(x,from=min(x),to=max(x),...) {
	# Exploit built-in R function to get ordinary (lower) ECDF, Pr(X<=x)
	x.ecdf <- ecdf(x)
	# Now we want Pr(X>=x) = (1-Pr(X<=x)) + Pr(X==x)
        # If x is one of the "knots" of the step function, i.e., a point with
	# positive probability mass, should add that in to get Pr(X>=x)
	# rather than Pr(X>x)
	away.from.knot <- function(y) { 1 - x.ecdf(y) }
	at.knot.prob.jump <- function(y) {
		x.knots = knots(x.ecdf)
		# Either get the knot number, or give zero if this was called
		# away from a knot
		k <- match(y,x.knots,nomatch=0)
		if ((k==0) || (k==1)) { # Handle special cases
			if (k==0) {
				prob.jump = 0 # Not really a knot
			} else {
				prob.jump = x.ecdf(y) # Special handling of first knot
			}
		} else {
			prob.jump = x.ecdf(y) - x.ecdf(x.knots[(k-1)]) # General case
		}
		return(prob.jump)
	}
	# Use one function or the other
	x.eucdf <- function(y) {
		baseline = away.from.knot(y)
		jumps = sapply(y,at.knot.prob.jump)
		ifelse (y %in% knots(x.ecdf), baseline+jumps, baseline)
	}
	plot(x.eucdf,from=from,to=to,log="xy",...)
	invisible(NULL)
}

# Clauset Revision history:
# no release	2003		First draft
# v 0.0		2007-06-04	First release
# v 0.0.1	2007-06-29	Fixed "not" for "knot" typo, thanks to
#				Nicholas A. Povak for bug report
# v 0.0.2	2007-07-22	Fixed bugs in plot.survival.loglog, thanks to
#						Stefan Wehrli for the report