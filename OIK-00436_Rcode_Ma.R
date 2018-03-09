#Supplementary E:  Rcodes in this study

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
    
   	 cat("   Exponent = ", round(e, 2), "\n")
     cat("   95%CI1   = ", round(int1, 2), "\n")
		 cat("   95%CI2   = ", round(int2, 2), "\n")
	
}

#### log-likehood for Truncated Pareto distributions in a closed interval
likelihood.power=function(x,exponent)
{  n = length(x)
  dmin=min(x)
 term1=n*log(exponent-1)+n*(exponent-1)*log(dmin)
 term2=-exponent*sum(log(x))
 obj.fn=term1+term2
  cat("   likelihood = ", round(obj.fn, 2), "\n")
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


### The crappy linear regression way to fit a power law
# The common procedure is to fit to the binned density function, which is even
# crappier than to fit to the complementary distribution function; this
# currently only implements the latter

# First, produce the empirical complementary distribution function, as
# a pair of lists, {x}, {C(x)}
# Then regress log(C) ~ log(x)
# and report the slope and the R^2
# Input: Data vector, threshold
# Output: List with distributional parameters and information about the
#         fit
pareto.fit.regression.cdf <- function(x,threshold=1) {
  x <- x[x>=threshold]
  n <- length(x)
  x <- sort(x)
  uniqs <- unique_values(x)
  distinct_x <- x[uniqs]
  upper_probs <- ((n+1-uniqs))/n
  log_distinct_x <- log(distinct_x)
  log_upper_probs <- log(upper_probs)
  # so if one unique index was n, this would give prob 1/n there, and if one
  # unique index is 1 (which it should always be!), it will give prob 1 there
  loglogfit <- lm(log_upper_probs ~ log_distinct_x)
  intercept <- coef(loglogfit)[1] # primarily useful for plotting purposes
  slope <- -coef(loglogfit)[2] # Remember sign of parameterization
  # But that's the exponent of the CDF, that of the pdf is one larger
  # and is what we're parameterizing by
  slope <- slope+1
  r2 <- summary(loglogfit)$r.squared
  loglike <- pareto.loglike(x, threshold, slope)
  result <- list(type="pareto", exponent = slope, rsquare = r2,
                 log_x = log_distinct_x, log_p = log_upper_probs,
                 intercept = intercept, loglike = loglike, xmin=threshold)
  return(result)
}

# Wrapper function to just get the exponent estimate
loglogslope <- function(x,threshold=1) {
  llf <- pareto.fit.regression.cdf(x,threshold)
  exponent <- llf$exponent
  return(exponent)
}

# Wrapper function to just get the R^2 values
loglogrsq <- function(x,threshold=1) {
  llf <- pareto.fit.regression.cdf(x,threshold)
  r2 <- llf$rsquare
  return(r2)
}

# Function to take a sorted list of values, and return only the indices
# to unique values
# Called in finding the empirical complementary distribution function
# If a value is unique, return its index
# If a value is repeated, return its lowest index --- this is intended
# for working with the empirical complementary distribution function
# Input: a SORTED list of (real) numbers
# Output: a list of the indices of the input which mark distinct values
unique_values <- function(a_sorted_list) {
    # See which members of the list are strictly less than their successor
    n <- length(a_sorted_list)
    is_lesser <- a_sorted_list[2:n] > a_sorted_list[1:(n-1)]
    # convert to index numbers
    most_indices <- 1:(n-1)
    my_indices <- most_indices[is_lesser]
    # Remember that we've checked a list shortened by one from the start
    my_indices <- my_indices+1
    # Remember that the first item in the list has to be included
    my_indices <- c(1,my_indices)
    return(my_indices)
}


 
# Revision history:
# no release	2003		First draft
# v 0.0		2007-06-04	First release
# v 0.0.1	2007-06-29	Fixed "not" for "knot" typo, thanks to
#				Nicholas A. Povak for bug report
# v 0.0.2	2007-07-22	Fixed bugs in plot.survival.loglog, thanks to
#						Stefan Wehrli for the report



# Functions for estimation of a stretched exponential or Weibull distribution

# The use of a Weibull distribution to model values above a specified lower
# threshold can be done in one of two ways.  The first is simply to shift the
# the standard Weibull, i.e, to say that x-threshold ~ weibull.
# The other is to say that Pr(X|X>threshold) = weibull(X|X>threshold), i.e.,
# that the right tail follows the same functional form as the right tail of a
# Weibull, without necessarily having the same probability of being in the tail.
# These will be called the "shift" and "tail" methods respectively.
# The shift method is, computationally, infinitely easier, but not so suitable
# for our purposes.

# The basic R system provides dweibull (density), pweibull (cumulative
# distribution), qweibull (quantiles) and rweibull (random variate generation)

### Function for fitting:
# weibull.fit			Fit Weibull to data with choice of methods
### Distributional functions, per R standards:
# dweibull.tail			Tail-conditional probability density
# pweibull.tail			Tail-conditional cumulative distribution
### Backstage functions, not for users:
# weibull.fit.tail		Fit by maximizing tail-conditional likelihood
#				(default)
# weibull.fit.eqns		Fit by solving ML estimating equations for
#				shifted Weibull
# weibull.est.shape.inefficient Inefficient estimator of shape for shifted
#				Weibull, used to initialize fit.eqns
# weibull.est.scale.from.shape  MLE of scale given shape for shifted Weibull
# weibull.loglike.shift		Log-likelihood of shifted Weibull
# weibull.loglike.tail		Tail-conditional log-likelihood


# Fit Weibull to data
# Wrapper for functions implementing different ML methods
# Input: Data vector, lower threshold, method flag
# Output: List giving distribution type ("weibull"), parameters, log-likelihood
weibull.fit <- function(x, threshold = 0,method="tail") {
  x <- x[x>=threshold]
  switch(method,
    tail = {
      # Estimate parameters by direct maxmization of the tail-conditional
      # log-likelihood
      fit <- weibull.fit.tail(x,threshold)
    },
    eqns = {
      # Estimate parameters by solving the ML estimating equations of a shifted
      # Weibull
      fit <- weibull.fit.eqns(x,threshold)
    },
    {
      cat("Unknown method\n")
      fit <- NA})
  return(fit)
}



# Tail-conditional probability density function
# Returns NA on values < threshold
# Input: Data vector, distributional parameters, log flag
# Output: Vector of (log) probability densities
dweibull.tail <- function(x,shape,scale,threshold=0,log=FALSE) {
  c <- pweibull(threshold,shape=shape,scale=scale,lower.tail=FALSE,log.p=log)
  if (log) {
    f <- function(y) {dweibull(y,shape,scale,log=TRUE) - c}
  } else {
    f <- function(y) {dweibull(y,shape,scale)/c}
  }
  d <- ifelse(x<threshold,NA,f(x))
  return(d)
}

# Tail-conditional cumulative distribution function
# Returns NA on values < threshold
# Input: Data vector, distributional parameters, usual flags
# Output: Vector of (log) cumulative probabilities
pweibull.tail <- function(x,shape,scale,threshold=0,lower.tail=TRUE,
                          log.p=FALSE) {
  c <- pweibull(threshold,shape,scale,lower.tail=FALSE)
  c.log <- pweibull(threshold,shape,scale,lower.tail=FALSE,log.p=TRUE)
  if ((!lower.tail)&&(!log.p)) {
    f <- function(x) {pweibull(x,shape,scale,lower.tail=FALSE)/c}
  }
  if ((!lower.tail)&&(log.p)) {
    f <- function(x) {pweibull(x,shape,scale,lower.tail=FALSE,log.p=TRUE) - c.log}
  }
  if ((lower.tail)&&(!log.p)) {
    f <- function(x) {1 - pweibull(x,shape,scale,lower.tail=FALSE)/c}
  }
  if ((lower.tail)&&(log.p)) {
    f <- function(x) {log(1 - pweibull(x,shape,scale,lower.tail=FALSE)/c)}
  }
  p <- ifelse(x<threshold,NA,f(x))
  return(p)
}


# Fit Weibull to data by maximizing tail-conditional likelihood
  # CONSTRAINTS: The shape and scale parameters must both be positive
  # This will not necessarily give a _stretched_ exponential which would be
  # a shape < 1
# Input: Data vector, lower threshold
# Output: List giving distribution type ("weibull"), parameters, log-likelihood
weibull.fit.tail <- function(x,threshold=0) {
  # Use the whole data to produce initial estimates, via the estimating equation
  initial_fit <- weibull.fit.eqns(x)
  theta_0 <- c(initial_fit$shape, initial_fit$scale)
  # Apply constraints: if we're outside the feasible set, default to a
  # standardized exponential
  if (theta_0[1] < 0) { theta_0[1] = 1 }
  if (theta_0[2] < 0) { theta_0[2] = 1 }
  # Now threshold and directly minimize the negative log likelihood
  x <- x[x>= threshold]
  n <- length(x)
  negloglike <- function(theta) {
    -weibull.loglike.tail(x,threshold,shape=theta[1],scale=theta[2])
  }
  ui <- rbind(c(1,0),c(0,1))
  ci <- c(0,0)
  est <- constrOptim(theta=theta_0, f=negloglike, grad=NULL, ui=ui, ci=ci)
  fit <- list(type="weibull", shape=est$par[1], scale=est$par[2],
              loglike = -est$value, samples.over.threshold=n)
  return(fit)
}

# Fit shifted Weibull to data by solving ML estimating equations
# Input: Data vector, lower threshold
# Output: List giving distribution type ("weibull"), parameters, log-likelihood
weibull.fit.eqns <- function(x, threshold = 0) {
  # ML estimating equations of the shape and scale of the Weibull distribution,
  # taken from Johnson and Kotz, ch. 20
  # This assumes that x-threshold is Weibull-distributed
  x <- x[x>=threshold]
  x <- x-threshold
  x.log <- log(x) # This will be needed repeatedly
  n<-length(x) # So will this
  h <- sum(x.log)/n # And this
  scale_from_shape <- function(shape) {weibull.est.scale.from.shape(x,shape)}
  # Note that the estimation of the shape parameter is only implicit,
  # through a transcendental equation.
  initial_estimates <- weibull.est.shape.inefficient(x)
  shape <- initial_estimates[1]
  scale <- initial_estimates[2]
  map <- function(c) {(sum((x^c) * x.log)/sum(x^c) - h)^(-1)}
  estimating_equation <- function(c) { (c - map(c))^2 }
  shape <- nlm(f=estimating_equation,p=shape)$estimate
  scale <- scale_from_shape(shape)
  loglike <- weibull.loglike.shift(x,threshold,shape,scale)
  fit <- list(type="weibull", shape=shape, scale=scale, loglike=loglike, samples.over.threshold=n)
  return(fit)
}

# Log-likelihood of shifted Weibull distribution
# Input: Data vector, parameters, lower threshold
# Output: real-valued log-likelihood
weibull.loglike.shift <- function(x, shape, scale, threshold = 0) {
  # Assumes x - threshold is Weibull-distributed
  # See Johnson and Kotz for more; they call the lower threshold xi_0
  x <- x[x>=threshold]
  x <- x-threshold
  L <- sum(dweibull(x,shape,scale,log=TRUE))
  return(L)
}

# Tail-conditional log-likelihood
# Input: Data vector, parameters, lower threshold
# Output: Real-valued log-likelihood
weibull.loglike.tail <- function(x, shape, scale, threshold = 0) {
  # We want p(X=x|X>=threshold) = p(X=x)/Pr(X>=threshold)
  x <- x[x>=threshold]
  n <- length(x)
  Like <- weibull.loglike.shift(x,shape,scale)
  ThresholdProb <- pweibull(threshold,shape,scale,log=TRUE,lower.tail = FALSE)
  L <- Like - n*ThresholdProb
  return(L)
}

# Inefficient estimator of the shape parameter of a Weibull, plus scale
# Based on the moments of log of the data
# Input: Data vector, lower threshold
# Output: Real-valued estimates of shape and scale
weibull.est.shape.inefficient <- function(x,threshold=0) {
  # The follow is not an efficient estimator of the shape, but serves to start
  # the approximation process off.  (See Johnson and Katz, ch. 20, section 6.3,
  # "Estimators Based on Distribution of log X".)
  x <- x-threshold
  shape <- (sqrt(6)/pi)*sd(log(x))
  scale <- weibull.est.scale.from.shape(x,shape)
  c(shape,scale)
}

# Maximum likelihood estimate of a Weibull scale parameter, given shape
# Input: Data vector, shape parameter
# Output: Real-valued scale parameter
weibull.est.scale.from.shape <- function(x,shape) {
  # Given a value of the shape parameter, return the corresponding
  # MLE of the scale parameter
  n <- length(x)
  scale <- ((1/n)*sum(x^shape))^(1/shape)
  return(scale)
}
