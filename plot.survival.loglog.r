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