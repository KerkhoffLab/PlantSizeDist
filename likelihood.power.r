#### log-likehood for Truncated Pareto distributions in a closed interval
likelihood.power=function(x,exponent)
{  n = length(x)
   dmin=min(x)
   term1=n*log(exponent-1)+n*(exponent-1)*log(dmin)
   term2=-exponent*sum(log(x))
   obj.fn=term1+term2
   return(round(obj.fn, 2))
}