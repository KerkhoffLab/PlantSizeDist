#plot linear binned log-log histogram of points, for visualization of
#size distribution
logloghistplot<-function(x, bins, type="density", xl="ln(Size)", yl="ln(Density)",logbin=T){
  if(logbin==T) bins<-exp(hist(log(x),breaks=bins,plot=F)$breaks)
    
  xhist<-hist(x,breaks=bins, plot=F) 
        
  switch(type,
          density = plot(log(xhist$density)~log(xhist$mids),xlab=xl,ylab=yl),
          counts = plot(log(xhist$counts)~log(xhist$mids),xlab=xl,ylab=yl)
         )
}
  