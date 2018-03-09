
#requires ParetoAndWeibullFits.R and FitParetoWeibullDist.R
#first set some plot parameters
par(cex=1.25, cex.axis=1.1, cex.lab=1.5, lwd=2.5)
#for Wetland plot 1
sizes<-Wetland1$Weight #assigns weights to variable "sizes"
logloghistplot(sizes,bins=50,xl="ln(Mass (g))")
legend(x="topright", legend=c("BFEC Wetland 1","(315 m)"), bty="n", cex=1.1)

Wetland1.PWfit<-PandW.fits(Wetland1$Weight)
Wetland1.X.s<-Wetland1.PWfit$pareto.full.fits[,1]
Wetland1.Pfit.d<-Wetland1.PWfit$pareto.full.fits[,2]
Wetland1.Wfit.d<-Wetland1.PWfit$weibull.full.fits[,2]
lines(log(Wetland1.Pfit.d)~log(Wetland1.X.s), col="red")
lines(log(Wetland1.Wfit.d)~log(Wetland1.X.s), col="blue")
Wetland1.pareto.AIC<-(-2)*Wetland1.PWfit$pareto.full$loglike+2
Wetland1.weibull.AIC<-(-2)*Wetland1.PWfit$weibull.full$loglike+4
Wetland1.exponent<-Wetland1.PWfit$pareto.full$exponent
Wetland1.shape<-Wetland1.PWfit$weibull.full$shape
Wetland1.scale<-Wetland1.PWfit$weibull.full$scale


#for Wetland plot 4
sizes<-Wetland4$Weight #assigns weights to variable "sizes"
logloghistplot(sizes,bins=50,xl="ln(Mass (g))")
legend(x="topright", legend=c("BFEC Wetland 2","(315 m)"), bty="n",cex=1.25)

Wetland4.PWfit<-PandW.fits(Wetland4$Weight)
Wetland4.X.s<-Wetland4.PWfit$pareto.full.fits[,1]
Wetland4.Pfit.d<-Wetland4.PWfit$pareto.full.fits[,2]
Wetland4.Wfit.d<-Wetland4.PWfit$weibull.full.fits[,2]
lines(log(Wetland4.Pfit.d)~log(Wetland4.X.s), col="red")
lines(log(Wetland4.Wfit.d)~log(Wetland4.X.s), col="blue")
Wetland4.pareto.AIC<-(-2)*Wetland4.PWfit$pareto.full$loglike+2
Wetland4.weibull.AIC<-(-2)*Wetland4.PWfit$weibull.full$loglike+4
Wetland4.exponent<-Wetland4.PWfit$pareto.full$exponent
Wetland4.shape<-Wetland4.PWfit$weibull.full$shape
Wetland4.scale<-Wetland4.PWfit$weibull.full$scale

#for Prairie Plot 1
sizes<-PrairiePlot1.Streiter$mass.g #assigns weights to variable "sizes"
logloghistplot(sizes,bins=50,xl="ln(Mass (g))")
legend(x="topright", legend=c("BFEC Prairie 1","(318 m)"), bty="n", cex=1.25)


PrairiePlot1.Streiter.PWfit<-PandW.fits(PrairiePlot1.Streiter$mass.g)
PrairiePlot1.Streiter.X.s<-PrairiePlot1.Streiter.PWfit$pareto.full.fits[,1]
PrairiePlot1.Streiter.Pfit.d<-PrairiePlot1.Streiter.PWfit$pareto.full.fits[,2]
PrairiePlot1.Streiter.Wfit.d<-PrairiePlot1.Streiter.PWfit$weibull.full.fits[,2]
lines(log(PrairiePlot1.Streiter.Pfit.d)~log(PrairiePlot1.Streiter.X.s), col="red")
lines(log(PrairiePlot1.Streiter.Wfit.d)~log(PrairiePlot1.Streiter.X.s), col="blue")
PrairiePlot1.Streiter.pareto.AIC<-(-2)*PrairiePlot1.Streiter.PWfit$pareto.full$loglike+2
PrairiePlot1.Streiter.weibull.AIC<-(-2)*PrairiePlot1.Streiter.PWfit$weibull.full$loglike+4
PrairiePlot1.Streiter.exponent<-PrairiePlot1.Streiter.PWfit$pareto.full$exponent
PrairiePlot1.Streiter.shape<-PrairiePlot1.Streiter.PWfit$weibull.full$shape
PrairiePlot1.Streiter.scale<-PrairiePlot1.Streiter.PWfit$weibull.full$scale

#for Prairie Plot 2
sizes<-PrairiePlot2.Streiter$mass.g #assigns weights to variable "sizes"
logloghistplot(sizes,bins=50,xl="ln(Mass (g))")
legend(x="topright", legend=c("BFEC Prairie 2","(318 m)"), bty="n", cex=1.25)


PrairiePlot2.Streiter.PWfit<-PandW.fits(PrairiePlot2.Streiter$mass.g)
PrairiePlot2.Streiter.X.s<-PrairiePlot2.Streiter.PWfit$pareto.full.fits[,1]
PrairiePlot2.Streiter.Pfit.d<-PrairiePlot2.Streiter.PWfit$pareto.full.fits[,2]
PrairiePlot2.Streiter.Wfit.d<-PrairiePlot2.Streiter.PWfit$weibull.full.fits[,2]
lines(log(PrairiePlot2.Streiter.Pfit.d)~log(PrairiePlot2.Streiter.X.s), col="red")
lines(log(PrairiePlot2.Streiter.Wfit.d)~log(PrairiePlot2.Streiter.X.s), col="blue")
PrairiePlot2.Streiter.pareto.AIC<-(-2)*PrairiePlot2.Streiter.PWfit$pareto.full$loglike+2
PrairiePlot2.Streiter.weibull.AIC<-(-2)*PrairiePlot2.Streiter.PWfit$weibull.full$loglike+4
PrairiePlot2.Streiter.exponent<-PrairiePlot2.Streiter.PWfit$pareto.full$exponent
PrairiePlot2.Streiter.shape<-PrairiePlot2.Streiter.PWfit$weibull.full$shape
PrairiePlot2.Streiter.scale<-PrairiePlot2.Streiter.PWfit$weibull.full$scale

#for Prairie Plot 3
sizes<-PrairiePlot3.Boicourt$mass.g #assigns weights to variable "sizes"
logloghistplot(sizes,bins=50,xl="ln(Mass (g))")
legend(x="topright", legend=c("BFEC Prairie 3","(318 m)"), bty="n", cex=1.25)


PrairiePlot3.Boicourt.PWfit<-PandW.fits(PrairiePlot3.Boicourt$mass.g)
PrairiePlot3.Boicourt.X.s<-PrairiePlot3.Boicourt.PWfit$pareto.full.fits[,1]
PrairiePlot3.Boicourt.Pfit.d<-PrairiePlot3.Boicourt.PWfit$pareto.full.fits[,2]
PrairiePlot3.Boicourt.Wfit.d<-PrairiePlot3.Boicourt.PWfit$weibull.full.fits[,2]
lines(log(PrairiePlot3.Boicourt.Pfit.d)~log(PrairiePlot3.Boicourt.X.s), col="red")
lines(log(PrairiePlot3.Boicourt.Wfit.d)~log(PrairiePlot3.Boicourt.X.s), col="blue")
PrairiePlot3.Boicourt.pareto.AIC<-(-2)*PrairiePlot3.Boicourt.PWfit$pareto.full$loglike+2
PrairiePlot3.Boicourt.weibull.AIC<-(-2)*PrairiePlot3.Boicourt.PWfit$weibull.full$loglike+4
PrairiePlot3.Boicourt.exponent<-PrairiePlot3.Boicourt.PWfit$pareto.full$exponent
PrairiePlot3.Boicourt.shape<-PrairiePlot3.Boicourt.PWfit$weibull.full$shape
PrairiePlot3.Boicourt.scale<-PrairiePlot3.Boicourt.PWfit$weibull.full$scale

#for forest 2006
sizes<-BBFtrees$mass.kg.06*1000 #assigns weights to variable "sizes"
logloghistplot(sizes,bins=50,xl="ln(Mass (g))")
legend(x="topright", legend=c("BFEC Forest","(324 m)"), bty="n", cex=1.25)


BBFtrees.06.PWfit<-PandW.fits(na.omit(BBFtrees$mass.kg.06)*1000)
BBFtrees.06.X.s<-BBFtrees.06.PWfit$pareto.full.fits[,1]
BBFtrees.06.Pfit.d<-BBFtrees.06.PWfit$pareto.full.fits[,2]
BBFtrees.06.Wfit.d<-BBFtrees.06.PWfit$weibull.full.fits[,2]
lines(log(BBFtrees.06.Pfit.d)~log(BBFtrees.06.X.s), col="red")
lines(log(BBFtrees.06.Wfit.d)~log(BBFtrees.06.X.s), col="blue")
BBFtrees06.pareto.AIC<-(-2)*BBFtrees.06.PWfit$pareto.full$loglike+2
BBFtrees06.weibull.AIC<-(-2)*BBFtrees.06.PWfit$weibull.full$loglike+4
BBFtrees06.exponent<-BBFtrees.06.PWfit$pareto.full$exponent
BBFtrees06.shape<-BBFtrees.06.PWfit$weibull.full$shape
BBFtrees06.scale<-BBFtrees.06.PWfit$weibull.full$scale

#for forest 2011
sizes<-BBFtrees$mass.kg.11*1000 #assigns weights to variable "sizes"
logloghistplot(sizes,bins=50,xl="ln(Mass (g))")
legend(x="topright", legend=c("BFEC Forest","(324 m)"), bty="n", cex=1.25)


BBFtrees.PWfit<-PandW.fits(na.omit(BBFtrees$mass.kg.11)*1000)
BBFtrees.X.s<-BBFtrees.PWfit$pareto.full.fits[,1]
BBFtrees.Pfit.d<-BBFtrees.PWfit$pareto.full.fits[,2]
BBFtrees.Wfit.d<-BBFtrees.PWfit$weibull.full.fits[,2]
lines(log(BBFtrees.Pfit.d)~log(BBFtrees.X.s), col="red")
lines(log(BBFtrees.Wfit.d)~log(BBFtrees.X.s), col="blue")
BBFtrees11.pareto.AIC<-(-2)*BBFtrees.PWfit$pareto.full$loglike+2
BBFtrees11.weibull.AIC<-(-2)*BBFtrees.PWfit$weibull.full$loglike+4
BBFtrees11.exponent<-BBFtrees.PWfit$pareto.full$exponent
BBFtrees11.shape<-BBFtrees.PWfit$weibull.full$shape
BBFtrees11.scale<-BBFtrees.PWfit$weibull.full$scale

#for RMBL.Almont
sizes<-RMBL.Almont$biomass #assigns weights to variable "sizes"
logloghistplot(sizes,bins=50,xl="ln(Mass (g))")
legend(x="topright", legend=c("RMBL Grass/Sage","(2,465 m)"), bty="n", cex=1.25)


RMBL.Almont.PWfit<-PandW.fits(na.omit(RMBL.Almont$biomass))
RMBL.Almont.X.s<-RMBL.Almont.PWfit$pareto.full.fits[,1]
RMBL.Almont.Pfit.d<-RMBL.Almont.PWfit$pareto.full.fits[,2]
RMBL.Almont.Wfit.d<-RMBL.Almont.PWfit$weibull.full.fits[,2]
lines(log(RMBL.Almont.Pfit.d)~log(RMBL.Almont.X.s), col="red")
lines(log(RMBL.Almont.Wfit.d)~log(RMBL.Almont.X.s), col="blue")
RMBL.Almont.pareto.AIC<-(-2)*RMBL.Almont.PWfit$pareto.full$loglike+2
RMBL.Almont.weibull.AIC<-(-2)*RMBL.Almont.PWfit$weibull.full$loglike+4
RMBL.Almont.exponent<-RMBL.Almont.PWfit$pareto.full$exponent
RMBL.Almont.shape<-RMBL.Almont.PWfit$weibull.full$shape
RMBL.Almont.scale<-RMBL.Almont.PWfit$weibull.full$scale

#for RMBL.CBT
sizes<-RMBL.CBT$biomass #assigns weights to variable "sizes"
logloghistplot(sizes,bins=50,xl="ln(Mass (g))")
legend(x="topright", legend=c("RMBL Wet Meadow","(2,700 m)"), bty="n", cex=1.25)

RMBL.CBT.PWfit<-PandW.fits(na.omit(RMBL.CBT$biomass))
RMBL.CBT.X.s<-RMBL.CBT.PWfit$pareto.full.fits[,1]
RMBL.CBT.Pfit.d<-RMBL.CBT.PWfit$pareto.full.fits[,2]
RMBL.CBT.Wfit.d<-RMBL.CBT.PWfit$weibull.full.fits[,2]
lines(log(RMBL.CBT.Pfit.d)~log(RMBL.CBT.X.s), col="red")
lines(log(RMBL.CBT.Wfit.d)~log(RMBL.CBT.X.s), col="blue")
RMBL.CBT.pareto.AIC<-(-2)*RMBL.CBT.PWfit$pareto.full$loglike+2
RMBL.CBT.weibull.AIC<-(-2)*RMBL.CBT.PWfit$weibull.full$loglike+4
RMBL.CBT.exponent<-RMBL.CBT.PWfit$pareto.full$exponent
RMBL.CBT.shape<-RMBL.CBT.PWfit$weibull.full$shape
RMBL.CBT.scale<-RMBL.CBT.PWfit$weibull.full$scale

#for RMBL.PBM
sizes<-RMBL.PBM$biomass #assigns weights to variable "sizes"
logloghistplot(sizes,bins=50,xl="ln(Mass (g))")
legend(x="topright", legend=c("RMBL Treeline Meadow","(3,380 m)"), bty="n", cex=1.25)


RMBL.PBM.PWfit<-PandW.fits(na.omit(RMBL.PBM$biomass))
RMBL.PBM.X.s<-RMBL.PBM.PWfit$pareto.full.fits[,1]
RMBL.PBM.Pfit.d<-RMBL.PBM.PWfit$pareto.full.fits[,2]
RMBL.PBM.Wfit.d<-RMBL.PBM.PWfit$weibull.full.fits[,2]
lines(log(RMBL.PBM.Pfit.d)~log(RMBL.PBM.X.s), col="red")
lines(log(RMBL.PBM.Wfit.d)~log(RMBL.PBM.X.s), col="blue")
RMBL.PBM.pareto.AIC<-(-2)*RMBL.PBM.PWfit$pareto.full$loglike+2
RMBL.PBM.weibull.AIC<-(-2)*RMBL.PBM.PWfit$weibull.full$loglike+4
RMBL.PBM.exponent<-RMBL.PBM.PWfit$pareto.full$exponent
RMBL.PBM.shape<-RMBL.PBM.PWfit$weibull.full$shape
RMBL.PBM.scale<-RMBL.PBM.PWfit$weibull.full$scale

#for RMBL.Pfeiler
sizes<-RMBL.Pfeiler$biomass #assigns weights to variable "sizes"
logloghistplot(sizes,bins=50,xl="ln(Mass (g))")
legend(x="topright", legend=c("RMBL Alpine Meadow","(3,165 m)"), bty="n", cex=1.25)


RMBL.Pfeiler.PWfit<-PandW.fits(na.omit(RMBL.Pfeiler$biomass))
RMBL.Pfeiler.X.s<-RMBL.Pfeiler.PWfit$pareto.full.fits[,1]
RMBL.Pfeiler.Pfit.d<-RMBL.Pfeiler.PWfit$pareto.full.fits[,2]
RMBL.Pfeiler.Wfit.d<-RMBL.Pfeiler.PWfit$weibull.full.fits[,2]
lines(log(RMBL.Pfeiler.Pfit.d)~log(RMBL.Pfeiler.X.s), col="red")
lines(log(RMBL.Pfeiler.Wfit.d)~log(RMBL.Pfeiler.X.s), col="blue")
RMBL.Pfeiler.pareto.AIC<-(-2)*RMBL.Pfeiler.PWfit$pareto.full$loglike+2
RMBL.Pfeiler.weibull.AIC<-(-2)*RMBL.Pfeiler.PWfit$weibull.full$loglike+4
RMBL.Pfeiler.exponent<-RMBL.Pfeiler.PWfit$pareto.full$exponent
RMBL.Pfeiler.shape<-RMBL.Pfeiler.PWfit$weibull.full$shape
RMBL.Pfeiler.scale<-RMBL.Pfeiler.PWfit$weibull.full$scale

#for RMBL.Road
sizes<-RMBL.Road$biomass #assigns weights to variable "sizes"
logloghistplot(sizes,bins=50,xl="ln(Mass (g))")
legend(x="topright", legend=c("RMBL Meadow","(2,815 m)"), bty="n", cex=1.25)

RMBL.Road.PWfit<-PandW.fits(na.omit(RMBL.Road$biomass))
RMBL.Road.X.s<-RMBL.Road.PWfit$pareto.full.fits[,1]
RMBL.Road.Pfit.d<-RMBL.Road.PWfit$pareto.full.fits[,2]
RMBL.Road.Wfit.d<-RMBL.Road.PWfit$weibull.full.fits[,2]
lines(log(RMBL.Road.Pfit.d)~log(RMBL.Road.X.s), col="red")
lines(log(RMBL.Road.Wfit.d)~log(RMBL.Road.X.s), col="blue")
RMBL.Road.pareto.AIC<-(-2)*RMBL.Road.PWfit$pareto.full$loglike+2
RMBL.Road.weibull.AIC<-(-2)*RMBL.Road.PWfit$weibull.full$loglike+4
RMBL.Road.exponent<-RMBL.Road.PWfit$pareto.full$exponent
RMBL.Road.shape<-RMBL.Road.PWfit$weibull.full$shape
RMBL.Road.scale<-RMBL.Road.PWfit$weibull.full$scale

#for Desert Data - Tumamoc
sizes<-TumamocPlotMass$DryMass #assigns weights to variable "sizes"
logloghistplot(sizes,bins=50,xl="ln(Mass (g))")
legend(x="topright", legend=c("Tumamoc Hill Desert","(947 m)"), bty="n", cex=1.25)

TumamocPlotMass.PWfit<-PandW.fits(na.omit(TumamocPlotMass$DryMass))
TumamocPlotMass.X.s<-TumamocPlotMass.PWfit$pareto.full.fits[,1]
TumamocPlotMass.Pfit.d<-TumamocPlotMass.PWfit$pareto.full.fits[,2]
TumamocPlotMass.tPfit.d<-TumamocPlotMass.PWfit$tpareto.full.fits[,2]
TumamocPlotMass.Wfit.d<-TumamocPlotMass.PWfit$weibull.full.fits[,2]
lines(log(TumamocPlotMass.Pfit.d)~log(TumamocPlotMass.X.s), col="red")
lines(log(TumamocPlotMass.Wfit.d)~log(TumamocPlotMass.X.s), col="blue")
lines(log(TumamocPlotMass.tPfit.d)~log(TumamocPlotMass.X.s), col="green")

TumamocPlotMass.pareto.AIC<-(-2)*TumamocPlotMass.PWfit$pareto.full$loglike+2
TumamocPlotMass.tpareto.AIC<-(-2)*TumamocPlotMass.PWfit$tpareto.full$loglike+2

TumamocPlotMass.weibull.AIC<-(-2)*TumamocPlotMass.PWfit$weibull.full$loglike+4
TumamocPlotMass.exponent<-TumamocPlotMass.PWfit$pareto.full$exponent
TumamocPlotMass.shape<-TumamocPlotMass.PWfit$weibull.full$shape
TumamocPlotMass.scale<-TumamocPlotMass.PWfit$weibull.full$scale

