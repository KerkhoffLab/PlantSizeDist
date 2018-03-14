# Fit alternative Pareto, Truncated Pareto, and Weibull fits of 
# Individual Size Distribution Models for all of the plots read in via ReadData.R
# Kelsey Dillon and Drew Kerkhoff
# Written September 2014, Updated March 2018

# This script runs plot level individual size distribution analyses and makes a figure
# for each plot comparing Pareto and Weibull fits

# Source all relevant functions

source("Rcode/ParetoFunctionDefinitions.R")
source("Rcode/WeibullFunctionDefinitions.R")
source("Rcode/pwtp.fits.r")
source("Rcode/logloghistplot.r")


# BFEC --------------------------------------------------------------------

#Wetland plot 1
#analysis
Wetland1.PWfit<-pwtp.fits(BFECWetland1$Weight)
Wetland1.X.s<-Wetland1.PWfit$pareto.full.fits[,1]
Wetland1.Pfit.d<-Wetland1.PWfit$pareto.full.fits[,2]
Wetland1.Wfit.d<-Wetland1.PWfit$weibull.full.fits[,2]

Wetland1.pareto.AIC<-(-2)*Wetland1.PWfit$pareto.full$loglike+2
Wetland1.weibull.AIC<-(-2)*Wetland1.PWfit$weibull.full$loglike+4
Wetland1.exponent<-Wetland1.PWfit$pareto.full$exponent
Wetland1.shape<-Wetland1.PWfit$weibull.full$shape
Wetland1.scale<-Wetland1.PWfit$weibull.full$scale

#figure
pdf("Figures/BFECWetland1.pdf", width=6.5, height=5)

logloghistplot(BFECWetland1$Weight,bins=50,xl="ln(Mass (g))")
legend(x="topright", legend=c("BFEC Wetland 1","(315 m)"), bty="n",cex=1.25)
lines(log(Wetland1.Pfit.d)~log(Wetland1.X.s), col="red")
lines(log(Wetland1.Wfit.d)~log(Wetland1.X.s), col="blue")

dev.off()

#Wetland plot 2
#analysis
Wetland2.PWfit<-pwtp.fits(BFECWetland2$Weight)
Wetland2.X.s<-Wetland2.PWfit$pareto.full.fits[,1]
Wetland2.Pfit.d<-Wetland2.PWfit$pareto.full.fits[,2]
Wetland2.Wfit.d<-Wetland2.PWfit$weibull.full.fits[,2]

Wetland2.pareto.AIC<-(-2)*Wetland2.PWfit$pareto.full$loglike+2
Wetland2.weibull.AIC<-(-2)*Wetland2.PWfit$weibull.full$loglike+4
Wetland2.exponent<-Wetland2.PWfit$pareto.full$exponent
Wetland2.shape<-Wetland2.PWfit$weibull.full$shape
Wetland2.scale<-Wetland2.PWfit$weibull.full$scale

#figure

pdf("Figures/BFECWetland2.pdf", width=6.5, height=5)

logloghistplot(BFECWetland2$Weight,bins=50,xl="ln(Mass (g))")
legend(x="topright", legend=c("BFEC Wetland 2","(315 m)"), bty="n",cex=1.25)
lines(log(Wetland2.Pfit.d)~log(Wetland2.X.s), col="red")
lines(log(Wetland2.Wfit.d)~log(Wetland2.X.s), col="blue")

dev.off()

#Prairie plot 1
#analysis
Prairie1.PWfit<-pwtp.fits(BFECPrairie1$mass.g)
Prairie1.X.s<-Prairie1.PWfit$pareto.full.fits[,1]
Prairie1.Pfit.d<-Prairie1.PWfit$pareto.full.fits[,2]
Prairie1.Wfit.d<-Prairie1.PWfit$weibull.full.fits[,2]

Prairie1.pareto.AIC<-(-2)*Prairie1.PWfit$pareto.full$loglike+2
Prairie1.weibull.AIC<-(-2)*Prairie1.PWfit$weibull.full$loglike+4
Prairie1.exponent<-Prairie1.PWfit$pareto.full$exponent
Prairie1.shape<-Prairie1.PWfit$weibull.full$shape
Prairie1.scale<-Prairie1.PWfit$weibull.full$scale

#figure

pdf("Figures/BFECPrairie1.pdf", width=6.5, height=5)

logloghistplot(BFECPrairie1$mass.g,bins=50,xl="ln(Mass (g))")
legend(x="topright", legend=c("BFEC Prairie 1","(318 m)"), bty="n",cex=1.25)
lines(log(Prairie1.Pfit.d)~log(Prairie1.X.s), col="red")
lines(log(Prairie1.Wfit.d)~log(Prairie1.X.s), col="blue")

dev.off()

#Prairie plot 2
#analysis
Prairie2.PWfit<-pwtp.fits(BFECPrairie2$mass.g)
Prairie2.X.s<-Prairie2.PWfit$pareto.full.fits[,1]
Prairie2.Pfit.d<-Prairie2.PWfit$pareto.full.fits[,2]
Prairie2.Wfit.d<-Prairie2.PWfit$weibull.full.fits[,2]

Prairie2.pareto.AIC<-(-2)*Prairie2.PWfit$pareto.full$loglike+2
Prairie2.weibull.AIC<-(-2)*Prairie2.PWfit$weibull.full$loglike+4
Prairie2.exponent<-Prairie2.PWfit$pareto.full$exponent
Prairie2.shape<-Prairie2.PWfit$weibull.full$shape
Prairie2.scale<-Prairie2.PWfit$weibull.full$scale

#figure

pdf("Figures/BFECPrairie2.pdf", width=6.5, height=5)

logloghistplot(BFECPrairie2$mass.g,bins=50,xl="ln(Mass (g))")
legend(x="topright", legend=c("BFEC Prairie 2","(318 m)"), bty="n",cex=1.25)
lines(log(Prairie2.Pfit.d)~log(Prairie2.X.s), col="red")
lines(log(Prairie2.Wfit.d)~log(Prairie2.X.s), col="blue")

dev.off()

#Prairie plot 3
#analysis
Prairie3.PWfit<-pwtp.fits(BFECPrairie3$mass.g)
Prairie3.X.s<-Prairie3.PWfit$pareto.full.fits[,1]
Prairie3.Pfit.d<-Prairie3.PWfit$pareto.full.fits[,2]
Prairie3.Wfit.d<-Prairie3.PWfit$weibull.full.fits[,2]

Prairie3.pareto.AIC<-(-2)*Prairie3.PWfit$pareto.full$loglike+2
Prairie3.weibull.AIC<-(-2)*Prairie3.PWfit$weibull.full$loglike+4
Prairie3.exponent<-Prairie3.PWfit$pareto.full$exponent
Prairie3.shape<-Prairie3.PWfit$weibull.full$shape
Prairie3.scale<-Prairie3.PWfit$weibull.full$scale

#figure

pdf("Figures/BFECPrairie3.pdf", width=6.5, height=5)

logloghistplot(BFECPrairie3$mass.g,bins=50,xl="ln(Mass (g))")
legend(x="topright", legend=c("BFEC Prairie 3","(318 m)"), bty="n",cex=1.25)
lines(log(Prairie3.Pfit.d)~log(Prairie3.X.s), col="red")
lines(log(Prairie3.Wfit.d)~log(Prairie3.X.s), col="blue")

dev.off()

#Forest plot
#analysis
Forest.PWfit<-pwtp.fits(BFECForest$Biomass*1000)
Forest.X.s<-Forest.PWfit$pareto.full.fits[,1]
Forest.Pfit.d<-Forest.PWfit$pareto.full.fits[,2]
Forest.Wfit.d<-Forest.PWfit$weibull.full.fits[,2]

Forest.pareto.AIC<-(-2)*Forest.PWfit$pareto.full$loglike+2
Forest.weibull.AIC<-(-2)*Forest.PWfit$weibull.full$loglike+4
Forest.exponent<-Forest.PWfit$pareto.full$exponent
Forest.shape<-Forest.PWfit$weibull.full$shape
Forest.scale<-Forest.PWfit$weibull.full$scale

#figure

pdf("Figures/BFECForest.pdf", width=6.5, height=5)

logloghistplot(BFECForest$Biomass*1000,bins=50,xl="ln(Mass (g))")
legend(x="topright", legend=c("BFEC Forest","(324 m)"), bty="n", cex=1.25)
lines(log(Forest.Pfit.d)~log(Forest.X.s), col="red")
lines(log(Forest.Wfit.d)~log(Forest.X.s), col="blue")

dev.off()


# RMBL --------------------------------------------------------------------

#Grass/Sage plot (Almont)
#analysis
GrassSage.PWfit<-pwtp.fits(na.omit(RMBLGrassSage$biomass))
GrassSage.X.s<-GrassSage.PWfit$pareto.full.fits[,1]
GrassSage.Pfit.d<-GrassSage.PWfit$pareto.full.fits[,2]
GrassSage.Wfit.d<-GrassSage.PWfit$weibull.full.fits[,2]

GrassSage.pareto.AIC<-(-2)*GrassSage.PWfit$pareto.full$loglike+2
GrassSage.weibull.AIC<-(-2)*GrassSage.PWfit$weibull.full$loglike+4
GrassSage.exponent<-GrassSage.PWfit$pareto.full$exponent
GrassSage.shape<-GrassSage.PWfit$weibull.full$shape
GrassSage.scale<-GrassSage.PWfit$weibull.full$scale

#figure

pdf("Figures/RMBLGrassSage.pdf", width=6.5, height=5)

logloghistplot(na.omit(RMBLGrassSage$biomass),bins=50,xl="ln(Mass (g))")
legend(x="topright", legend=c("RMBL Grass/Sage","(2,465 m)"), bty="n", cex=1.25)
lines(log(GrassSage.Pfit.d)~log(GrassSage.X.s), col="red")
lines(log(GrassSage.Wfit.d)~log(GrassSage.X.s), col="blue")

dev.off()

#Wet meadow plot (CBT)
#analysis
WetMeadow.PWfit<-pwtp.fits(na.omit(RMBLWetMeadow$biomass))
WetMeadow.X.s<-WetMeadow.PWfit$pareto.full.fits[,1]
WetMeadow.Pfit.d<-WetMeadow.PWfit$pareto.full.fits[,2]
WetMeadow.Wfit.d<-WetMeadow.PWfit$weibull.full.fits[,2]

WetMeadow.pareto.AIC<-(-2)*WetMeadow.PWfit$pareto.full$loglike+2
WetMeadow.weibull.AIC<-(-2)*WetMeadow.PWfit$weibull.full$loglike+4
WetMeadow.exponent<-WetMeadow.PWfit$pareto.full$exponent
WetMeadow.shape<-WetMeadow.PWfit$weibull.full$shape
WetMeadow.scale<-WetMeadow.PWfit$weibull.full$scale

#figure

pdf("Figures/RMBLWetMeadow.pdf", width=6.5, height=5)

logloghistplot(na.omit(RMBLWetMeadow$biomass),bins=50,xl="ln(Mass (g))")
legend(x="topright", legend=c("RMBL Wet Meadow","(2,700 m)"), bty="n", cex=1.25)
lines(log(WetMeadow.Pfit.d)~log(WetMeadow.X.s), col="red")
lines(log(WetMeadow.Wfit.d)~log(WetMeadow.X.s), col="blue")

dev.off()

#Alpine plot 1 (Road)
#analysis
Alpine1.PWfit<-pwtp.fits(na.omit(RMBLAlpine1$biomass))
Alpine1.X.s<-Alpine1.PWfit$pareto.full.fits[,1]
Alpine1.Pfit.d<-Alpine1.PWfit$pareto.full.fits[,2]
Alpine1.Wfit.d<-Alpine1.PWfit$weibull.full.fits[,2]

Alpine1.pareto.AIC<-(-2)*Alpine1.PWfit$pareto.full$loglike+2
Alpine1.weibull.AIC<-(-2)*Alpine1.PWfit$weibull.full$loglike+4
Alpine1.exponent<-Alpine1.PWfit$pareto.full$exponent
Alpine1.shape<-Alpine1.PWfit$weibull.full$shape
Alpine1.scale<-Alpine1.PWfit$weibull.full$scale

#figure

pdf("Figures/RMBLAlpine1.pdf", width=6.5, height=5)

logloghistplot(na.omit(RMBLAlpine1$biomass),bins=50,xl="ln(Mass (g))")
legend(x="topright", legend=c("RMBL Alpine 1","(2,815 m)"), bty="n", cex=1.25)
lines(log(Alpine1.Pfit.d)~log(Alpine1.X.s), col="red")
lines(log(Alpine1.Wfit.d)~log(Alpine1.X.s), col="blue")

dev.off()

#Alpine plot 2 (Pfeiler)
#analysis
Alpine2.PWfit<-pwtp.fits(na.omit(RMBLAlpine2$biomass))
Alpine2.X.s<-Alpine2.PWfit$pareto.full.fits[,1]
Alpine2.Pfit.d<-Alpine2.PWfit$pareto.full.fits[,2]
Alpine2.Wfit.d<-Alpine2.PWfit$weibull.full.fits[,2]

Alpine2.pareto.AIC<-(-2)*Alpine2.PWfit$pareto.full$loglike+2
Alpine2.weibull.AIC<-(-2)*Alpine2.PWfit$weibull.full$loglike+4
Alpine2.exponent<-Alpine2.PWfit$pareto.full$exponent
Alpine2.shape<-Alpine2.PWfit$weibull.full$shape
Alpine2.scale<-Alpine2.PWfit$weibull.full$scale

#figure

pdf("Figures/RMBLAlpine2.pdf", width=6.5, height=5)

logloghistplot(na.omit(RMBLAlpine2$biomass),bins=50,xl="ln(Mass (g))")
legend(x="topright", legend=c("RMBL Alpine 2","(3,165 m)"), bty="n", cex=1.25)
lines(log(Alpine2.Pfit.d)~log(Alpine2.X.s), col="red")
lines(log(Alpine2.Wfit.d)~log(Alpine2.X.s), col="blue")

dev.off()

#Alpine plot 3 (PBM)
#analysis
Alpine3.PWfit<-pwtp.fits(na.omit(RMBLAlpine3$biomass))
Alpine3.X.s<-Alpine3.PWfit$pareto.full.fits[,1]
Alpine3.Pfit.d<-Alpine3.PWfit$pareto.full.fits[,2]
Alpine3.Wfit.d<-Alpine3.PWfit$weibull.full.fits[,2]

Alpine3.pareto.AIC<-(-2)*Alpine3.PWfit$pareto.full$loglike+2
Alpine3.weibull.AIC<-(-2)*Alpine3.PWfit$weibull.full$loglike+4
Alpine3.exponent<-Alpine3.PWfit$pareto.full$exponent
Alpine3.shape<-Alpine3.PWfit$weibull.full$shape
Alpine3.scale<-Alpine3.PWfit$weibull.full$scale

#figure

pdf("Figures/RMBLAlpine3.pdf", width=6.5, height=5)

logloghistplot(na.omit(RMBLAlpine3$biomass),bins=50,xl="ln(Mass (g))")
legend(x="topright", legend=c("RMBL Alpine 3","(3,380 m)"), bty="n", cex=1.25)
lines(log(Alpine3.Pfit.d)~log(Alpine3.X.s), col="red")
lines(log(Alpine3.Wfit.d)~log(Alpine3.X.s), col="blue")

dev.off()

# THDL --------------------------------------------------------------------

#Desert plot
#analysis
Desert.PWfit<-pwtp.fits(na.omit(THDLDesert$DryMass))
Desert.X.s<-Desert.PWfit$pareto.full.fits[,1]
Desert.Pfit.d<-Desert.PWfit$pareto.full.fits[,2]
Desert.Wfit.d<-Desert.PWfit$weibull.full.fits[,2]

Desert.pareto.AIC<-(-2)*Desert.PWfit$pareto.full$loglike+2
Desert.weibull.AIC<-(-2)*Desert.PWfit$weibull.full$loglike+4
Desert.exponent<-Desert.PWfit$pareto.full$exponent
Desert.shape<-Desert.PWfit$weibull.full$shape
Desert.scale<-Desert.PWfit$weibull.full$scale

#figure

pdf("Figures/THDLDesert.pdf", width=6.5, height=5)

logloghistplot(na.omit(THDLDesert$DryMass),bins=50,xl="ln(Mass (g))")
legend(x="topright", legend=c("THDL Desert","(947 m)"), bty="n", cex=1.25)
lines(log(Desert.Pfit.d)~log(Desert.X.s), col="red")
lines(log(Desert.Wfit.d)~log(Desert.X.s), col="blue")

dev.off()
