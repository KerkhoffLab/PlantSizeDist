#cross community scaling
#Drew Kerkhoff
#construct a plot of community mean size vs. plant density, comparing across all 12 sites
#Plant Size Distribution Project
#Dillon et al.
#Drew Kerkhoff 2016, updated 2018

#Assumes you have already read in all of the plot data using `ReadData.R`

#Make data frame to hold plot level observations

CrossCom=data.frame(Site=c("Forest","Desert","Wetland1","Wetland2","Prairie1","Prairie2","Prairie3","GrassSage","WetMeadow","Alpine1","Alpine2","Alpine3"), MeanSize=numeric(length=12), Area=c(20000,10000,1.44,1.44,1.44,1.44,1.44,1.69,1.69,1.69,1.69,1.69), Density=numeric(length=12), WeibShape=numeric(length=12), ParExp=numeric(length=12))

#Fill it in plot-by-plot

#BFEC Forest
CrossCom$GeoMeanSize[1]=exp(mean(log(na.omit(BFECForest$Biomass*1000))))
CrossCom$Density[1]=length(na.omit(BFECForest$Biomass))/CrossCom$Area[1]
CrossCom$MeanSize[1]=mean(na.omit(BFECForest$Biomass*1000))

#THDL Desert
CrossCom$GeoMeanSize[2]=exp(mean(log(na.omit(THDLDesert$DryMass))))
CrossCom$Density[2]=length(na.omit(THDLDesert$DryMass))/CrossCom$Area[2]
CrossCom$MeanSize[2]=mean(na.omit(THDLDesert$DryMass))

#BFEC Wetland 1
CrossCom$GeoMeanSize[3]=exp(mean(log(na.omit(BFECWetland1$Weight))))
CrossCom$Density[3]=length(na.omit(BFECWetland1$Weight))/CrossCom$Area[3]
CrossCom$MeanSize[3]=mean(na.omit(BFECWetland1$Weight))

#BFEC Wetland 2
CrossCom$GeoMeanSize[4]=exp(mean(log(na.omit(BFECWetland2$Weight))))
CrossCom$Density[4]=length(na.omit(BFECWetland2$Weight))/CrossCom$Area[4]
CrossCom$MeanSize[4]=mean(na.omit(BFECWetland2$Weight))

#BFEC Prairie 1
CrossCom$GeoMeanSize[5]=exp(mean(log(na.omit(BFECPrairie1$mass.g))))
CrossCom$Density[5]=length(na.omit(BFECPrairie1$mass.g))/CrossCom$Area[5]
CrossCom$MeanSize[5]=mean(na.omit(BFECPrairie1$mass.g))

#BFEC Prairie 2
CrossCom$GeoMeanSize[6]=exp(mean(log(na.omit(BFECPrairie2$mass.g))))
CrossCom$Density[6]=length(na.omit(BFECPrairie2$mass.g))/CrossCom$Area[6]
CrossCom$MeanSize[6]=mean(na.omit(BFECPrairie2$mass.g))

#BFEC Prairie 3
CrossCom$GeoMeanSize[7]=exp(mean(log(na.omit(BFECPrairie3$mass.g))))
CrossCom$Density[7]=length(na.omit(BFECPrairie3$mass.g))/CrossCom$Area[7]
CrossCom$MeanSize[7]=mean(na.omit(BFECPrairie3$mass.g))

#RMBL GrassSage
CrossCom$GeoMeanSize[8]=exp(mean(log(na.omit(RMBLGrassSage$biomass))))
CrossCom$Density[8]=length(na.omit(RMBLGrassSage$biomass))/CrossCom$Area[8]
CrossCom$MeanSize[8]=mean(na.omit(RMBLGrassSage$biomass))

#RMBL WetMeadow
CrossCom$GeoMeanSize[9]=exp(mean(log(na.omit(RMBLWetMeadow$biomass))))
CrossCom$Density[9]=length(na.omit(RMBLWetMeadow$biomass))/CrossCom$Area[9]
CrossCom$MeanSize[9]=mean(na.omit(RMBLWetMeadow$biomass))

#RMBL Alpine1
CrossCom$GeoMeanSize[10]=exp(mean(log(na.omit(RMBLAlpine1$biomass))))
CrossCom$Density[10]=length(na.omit(RMBLAlpine1$biomass))/CrossCom$Area[10]
CrossCom$MeanSize[10]=mean(na.omit(RMBLAlpine1$biomass))

#RMBL Alpine2
CrossCom$GeoMeanSize[11]=exp(mean(log(na.omit(RMBLAlpine2$biomass))))
CrossCom$Density[11]=length(na.omit(RMBLAlpine2$biomass))/CrossCom$Area[11]
CrossCom$MeanSize[11]=mean(na.omit(RMBLAlpine2$biomass))

#RMBL Alpine3
CrossCom$GeoMeanSize[12]=exp(mean(log(na.omit(RMBLAlpine3$biomass))))
CrossCom$Density[12]=length(na.omit(RMBLAlpine3$biomass))/CrossCom$Area[12]
CrossCom$MeanSize[12]=mean(na.omit(RMBLAlpine3$biomass))

#Regression across sampled communities
CCreg=lm(log(Density)~log(MeanSize), data=CrossCom)
summary(CCreg)
abline(CCreg, col="dark green")

#Make plot with additional data from Deng et al. 2012

#load data

DengPlantationsLuo <- read.csv("Data/CrossCom/DengPlantationsLuo.csv")
DengForestLuo <- read.csv("Data/CrossCom/DengForestLuo.csv")
DengCrops <- read.csv("Data/CrossCom/DengCrops.csv")
DengBamboo <- read.csv("Data/CrossCom/DengBamboo.csv")

#make plot
pdf("Figures/CrossCom.pdf", height=5, width=5)
par(cex=1.25, cex.axis=1.1, cex.lab=1.5, lwd=2.5)

plot(log(DengCrops$Shoot.mass.g.),log(DengCrops$Actual.stand.density.no..m2.), xlim=c(-9,16), ylim=c(-5,13), col="gray", xlab="ln(Plant mass (g))", ylab="ln(Plant density)")
points(log(DengForestLuo$Shoot.mass.g.),log(DengForestLuo$Actual.stand.density.no..m2.), col="black")
points(log(DengPlantationsLuo$Shoot.mass.g.),log(DengPlantationsLuo$Actual.stand.density.no..m2.), col="black")
points(log(DengBamboo$Shoot.mass.g.),log(DengBamboo$Actual.stand.density.no..m2.), col="black")
points(log(CrossCom$MeanSize),log(CrossCom$Density), col="red", pch=19)

dev.off()


