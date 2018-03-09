#cross community scaling
#Drew Kerkhoff
#construct a plot of community mean size vs. plant density, comparing across all 12 sites
#Plant Size Distribution Project
#Dillon et al.
#2016

CrossCom=data.frame(Site=c("BBForest","Tumamoc","Wetland1","Wetland2","Prairie1","Prairie2","Prairie3","Almont","CBT","Road","Pfeiler","Painterboy"), MeanSize=numeric(length=12), Area=c(20000,10000,1.44,1.44,1.44,1.44,1.44,1.69,1.69,1.69,1.69,1.69), Density=numeric(length=12), WeibShape=numeric(length=12), ParExp=numeric(length=12))

CrossCom$GeoMeanSize[1]=exp(mean(log(na.omit(BBFtrees$mass.kg.11*1000))))
CrossCom$Density[1]=length(na.omit(BBFtrees$mass.kg.11))/CrossCom$Area[1]
CrossCom$MeanSize[1]=mean(na.omit(BBFtrees$mass.kg.11*1000))

CrossCom$GeoMeanSize[2]=exp(mean(log(na.omit(TumamocPlotMass$DryMass))))
CrossCom$Density[2]=length(na.omit(TumamocPlotMass$DryMass))/CrossCom$Area[2]
CrossCom$MeanSize[2]=mean(na.omit(TumamocPlotMass$DryMass))

CrossCom$GeoMeanSize[3]=exp(mean(log(na.omit(Wetland1$Weight))))
CrossCom$Density[3]=length(na.omit(Wetland1$Weight))/CrossCom$Area[3]
CrossCom$MeanSize[3]=mean(na.omit(Wetland1$Weight))

CrossCom$GeoMeanSize[4]=exp(mean(log(na.omit(Wetland4$Weight))))
CrossCom$Density[4]=length(na.omit(Wetland4$Weight))/CrossCom$Area[4]
CrossCom$MeanSize[4]=mean(na.omit(Wetland4$Weight))

CrossCom$GeoMeanSize[5]=exp(mean(log(na.omit(PrairiePlot1.Streiter$mass.g))))
CrossCom$Density[5]=length(na.omit(PrairiePlot1.Streiter$mass.g))/CrossCom$Area[5]
CrossCom$MeanSize[5]=mean(na.omit(PrairiePlot1.Streiter$mass.g))

CrossCom$GeoMeanSize[6]=exp(mean(log(na.omit(PrairiePlot2.Streiter$mass.g))))
CrossCom$Density[6]=length(na.omit(PrairiePlot2.Streiter$mass.g))/CrossCom$Area[6]
CrossCom$MeanSize[6]=mean(na.omit(PrairiePlot2.Streiter$mass.g))

CrossCom$GeoMeanSize[7]=exp(mean(log(na.omit(PrairiePlot3.Boicourt$mass.g))))
CrossCom$Density[7]=length(na.omit(PrairiePlot3.Boicourt$mass.g))/CrossCom$Area[7]
CrossCom$MeanSize[7]=mean(na.omit(PrairiePlot3.Boicourt$mass.g))

CrossCom$GeoMeanSize[8]=exp(mean(log(na.omit(RMBL.Almont$biomass))))
CrossCom$Density[8]=length(na.omit(RMBL.Almont$biomass))/CrossCom$Area[8]
CrossCom$MeanSize[8]=mean(na.omit(RMBL.Almont$biomass))

CrossCom$GeoMeanSize[9]=exp(mean(log(na.omit(RMBL.CBT$biomass))))
CrossCom$Density[9]=length(na.omit(RMBL.CBT$biomass))/CrossCom$Area[9]
CrossCom$MeanSize[9]=mean(na.omit(RMBL.CBT$biomass))

CrossCom$GeoMeanSize[10]=exp(mean(log(na.omit(RMBL.Road$biomass))))
CrossCom$Density[10]=length(na.omit(RMBL.Road$biomass))/CrossCom$Area[10]
CrossCom$MeanSize[10]=mean(na.omit(RMBL.Road$biomass))

CrossCom$GeoMeanSize[11]=exp(mean(log(na.omit(RMBL.Pfeiler$biomass))))
CrossCom$Density[11]=length(na.omit(RMBL.Pfeiler$biomass))/CrossCom$Area[11]
CrossCom$MeanSize[11]=mean(na.omit(RMBL.Pfeiler$biomass))

CrossCom$GeoMeanSize[12]=exp(mean(log(na.omit(RMBL.PBM$biomass))))
CrossCom$Density[12]=length(na.omit(RMBL.PBM$biomass))/CrossCom$Area[12]
CrossCom$MeanSize[12]=mean(na.omit(RMBL.PBM$biomass))

par(cex=1.25, cex.axis=1.1, cex.lab=1.5, lwd=2.5)
plot(log(CrossCom$MeanSize),log(CrossCom$Density), xlab="ln(Plant mass (g))", ylab="ln(Plant density)")

CCreg=lm(log(Density)~log(MeanSize), data=CrossCom)
summary(CCreg)
abline(CCreg, col="dark green")

#Make plot with additional data from Deng et al. 2012

#load data

DengPlantationsLuo <- read.csv("~/GoogleDrive/Projects/BFEC herb communities/BFECSizeDist/DengPlantationsLuo.csv")
DengForestLuo <- read.csv("~/GoogleDrive/Projects/BFEC herb communities/BFECSizeDist/DengForestLuo.csv")
DengCrops <- read.csv("~/GoogleDrive/Projects/BFEC herb communities/BFECSizeDist/DengCrops.csv")
DengBamboo <- read.csv("~/GoogleDrive/Projects/BFEC herb communities/BFECSizeDist/DengBamboo.csv")

#make plot

plot(log(DengCrops$Shoot.mass.g.),log(DengCrops$Actual.stand.density.no..m2.), xlim=c(-9,16), ylim=c(-5,13), col="darkblue", xlab="ln(Plant mass (g))", ylab="ln(Plant density)")
points(log(DengForestLuo$Shoot.mass.g.),log(DengForestLuo$Actual.stand.density.no..m2.), col="gray")
points(log(DengPlantationsLuo$Shoot.mass.g.),log(DengPlantationsLuo$Actual.stand.density.no..m2.), col="blue")
points(log(DengBamboo$Shoot.mass.g.),log(DengBamboo$Actual.stand.density.no..m2.), col="cyan")
points(log(CrossCom$MeanSize),log(CrossCom$Density), col="red", pch=19)



