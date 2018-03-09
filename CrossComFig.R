#CrossComFigure
par(mar=c(5,5,4,2)+0.1)
plot(log(DengCrops$Shoot.mass.g.),log(DengCrops$Actual.stand.density.no..m2.), xlim=c(-9,16), ylim=c(-5,13), col="darkgray", cex.lab=1.2, xlab="ln(Plant mass (g))", ylab=expression(paste("ln(Plant density (N ",m^-2,"))")))
points(log(DengForestLuo$Shoot.mass.g.),log(DengForestLuo$Actual.stand.density.no..m2.), col="black")
points(log(DengPlantationsLuo$Shoot.mass.g.),log(DengPlantationsLuo$Actual.stand.density.no..m2.), col="black")
points(log(DengBamboo$Shoot.mass.g.),log(DengBamboo$Actual.stand.density.no..m2.), col="black")
points(log(CrossCom$MeanSize),log(CrossCom$Density), col="red", pch=19)

#Try using ggplot



ggplot(DengCrops, aes(x=log(Shoot.mass.g.), y=log(Actual.stand.density.no..m2.))) +
  geom_point(color="darkgray") +
  geom_point(DengBamboo, aes(x=log(Shoot.mass.g.), y=log(Actual.stand.density.no..m2.)), color="black")
  theme_bw() +
  xlim(c(-9,16)) +
  ylim(c(-5,13)) +
  xlab("ln(Plant mass (g))") +
  ylab(expression(paste("ln(Plant density (N ",m^-2,"))")))
