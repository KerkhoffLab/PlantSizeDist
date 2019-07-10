#Tree mortality in the BFEC Forest
#Nina Hamilton and Drew Kerkhoff
#construct a plot of mortality rate vs. plant size, from 2006 to 2011
#Plant Size Distribution Project
#Dillon et al.
#Drew Kerkhoff 2018

#Assumes you have already read in all of the plot data using `ReadData.R`

require(ggplot2)
require(dplyr)
require(tidyr)

# Bin the data by log DBH in 2006 then calculate annualized mortality rate in each size class
TreeDeath = BFECTreeMortality %>%
  mutate(SizeClass = cut(logDBH1, breaks=seq(0,2.1,0.1), labels=seq(0.05,2.1,0.1), 
                      include.lowest=TRUE),
         SizeClass = as.numeric(as.character(SizeClass))) %>%
  group_by(Status, SizeClass) %>% 
  summarise(n = n()) %>%
  spread(Status, n) %>%
  mutate(AnnMortRate = dead/(alive+dead)/5)

#plot annualized mortality as a function of size class

pdf("Figures/MortRateBFEC.pdf", height=5, width=5)
par(cex=1.25, cex.axis=1.1, cex.lab=1.5, lwd=2.5)

ggplot(TreeDeath,aes(x=SizeClass,y=AnnMortRate)) +
  geom_point() +
  geom_smooth() +
  labs(x="log(DBH)", y="Annualized mortality rate")+
  theme_bw(base_size=12)

dev.off()
