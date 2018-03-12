# Data on plant sizes in forest and non-forest communities
# Kelsey Dillon and Drew Kerkhoff
# Updated March 2018

# Data for this study are drawn from a number of independent studies at three sites
# and collected by a number of different researchers
# Brown Family Environmental Center (BFEC), Gambier, OH. Dillon, Lodge, Hamilton, Henderson, Kerkhoff
# Rocky Mountain Biological Lab (RMBL), Gothic, CO. Henderson, Sloat, Enquist, Kerkhoff.
# Tumamoc Hill Desert Lab (THDL), Tucson, AZ. Price.

# Since the data are heterogeneously collected across the studies, we keep the data in
# separate csv files for each study plot.
# While this is not the most efficient way to code and leads to repetition, we assigned each
# plot size distribution to its own dataframe.

# BFEC
BFECForest <- read.csv("data/BFECForest2011.csv", header=TRUE)
BFECPrairie1 <- read.csv("data/BFECPrairie1.csv", header=TRUE)
BFECPrairie2 <- read.csv("data/BFECPrairie2.csv", header=TRUE)
BFECPrairie3 <- read.csv("data/BFECPrairie3.csv", header=TRUE)
BFECWetland1 <- read.csv("data/BFECWetland1.csv", header=TRUE)
BFECWetland2 <- read.csv("data/BFECWetland2.csv", header=TRUE)

#RMBL
RMBLAlpine1 <- read.csv("data/RMBLAlpine1.csv", header=TRUE)
RMBLAlpine2 <- read.csv("data/RMBLAlpine2.csv", header=TRUE)
RMBLAlpine3 <- read.csv("data/RMBLAlpine3.csv", header=TRUE)
RMBLGrassSage <- read.csv("data/RMBLGrassSage.csv", header=TRUE)
RMBLWetMeadow <- read.csv("data/RMBLWetMeadow.csv", header=TRUE)

#THDL
THDLDesert <- read.csv("data/TumamocDesert.csv", header=TRUE)







