# PlantSizeDist
Analysis of plant size distributions in forest and non-forest communities

This repository contains all code and data associated with the paper "On the relationships between size and abundance in plants: beyond forest communities" by Kelsey T. Dillon, Amanda N. Henderson, Alexandra G. Lodge, Nina I. Hamilton, Lindsey L. Sloat, Brian J. Enquist, Charles A. Price, and Andrew J. Kerkhoff.

All analyses were done in R

Script inventory - scripts can be executed in numerical order:

O1.ReadData.R - load raw data and clean for subsequent analyses
02.IndSizeDist.R - model individual size distributions for each plot and compare Weibull and Pareto models (Figure 2, Table 2)
03.CrossCommunity.R - cross community analyses (Figure 3)


Folder inventory:

Data/ contains all data used in the individual size distribution analyses - each study plot is given its own .csv file

Data/CrossCom/ contains all data used in the cross-community analyses - compiled by Deng et al.

Figures/ contains the raw figures from the original submission - Figure 2 was composed from multiple figures generated in R.

Rcode/ contains function definition files upon which the analysis scripts depend.
