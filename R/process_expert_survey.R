#################################################
# O'Leary et al. 2015 
# Bright spots of hope: resilience of nearshore
# marine ecosystems to climatic impacts

# Decision tree for expert examples
# Author: Robin Elahi
# Date: 150701
#################################################
# CHANGE LOG
# Updated survey results file

# load packages
library(plyr)
library(dplyr)

# load data
dat <- read.csv("./data/bsSurveyResults_140527_JO_Oct1.csv", header=TRUE, 
                na.strings="NA")
names(dat)

# create new ecosystem column (with easier names)
ecoList <- unique(dat$Ecosystem)
ecoList
str(ecoList)
levels(ecoList)
ecoList2 <- c("Algal forests", "Coral reefs", "Mangroves", "Oyster reefs", 
              "Salt marshes", "Seagrasses")
dat$ecosystem <- mapvalues(dat$Ecosystem, from = ecoList, to = ecoList2)




