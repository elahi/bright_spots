#################################################
# O'Leary et al. 2015 
# Bright spots of hope: resilience of nearshore
# marine ecosystems to climatic impacts

# Decision tree for expert examples
# Author: Robin Elahi
# Date: 150701
#################################################
# CHANGE LOG
# 18 Nov 2015
# Updated survey results file

# load data
dat <- read.csv("./data/bsSurveyResults_140527_JO_Oct17.csv", header=TRUE, 
                na.strings="NA")
names(dat)

unique(dat$Ecosystem)

# create new ecosystem column (with easier names)
# Use regular expressions to simplify Algal forests name
dat$Ecosystem
grepl("Algal forests (including kelps and fucoids)", fixed = TRUE, dat$Ecosystem)

dat$ecosystem <- gsub("Algal forests (including kelps and fucoids)", 
    "Algal forests", fixed = TRUE, dat$Ecosystem)

unique(dat$ecosystem)
