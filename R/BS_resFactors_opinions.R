#################################################
# O'Leary et al. 2015 
# Bright spots of hope: resilience of nearshore
# marine ecosystems to climatic impacts

# Prepare expert opinions
# (to plot factors that promote and prevent resilience)
# Author: Robin Elahi
# Date: 151211
#################################################

##### LOAD PACKAGES, SOURCE DATA AND FUNCTIONS #####

### Load in this order
library(plyr)
source("./R/summarizeData.R")

library(dplyr)
library(reshape2)
library(ggplot2)

################################
##### PROMOTING RESILIENCE #####
################################
# use calculated summary data from Jen
dat <- read.csv("./data/resOpinions.csv", header=TRUE, na.strings="NA")

# create new ecosystem column (with easier names)
ecoList <- unique(dat$ecosystem)
ecoList
str(ecoList)
levels(ecoList)
ecoList2 <- c("Algal forests", "Coral reefs", "Mangroves", "Oyster reefs", 
              "Salt marshes", "Seagrasses")
ecosystemNew <- mapvalues(dat$ecosystem, from = ecoList, to = ecoList2)
ecosystemNew 

# create new factor column
factorList <- unique(dat$factor1)
factorList
factorList2 <- c("Recruitment or connectivity", "Species interactions", 
                 "Physical setting", "Remaining biogenic habitat", 
                 "Genetic diversity", "Functional diversity", 
                 "Remoteness", "Management", "Other")

factorList2
factorNew <- mapvalues(dat$factor1, from = factorList, to = factorList2)
factorNew

dat3 <- cbind(ecosystemNew, factorNew, dat)
names(dat3)

# drop duplicate columns & unnecessary columns
drops <- c("ecosystem", "factor1")
dat4 <- dat3[, !(names(dat3) %in% drops)]
head(dat4)
colnames(dat4)[1:2] <- c("ecosystem", "factor1")
names(dat4)


##### CALCULATE MEANS AND ERROR #####
detach("package:dplyr", unload = TRUE)

resProp <- summarySE(dat4, measurevar = "resistProp", groupvars = "factor1")
recProp <- summarySE(dat4, measurevar = "recovProp", groupvars = "factor1")
colnames(resProp)[3] <- "proportion"
colnames(recProp)[3] <- "proportion"
names(resProp)

# combine dataframes
resilCat <- c(rep("resistance", 9), rep("recovery", 9))
resilCat
opinions <- rbind(resProp, recProp)
opinions$resilCat <- resilCat
opinions

##### PREPARE FOR PLOTTING #####
# Reorder based on expert examples, above
# custom order of factors
opinions$factor2 <- factor(opinions$factor1, levels = newFactorOrder)
opinions

################################
##### PREVENTING RESILIENCE #####
################################
source("./R/process_expert_survey.R")

names(dat)
# columns for preventing resilience 37 - 50
dat2 <- dat[, c(2, 3, 5, 37:42, 44:49, 51)]

# want to count up the number of very important responses for each column by ecosystemNew
tbl2 <- ddply(dat2, .(ecosystem, DRes_SpacePrem), summarise, 
              freq = length(ecosystemNew), .drop = FALSE) # frequency, I want %
tbl2
tbl2[, 3]

tbl3 <- ddply(dat2, .(ecosystemNew), summarise, freq = length(ecosystemNew), .drop = FALSE)
tbl3

dat2[, "DRes_LocalAnthro"]

# create list of the factors preventing resilience
names(dat2)
factorList <- names(dat2[4:15])
factorList

# generalized call to get the frequency of responses for each ecosystem
outputN <- ddply(dat2, .(ecosystem, dat2[, factorList[1]]), 
                 summarise, freq = length(ecosystem), .drop = FALSE) 

names(outputN)[2] <- "category"
# how can i get the total number of responses, to calculate %?
outputTotal <- ddply(dat2, .(ecosystem), 
                     summarise, freq = length(ecosystem), .drop = FALSE) 

outputN	
freq <- outputN[outputN$category == "Very Important", ]$freq
outputTotal	
mean(freq/outputTotal$freq)

# 	

factorList[3]
dat2[, factorList[3]]

names(dat2)

N <- length(factorList)
N
emptyMat <- matrix(nrow = 6, ncol = N)
emptyMat
outputTotal <- ddply(dat2, .(ecosystem), 
                     summarise, freq = length(ecosystem), .drop = FALSE) 
outputTotalFreq <- outputTotal$freq


for (x in 1:N) {
  factorCol <- factorList[x]
  outputN <- ddply(dat2, .(ecosystem, dat2[, factorCol]), 
                   summarise, freq = length(ecosystem), .drop = FALSE)	
  names(outputN)[2] <- "category"
  freq <- outputN[outputN$category == "Very Important", ]$freq	 	
  emptyMat[, x] <- freq/outputTotalFreq
}

emptyMat
datVI <- data.frame(emptyMat)
names(datVI) <- factorList
datVI

datVI$ecosystem <- ecoList2
datVI

# change to long format
longVI <- melt(data = datVI, id.vars = "ecosystem")
longVI

# resistance/recovery column
longVI$resilience <- c(rep("resistance", 36), rep("recovery", 36))
head(longVI)

# create new factor column (with plottable names)
factorList <- unique(longVI$variable)
factorList

factorList2 <- rep(c("Space preemption", "Chronic biotic stressors", 
                     "Local stressors", "Global stressors", "Lack of management", 
                     "Other"), 2)
factorList2

longVI$factor <- mapvalues(longVI$variable, from = factorList, to = factorList2)

head(longVI)

prevSummary <- summarySE(longVI, measurevar = "value", 
                         groupvars = c("factor", "resilience"))
prevSummary

### Add a row for multiple
# need to change examples column to a string (not factor)
extraRowRecov <- data.frame(factor = "Multiple", resilience = "recovery", 
                            N = 6, value = 0, sd = 0, se = 0, ci = 0)
extraRowResist <- data.frame(factor = "Multiple", resilience = "resistance", 
                             N = 6, value = 0, sd = 0, se = 0, ci = 0)
prevSummary2 <- rbind(prevSummary, extraRowRecov, extraRowResist)
prevSummary2

#custom factor order
prevFactorOrder <- rev(c("Local stressors", "Chronic biotic stressors", 
                         "Space preemption", "Global stressors", "Lack of management",
                         "Multiple", "Other"))

prevSummary2$factor2 <- factor(prevSummary2$factor, levels = prevFactorOrder)
prevSummary2


