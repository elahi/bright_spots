#################################################
# O'Leary et al. 2015 
# Bright spots of hope: resilience of nearshore
# marine ecosystems to climatic impacts

# Prepare expert literature data
# (to plot factors that promote and prevent resilience)
# Author: Robin Elahi
# Date: 151211
#################################################

##### LOAD PACKAGES, SOURCE DATA AND FUNCTIONS #####

library(dplyr)
source("./R/process_expert_papers.R")

summary(dat2)
names(dat2)
levels(dat2$ecosystem)

##### GET RELEVANT ROWS AND COLUMNS #####

# select relevant columns and rename
ls2 <- litSub %>% select(ecosystem, ResilienceOutcome, ResilienceResponse,
                         MostImportantFactor1, MostImportFactor2, 
                         DisturbType1, DisturbType2,
                         FactorsPreventingResilience1, FactorsPreventingResilience2) %>%
  rename(promoteFactor1 = MostImportantFactor1, 
         promoteFactor2 = MostImportFactor2, 
         preventFactor1 = FactorsPreventingResilience1, 
         preventFactor2 = FactorsPreventingResilience2)

ls3 <- droplevels(ls2)
summary(ls3)

paperDF <- ls3

# Do not remove papers without evidence of resilience
# paperDF2 <- paperDF %>% filter(ResilienceOutcome != "No")

################################
##### PROMOTING RESILIENCE #####
################################

##### CHECK FACTORS 1 AND 2 #####
# First need to change NAs to blanks ""
factor1 <- as.character(paperDF$promoteFactor1)
factor2 <- as.character(paperDF$promoteFactor2)
factor1[is.na(factor1)] <- ""
factor2[is.na(factor2)] <- ""

factor1 == "" & factor2 != "" # one mistake
suspect <- ifelse(factor1 == "" & factor2 != "", "bad", "good")

factor1new <- ifelse(suspect == "good", factor1, factor2)
factor2new <- ifelse(suspect != "good", "", factor2)

paperDF3 <- cbind(paperDF, factor1new, factor2new)

##### COMBINE FACTORS 1 AND 2 #####
# Remove rows that are blank for promoteFactor1
paperDF3 <- paperDF3 %>% filter(factor1new != "")
names(paperDF3)

f1DF <- with(paperDF3, data.frame(ecosystem = ecosystem, 
                                  ResilienceResponse = ResilienceResponse, 
                                  DisturbType1 = DisturbType1, 
                                  promoteFactor = factor1new))

f2DF <- with(paperDF3, data.frame(ecosystem = ecosystem, 
                                  ResilienceResponse = ResilienceResponse, 
                                  DisturbType1 = DisturbType1, 
                                  promoteFactor = factor2new))

promDF <- rbind(f1DF, f2DF)

promDF2 <- promDF %>% filter(promoteFactor != "")
promDF2 <- droplevels(promDF2)

# create new factor column
factorList <- unique(promDF2$promoteFactor)
factorList

factorList2 <- c("Remoteness","Recruitment or connectivity",
                 "Functional diversity", "Management", 
                 "Physical setting", "Species interactions", 
                 "Remaining biogenic habitat", "Genetic diversity",  
                 "Other", "Management")
factorList2
promDF2$factorNew <- mapvalues(promDF2$promoteFactor, from = factorList, 
                               to = factorList2)
head(promDF2$promoteFactor)
head(promDF2$factorNew)

levels(promDF2$ResilienceResponse)

##### GENERALIZED SCRIPT TO GET PERCENTAGES #####
### Rename relevant dataframe
tbl1 <- promDF2

tbl2 <- ddply(tbl1, .(ecosystem, factorNew), summarise, 
              freq = length(ecosystem), .drop = FALSE) # frequency, I want %
tbl2

n <- with(tbl1, table(ecosystem))
str(n)
n <- as.data.frame(n)
n

tbl3 <- merge(tbl2, n)
tbl3$prop <- with(tbl3, freq/Freq)
str(tbl3)

qplot(factorNew, prop, data = tbl3, geom = "boxplot") + coord_flip()

##### CALCULATE MEANS AND ERROR #####
detach("package:dplyr", unload = TRUE)

papers <- summarySE(tbl3, measurevar = "prop", groupvars = "factorNew", na.rm = TRUE)
papers

##### PREPARE FOR PLOTTING #####
# Reorder based on expert examples (needs BS_resFactors_examples.R)
# custom order of factors
newFactorOrder
papers$factor2 <- factor(papers$factorNew, levels = newFactorOrder)
papers

################################
##### PREVENTING RESILIENCE ####
################################
head(paperDF)

##### CHECK FACTORS 1 AND 2 #####
# First need to change NAs to blanks ""
factor1 <- as.character(paperDF$preventFactor1)
factor2 <- as.character(paperDF$preventFactor2)
factor1[is.na(factor1)] <- ""
factor2[is.na(factor2)] <- ""

factor1 == "" & factor2 != "" 
suspect <- ifelse(factor1 == "" & factor2 != "", "bad", "good")
suspect

#factor1new <- ifelse(suspect == "good", factor1, factor2)
#factor2new <- ifelse(suspect != "good", "", factor2)
#paperDF3 <- cbind(paperDF2, factor1new, factor2new)

##### COMBINE FACTORS 1 AND 2 #####
# Remove rows that are blank for preventFactor1
library(dplyr)
paperDF %>% filter(preventFactor1 == "") # looks good

f1DF <- with(paperDF, data.frame(ecosystem = ecosystem, 
                                  ResilienceResponse = ResilienceResponse, 
                                  DisturbType1 = DisturbType1, 
                                  prevFactor = preventFactor1))

f2DF <- with(paperDF, data.frame(ecosystem = ecosystem, 
                                  ResilienceResponse = ResilienceResponse, 
                                  DisturbType1 = DisturbType1, 
                                  prevFactor = preventFactor2))

prevDF <- rbind(f1DF, f2DF)
summary(prevDF)

prevDF2 <- prevDF %>% filter(prevFactor != "")
prevDF2 <- droplevels(prevDF2)

# create new factor column
factorList <- unique(prevDF2$prevFactor)
factorList

factorList2 <- c("Local anthropogenic stressors", "Local biotic stressors", 
                 "Global stressors", "Lack of management", 
                 "Space preemption", "Multiple", "Other", 
                  "Lack of management")

factorList2
prevDF2$factorNew <- mapvalues(prevDF2$prevFactor, from = factorList, 
                               to = factorList2)

head(prevDF2$prevFactor)
head(prevDF2$factorNew)

##### GENERALIZED SCRIPT TO GET PERCENTAGES #####
### Rename relevant dataframe
tbl1 <- prevDF2

tbl2 <- ddply(tbl1, .(ecosystem, factorNew), summarise, 
              freq = length(ecosystem), .drop = FALSE) # frequency, I want %
tbl2

n <- with(tbl1, table(ecosystem))
str(n)
n <- as.data.frame(n)
n

tbl3 <- merge(tbl2, n)
tbl3$prop <- with(tbl3, freq/Freq)
str(tbl3)

qplot(factorNew, prop, data = tbl3, geom = "boxplot") + coord_flip()

##### CALCULATE MEANS AND ERROR #####
detach("package:dplyr", unload = TRUE)

prevPapers <- summarySE(tbl3, measurevar = "prop", groupvars = "factorNew", 
                        na.rm = TRUE)
prevPapers

##### PREPARE FOR PLOTTING #####
#custom factor order
levels(prevPapers$factorNew)

prevFactorOrder <- rev(c("Local anthropogenic stressors", "Local biotic stressors", 
                         "Space preemption", "Global stressors", "Lack of management", 
                         "Multiple", "Other"))

prevPapers$factor <- factor(prevPapers$factorNew, levels = prevFactorOrder)


