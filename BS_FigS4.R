#################################################
# O'Leary et al. 2015 
# Bright spots of hope: resilience of nearshore
# marine ecosystems to climatic impacts

# Figure S4
# Disturbance lengths for expert examples and literature
# Author: Robin Elahi
# Date: 150706
#################################################
rm(list=ls(all=TRUE)) # removes all previous material from R's memory

# load packages
library(plyr)
library(dplyr)
library(reshape2)
library(ggplot2)

# load source functions
source("./R/summarizeData_150204.R")
source("./R/multiplotF.R")

################################
################################
# EXPERT EXAMPLES
################################
################################

# Filtering steps
# 97 respondents to start; removed 30 that did not observe resilience
# 67; remove 7 that did not have a disturbance length response
# 60; remove 8 that were not climate-related
# 52 remaining

# load source data
source("./R/process_expert_survey.R")
names(dat)
summary(dat)

# filter respondents that did not observe resilience
# select and rename relevant columns
unique(dat$Resilience)
dat2 <- filter(dat, Resilience == "Yes") %>%
  select(ecosystem, Experience, 
         Disturbance_StrongReslience, Length_StrongResilience, 
         ResilienceFactorWriteIn1, ResilienceFactorWriteIn2) %>%
  rename(disturbance = Disturbance_StrongReslience, 
         distLength = Length_StrongResilience, 
         factor1 = ResilienceFactorWriteIn1, 
         factor2 = ResilienceFactorWriteIn2)

summary(dat2)

# remove studies without a disturbLength
dat3 <- dat2 %>% filter(distLength != "")

# create new disturbLength column (with easier names)
lengthList <- unique(dat3$distLength)
lengthList

lengthList2 <- c("Months", ">100 years", "1-5 years", 
                 "6-10 years", "1-4 weeks", "Multiple scales", 
                 "Hours-days", "Ongoing", "Decades")
  
dat3$length <- mapvalues(dat3$distLength, from = lengthList, 
                         to = lengthList2)
unique(dat3$length)

dat3 <- droplevels(dat3)
names(dat3)
unique(dat3$disturbance)

# now filter to relevant climate criteria
dat4 <- dat3 %>% filter(disturbance == "Storms" |
                              disturbance == "Temperature" |
                              disturbance == "ENSO (temperature/storms)" | 
                              disturbance == "Multiple" |
                              disturbance == "Bleaching" |
                              disturbance == "Sea level rise/ Hydrodynamic change")
dat4 <- droplevels(dat4)
unique(dat4$disturbance)

# generalized call to get the frequency of responses for each length
outputN <- ddply(dat4, .(length), 
                 summarise, freq = length(ecosystem), .drop = FALSE) 
outputN	

outputN$per <- outputN$freq/sum(outputN$freq)	

# ongoingRow <- data.frame(length = "Ongoing", freq = 0, per = 0)
# lengthDat <- rbind(outputN, ongoingRow)
lengthDat <- outputN

#custom factor order
lengthOrder <- rev(c("Multiple scales", "Ongoing", ">100 years", 
                     "Decades", "6-10 years", "1-5 years", 
                     "Months", "1-4 weeks", "Hours-days"))

lengthDat$length2 <- factor(lengthDat$length, levels = lengthOrder)
lengthDat

######
ULClabel <- theme(plot.title = element_text(hjust = -0.1, vjust = 0, 
                                            size = rel(1.5)))

panelA <- ggplot(lengthDat, aes(x = length2, y = per)) +
  theme_classic(base_size = 12) + xlab("") + ylab("Proportion") + 
  coord_flip() + 
  geom_bar(stat = "identity", position = position_dodge(0.8), 
           color = "black", fill = "darkgray", width = 0.8) +
  labs(title = "A") + ULClabel + 
  geom_text(label = "Expert examples", x = 9, y = 0.17, size = 4) + 
  scale_y_continuous(limits = c(0, 0.35))
panelA
sum(outputN$freq) 

# 52 rows - 
# resilience yes, habitat forming spp yes, climate disturbance yes	

################################
################################
# LITERATURE 
################################
################################
# Filtering steps
# 98 papers to start
# Remove papers without evidence of resilience; 75
# Remove rows that are blank for DisturbLength1; 60
# combine disturbLength 1 and 2; 75 
# Select habitat formers; 60
# Select climate disturbances; 45

# load source data
library(dplyr)
source("./R/process_expert_papers.R")

summary(dat2)

# select relevant columns and rename
dat3 <- dat2 %>% select(ecosystemNew, ResilienceOutcome, ResilienceResponse,
                        MostImportantFactor1, MostImportFactor2, 
                        DisturbType1, DisturbType2,
                        DisturbLength1, DisturbLength2, 
                        FactorsPreventingResilience1, FactorsPreventingResilience2) %>%
  rename(ecosystem = ecosystemNew, 
         promoteFactor1 = MostImportantFactor1, 
         promoteFactor2 = MostImportFactor2, 
         preventFactor1 = FactorsPreventingResilience1, 
         preventFactor2 = FactorsPreventingResilience2)

dat3 <- droplevels(dat3)
summary(dat3)

paperDF <- dat3

# Remove papers without evidence of resilience
paperDF2 <- paperDF %>% filter(ResilienceOutcome != "No")
summary(paperDF2)

# Remove rows that are blank for DisturbLength1
paperDF3 <- paperDF2 %>% filter(DisturbLength1 != "")
summary(paperDF3)

# combine disturbLength 1 and 2
f1DF <- with(paperDF3, data.frame(ecosystem = ecosystem, 
                                  ResilienceResponse = ResilienceResponse, 
                                  disturbType = DisturbType1, 
                                  disturbLength = DisturbLength1))

f2DF <- with(paperDF3, data.frame(ecosystem = ecosystem, 
                                  ResilienceResponse = ResilienceResponse, 
                                  disturbType = DisturbType2, 
                                  disturbLength = DisturbLength2))

lenDF <- rbind(f1DF, f2DF)

lenDF2 <- lenDF %>% filter(disturbLength != "" &
                             disturbLength != "N/G")

lenDF2 <- droplevels(lenDF2)
unique(lenDF2$disturbLength)
summary(lenDF2)

# create new disturbLength column (with easier names)
lengthList <- unique(lenDF2$disturbLength)
lengthList

lengthList2 <- c("Ongoing", "1-5 years", 
                 "Hours-days", "1-4 weeks", 
                 "Multiple scales", "Decades", 
                 "Months", "Ongoing", 
                 "Decades", "Multiple scales", 
                 ">100 years")

lenDF2$length <- mapvalues(lenDF2$disturbLength, 
                           from = lengthList, to = lengthList2)
unique(lenDF2$length)
head(lenDF2)

# lenDF2 is the full dataset; now filter to relevant criteria
lenDF3 <- lenDF2 %>% filter(ResilienceResponse == "HabitatFormingSpp" | 
                                ResilienceResponse == "WholeCommunity")

lenDF4 <- lenDF3 %>% filter(disturbType == "Storms" |
                                disturbType == "Temperature" |
                                disturbType == "ENSO" | 
                                disturbType == ">2 stressors" |
                                disturbType == "Bleaching" |
                                disturbType == "Sea level rise/hydrodymic change")
summary(lenDF4)

# what is behind the many ongoing disturbance lengths?
lenDF4 %>% filter(disturbLength == "Ongoing")
# Storms, temperature, >2 stressors

# generalized call to get the frequency of responses for each length
outputN <- ddply(lenDF4, .(length), 
	summarise, freq = length(ecosystem), .drop = FALSE) 
outputN	

outputN$per <- outputN$freq/sum(outputN$freq)	

#decadesRow <- data.frame(length = "Decades", freq = 0, per = 0)
sixTenRow <- data.frame(length = "6-10 years", freq = 0, per = 0)

lengthDat <- rbind(outputN, sixTenRow)
lengthDat

#custom factor order
lengthOrder <- rev(c("Multiple scales", "Ongoing", ">100 years", 
								"Decades", "6-10 years", "1-5 years", 
								"Months", "1-4 weeks", "Hours-days"))

lengthDat$length2 <- factor(lengthDat$length, levels = lengthOrder)
lengthDat

######
ULClabel <- theme(plot.title = element_text(hjust = -0.1, vjust = 0, 
                                            size = rel(1.5)))

panelB <- ggplot(lengthDat, aes(x = length2, y = per)) +
  theme_classic(base_size = 12) + xlab("") + ylab("Proportion") + 
  coord_flip() + 
  geom_bar(stat = "identity", position = position_dodge(0.8), 
           color = "black", fill = "darkgray", width = 0.8) +
  labs(title = "B") + ULClabel + 
  geom_text(label = "Literature examples", x = 9, y = 0.2, size = 4) + 
  theme(axis.text.y = element_blank()) + 
  scale_y_continuous(limits = c(0, 0.35))
panelB

sum(outputN$freq) 
# 45 total paper examples

###############################

multiplot(panelA, panelB, cols = 2)

# save as pdf
pdf("./figs/BS_FigS4.pdf", 7, 3.5)
multiplot(panelA, panelB, cols = 2)
dev.off()





