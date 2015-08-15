#################################################
# O'Leary et al. 2015 
# Bright spots of hope: resilience of nearshore
# marine ecosystems to climatic impacts

# Figure 4
# Factors promoting and preventing resilience
# Author: Robin Elahi
# Date: 150707
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

# load source data
source("./R/process_expert_survey.R")

###############
# Decision tree
# Factors promoting resilience - expert examples
# Start with 97 rows (respondents)
# Remove 30 respondents who did not observe resilience; 67 rows
# Remove 3 respondents who did not give a factor1 for resilience; 64 rows
# Combined two listed factors; 81 rows
# Removed non-climate disturbances; 71 rows

# filter studies that did not show resilience; select and rename relevant columns
unique(dat$Resilience)
dat2 <- filter(dat, Resilience == "Yes") %>%
  select(ecosystem, Length_StrongResilience, ResilienceFactorWriteIn1,
         ResilienceFactorWriteIn2, Disturbance_StrongReslience) %>%
  rename(factor1 = ResilienceFactorWriteIn1, 
         factor2 = ResilienceFactorWriteIn2, disturbLength1 = Length_StrongResilience)

summary(dat2)

# remove studies without a factor1
dat3 <- dat2 %>% filter(factor1 != "")

# create new factor column
factorList <- unique(dat3$factor1)
factorList
factorList2 <- c("Other", "Recruitment or connectivity", 
                 "Remaining biogenic habitat", "Functional diversity",
                 "Physical setting", "Remoteness",
                 "Species interactions")
factorList2
factorNew <- mapvalues(dat3$factor1, from = factorList, to = factorList2)

dat3 <- cbind(factorNew, dat3)
names(dat3)

dat4 <- dat3 %>% select(factorNew, ecosystem, factor2, Disturbance_StrongReslience) %>%
  rename(factor1 = factorNew)

head(dat4)

# I need to get factor 1 and factor 2 in the same column
factorList <- unique(dat4$factor2)
factorList
factorList2 <- c("NA", "Species interactions",
                 "Remoteness","Recruitment or connectivity", 
                 "Management", "Remaining biogenic habitat",
                  "Functional diversity")
factorList2
factor2New <- mapvalues(dat4$factor2, from = factorList, to = factorList2)
dat5 <- cbind(factor2New, dat4)

datF1 <- data.frame(ecosystem = dat5$ecosystem, factorAll = dat5$factor1, 
                    disturbance = dat5$Disturbance_StrongReslience)
datF2 <- data.frame(ecosystem = dat5$ecosystem, factorAll = dat5$factor2New, 
                    disturbance = dat5$Disturbance_StrongReslience)

head(datF1)

datL <- droplevels(rbind(datF1, datF2[datF2$factorAll != "NA", ]))
datL

# now filter to relevant climate criteria
unique(datL$disturbance)

datSub <- datL %>% filter(disturbance == "Storms" |
                           disturbance == "Temperature" |
                           disturbance == "ENSO (temperature/storms)" | 
                           disturbance == "Multiple" |
                           disturbance == "Bleaching" |
                           disturbance == "Sea level rise/ Hydrodynamic change")
datSub <- droplevels(datSub)

### GENERALIZED SCRIPT TO GET PERCENTAGES
tbl1 <- datSub
tbl2 <- ddply(tbl1, .(ecosystem, factorAll), summarise, 
              freq = length(ecosystem), .drop = FALSE) # frequency, I want %
tbl2

n <- with(tbl1, table(ecosystem))
str(n)
n <- as.data.frame(n)
n

tbl3 <- merge(tbl2, n)
tbl3$per <- with(tbl3, freq/Freq)
str(tbl3)

qplot(factorAll, per, data = tbl3, geom = "boxplot") + coord_flip()


detach("package:dplyr", unload = TRUE)

examples <- summarySE(tbl3, measurevar = "per", groupvars = "factorAll")
examples

# get the frequency values for plotting along the y-axis
totalN <- as.data.frame(with(datL, table(factorAll)))
totalN
totalN <- totalN[with(totalN, order(Freq, factorAll)), ]

# now to add 0's
frequency <- totalN$Freq
frequency <- c(0, frequency)
frequency

# add genetic diversity row
gdRow <- data.frame(factorAll = "Genetic diversity", N = 0, per = 0, sd = 0, se = 0, ci = 0)
examples <- rbind(examples, gdRow)
examples

# how many yes responses for each habitat?
with(tbl1, table(ecosystem))
sum(with(tbl1, table(ecosystem)))
# 81 total responses, 71 for the climate disturbances

# custom order of factors
newFactorOrder <- rev(c("Remaining biogenic habitat", "Recruitment or connectivity", 
                        "Physical setting", "Functional diversity", "Species interactions", 
                        "Remoteness", "Management", "Genetic diversity", "Other"))

examples$factor2 <- factor(examples$factorAll, levels = newFactorOrder)
examples

# Reorder based on max to min, except for other

ULClabel <- theme(plot.title = element_text(hjust = -0.1, vjust = 0, size = rel(1.5)))

panelA <- ggplot(examples, aes(x = factor2, y = per)) + 
	theme_classic(base_size = 12) + xlab("Factors\npromoting resilience") + 
  ylab("Proportion") + 
	geom_errorbar(aes(ymin = per, ymax = per + ci), 
	width = 0, color = "black") + 
	coord_flip() + geom_bar(fill = "darkgray", color = "black", stat = "identity") + 
	labs(title = "A") + ULClabel + 
	scale_y_continuous(limits = c(0, 0.6)) +
	geom_text(label = "Expert examples", x = 1, y = 0.42, size = 4) 

panelA

################################
################################
# EXPERT OPINIONS
################################
################################
# use calculated summary data from Jen
dat <- read.csv("./data/resOpinions.csv", header=TRUE, na.strings="NA")

# create new ecosystem column (with easier names)
ecoList <- unique(dat$ecosystem)
ecoList
str(ecoList)
levels(ecoList)
ecoList2 <- c("Algal forests", "Coral reefs", "Mangroves", "Oyster reefs", "Salt marshes", "Seagrasses")
ecosystemNew <- mapvalues(dat$ecosystem, from = ecoList, to = ecoList2)
ecosystemNew 

# create new factor column
factorList <- unique(dat$factor1)
factorList
factorList2 <- c("Recruitment or connectivity", "Species interactions", "Physical setting", 
                 "Remaining biogenic habitat", "Genetic diversity", "Functional diversity", 
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

ULClabel <- theme(plot.title = element_text(hjust = -0.1, vjust = 0, size = rel(1.5)))

# Reorder based on expert examples, above
# custom order of factors
opinions$factor2 <- factor(opinions$factor1, levels = newFactorOrder)
opinions
	
panelB <- ggplot(opinions, aes(x = factor2, y = proportion, fill = resilCat)) +
	theme_classic(base_size = 12) + xlab("") + ylab("Proportion") + 
	coord_flip() + 
	geom_errorbar(aes(ymin = proportion, ymax = proportion + ci), 
	width = 0, color = "black", position = position_dodge(0.8)) + 
	labs(title = "B") + ULClabel +
	geom_text(label = "Expert opinions", x = 1, y = 0.8, size = 4) +
	theme(legend.justification = c(1,0), legend.position = c(1, 0.1)) +
	theme(legend.title = element_blank()) +
	geom_bar(stat = "identity", position = position_dodge(0.8), 
	color = "black", width = 0.8) +
	scale_fill_manual(values = c("darkgray", "white")) +
	guides(fill = guide_legend(reverse = TRUE)) + # reverses the legend
	theme(axis.text.y = element_blank())
panelB

################################
################################
# EXPERT PAPERS
################################
################################

# load source data
library(dplyr)
source("./R/process_expert_papers.R")

# Filtering steps
# 98 papers to start
# Remove papers without evidence of resilience; 75
# Remove rows that are blank for promoteFactor1; 55
# Combine columns for factor1 and factor2; 78
# Select habitat formers; 68
# Select climate disturbances; 50 remaining

summary(dat2)

# select relevant columns and rename
dat3 <- dat2 %>% select(ecosystemNew, ResilienceOutcome, ResilienceResponse,
                        MostImportantFactor1, MostImportFactor2, 
                        DisturbType1, DisturbType2,
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

# Remove rows that are blank for promoteFactor1
paperDF3 <- paperDF2 %>% filter(promoteFactor1 != "")
names(paperDF3)

f1DF <- with(paperDF3, data.frame(ecosystem = ecosystem, 
                                  ResilienceResponse = ResilienceResponse, 
                                  DisturbType1 = DisturbType1, 
                                  promoteFactor = promoteFactor1))

f2DF <- with(paperDF3, data.frame(ecosystem = ecosystem, 
                                  ResilienceResponse = ResilienceResponse, 
                                  DisturbType1 = DisturbType1, 
                                  promoteFactor = promoteFactor2))

promDF <- rbind(f1DF, f2DF)

promDF2 <- promDF %>% filter(promoteFactor != "")
promDF2 <- droplevels(promDF2)

# create new factor column
factorList <- unique(promDF2$promoteFactor)
factorList
factorList2 <- c("Recruitment or connectivity", "Remaining biogenic habitat", 
                "Other", "Physical setting", 
                "Management", "Remoteness", 
                "Species interactions", "Functional diversity",
                "Genetic diversity", "Other")
factorList2
promDF2$factorNew <- mapvalues(promDF2$promoteFactor, from = factorList, to = factorList2)

# promDF2 is the full dataset; now filter to relevant criteria
promDF3 <- promDF2 %>% filter(ResilienceResponse == "HabitatFormingSpp" | 
                                ResilienceResponse == "WholeCommunity")

promDF4 <- promDF3 %>% filter(DisturbType1 == "Storms" |
                                DisturbType1 == "Temperature" |
                                DisturbType1 == "ENSO" | 
                                DisturbType1 == ">2 stressors" |
                                DisturbType1 == "Bleaching" |
                                DisturbType1 == "Sea level rise/hydrodymic change")

# Now get frequencies for the filtered dataset
tbl2 <- ddply(promDF4, .(ecosystem, factorNew), summarise, 
              freq = length(ecosystem), .drop = FALSE) # frequency, I want %
tbl2

n <- with(promDF4, table(ecosystem))
str(n)
n <- as.data.frame(n)
n

merge(tbl2, n)
tbl3 <- merge(tbl2, n)
tbl3$prop <- with(tbl3, freq/Freq)
tbl3

detach("package:dplyr", unload = TRUE)
papers <- summarySE(tbl3, measurevar = "prop", groupvars = "factorNew", na.rm = TRUE)
papers

ULClabel <- theme(plot.title = element_text(hjust = -0.07, vjust = 0, size = rel(1.5)))

# get the frequency values for plotting along the y-axis
totalN <- as.data.frame(with(promDF4, table(factorNew)))
totalN
totalN <- totalN[with(totalN, order(Freq, factorNew)), ]

# Reorder based on expert examples, above
# custom order of factors
newFactorOrder
papers$factor2 <- factor(papers$factorNew, levels = newFactorOrder)
papers

# Reorder based on max to min
panelC <- ggplot(papers, aes(x = factor2, y = prop)) + 
	theme_classic(base_size = 12) + xlab("") + ylab("Proportion") + 
	geom_errorbar(aes(ymin = prop, ymax = prop + ci), 
	width = 0, color = "black") + 
	coord_flip() + geom_bar(fill = "darkgray", color = "black", stat = "identity") + 
	labs(title = "C") + ULClabel + 
	scale_y_continuous(limits = c(0, 0.42)) +
	geom_text(label = "Literature examples", x = 3, y = 0.32, size = 4) +
	theme(axis.text.y = element_blank())

panelC

################################
################################
# EXPERT OPINIONS - PREVENTING RES
################################
################################
dat <- read.csv("./data/bsSurveyResults_140527.csv", header=TRUE, na.strings="NA")
str(dat)
names(dat)
dim(dat)

# create new ecosystem column (with easier names)
ecoList <- unique(dat$Ecosystem)
ecoList
str(ecoList)
levels(ecoList)
ecoList2 <- c("Algal forests", "Coral reefs", "Mangroves", "Oyster reefs", "Salt marshes", "Seagrasses")
dat$ecosystemNew <- mapvalues(dat$Ecosystem, from = ecoList, to = ecoList2)

names(dat)
# columns for preventing resilience 37 - 50
dat2 <- dat[, c(2, 3, 5, 37:42, 44:49, 51)]
names(dat2)
head(dat2)
summary(dat2)

# want to count up the number of very important responses for each column by ecosystemNew
tbl2 <- ddply(dat2, .(ecosystemNew, DRes_SpacePrem), summarise, 
              freq = length(ecosystemNew), .drop = FALSE) # frequency, I want %
tbl2
tbl2[, 3]

tbl3 <- ddply(dat2, .(ecosystemNew), summarise, freq = length(ecosystemNew), .drop = FALSE)
tbl3

dat2[, "DRes_LocalAnthro"]

# create list of the factors preventing resilience
factorList <- names(dat2[4:15])
factorList

# generalized call to get the frequency of responses for each ecosystem
outputN <- ddply(dat2, .(ecosystemNew, dat2[, factorList[1]]), 
	summarise, freq = length(ecosystemNew), .drop = FALSE) 
names(outputN)[2] <- "category"
# how can i get the total number of responses, to calculate %?
outputTotal <- ddply(dat2, .(ecosystemNew), 
	summarise, freq = length(ecosystemNew), .drop = FALSE) 
	
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
outputTotal <- ddply(dat2, .(ecosystemNew), 
	summarise, freq = length(ecosystemNew), .drop = FALSE) 
outputTotalFreq <- outputTotal$freq


for (x in 1:N) {
	factorCol <- factorList[x]
	outputN <- ddply(dat2, .(ecosystemNew, dat2[, factorCol]), 
		summarise, freq = length(ecosystemNew), .drop = FALSE)	
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

factorList2 <- rep(c("Space preemption", "Chronic biotic stressors", "Local stressors", "Global stressors", "Lack of management", "Other"), 2)
factorList2

longVI$factor <- mapvalues(longVI$variable, from = factorList, to = factorList2)

head(longVI)

prevSummary <- summarySE(longVI, measurevar = "value", 
                         groupvars = c("factor", "resilience"))

#custom factor order
prevFactorOrder <- rev(c("Local stressors", "Chronic biotic stressors", 
                         "Space preemption", "Global stressors", "Lack of management", 
                         "Other"))

prevSummary$factor2 <- factor(prevSummary$factor, levels = prevFactorOrder)

######

ULClabel <- theme(plot.title = element_text(hjust = -0.1, vjust = 0, size = rel(1.5)))

panelD <- ggplot(prevSummary, aes(x = factor2, y = value, 
	fill = resilience)) +
	theme_classic(base_size = 12) + xlab("Factors\npreventing resilience") + 
  ylab("Proportion") + coord_flip() + 
	geom_errorbar(aes(ymin = value, ymax = value + ci), 
	width = 0, color = "black", position = position_dodge(0.8)) + 
	labs(title = "D") + ULClabel +
	geom_text(label = "Expert opinions", x = 0.8, y = 0.8, size = 4) +
	theme(legend.justification = c(1,0), legend.position = c(1, 0.1)) +
	theme(legend.title = element_blank()) +
	geom_bar(stat = "identity", position = position_dodge(0.8), 
	color = "black", width = 0.8) +
	scale_fill_manual(values = c("darkgray", "white")) +
	guides(fill = guide_legend(reverse = TRUE))  +
	scale_x_discrete(labels = rev(c("Local anthropogenic\nstressors", 
	                                "Local biotic\nstressors", "Space\npreemption", 
	                                "Additional global\nstressors", "Lack of\nmanagement", 
	                                "Other"))) +
	scale_y_continuous(limits = c(0, 1))
panelD

################################
################################
# LITERATURE - PREVENTING RES
################################
################################
summary(paperDF2)

# Remove rows that are blank for preventFactor1
paperDF3 <- paperDF2 %>% filter(preventFactor1 != "")
names(paperDF3)

f1DF <- with(paperDF3, data.frame(ecosystem = ecosystem, 
                                  ResilienceResponse = ResilienceResponse, 
                                  DisturbType1 = DisturbType1, 
                                  prevFactor = preventFactor1))

f2DF <- with(paperDF3, data.frame(ecosystem = ecosystem, 
                                  ResilienceResponse = ResilienceResponse, 
                                  DisturbType1 = DisturbType1, 
                                  prevFactor = preventFactor2))

prevDF <- rbind(f1DF, f2DF)
summary(prevDF)

library(dplyr)
prevDF2 <- prevDF %>% filter(prevFactor != "")
prevDF2 <- droplevels(prevDF2)

# create new factor column
factorList <- unique(prevDF2$prevFactor)
factorList

factorList2 <- c("Global stressors", "Other", "Chronic biotic stressors", 
                 "Space preemption", "Local stressors", "Lack of management", 
                 "Other", "Lack of management", "Local stressors", "Local stressors", "Local stressors")
  
factorList2
prevDF2$factorNew <- mapvalues(prevDF2$prevFactor, from = factorList, to = factorList2)

# prevDF2 is the full dataset; now filter to relevant criteria
prevDF3 <- prevDF2 %>% filter(ResilienceResponse == "HabitatFormingSpp" | 
                                ResilienceResponse == "WholeCommunity")
prevDF4 <- prevDF3 %>% filter(DisturbType1 == "Storms" |
                                DisturbType1 == "Temperature" |
                                DisturbType1 == "ENSO" | 
                                DisturbType1 == ">2 stressors" |
                                DisturbType1 == "Bleaching" |
                                DisturbType1 == "Sea level rise/hydrodymic change")
summary(prevDF4)

# Now get frequencies for the filtered dataset
tbl2 <- ddply(prevDF4, .(ecosystem, factorNew), summarise, 
              freq = length(ecosystem), .drop = FALSE) # frequency, I want %
tbl2

n <- with(prevDF4, table(ecosystem))
str(n)
n <- as.data.frame(n)
n

merge(tbl2, n)
tbl3 <- merge(tbl2, n)
tbl3$prop <- with(tbl3, freq/Freq)
tbl3

detach("package:dplyr", unload = TRUE)
prevPapers <- summarySE(tbl3, measurevar = "prop", groupvars = "factorNew", na.rm = TRUE)
prevPapers

ULClabel <- theme(plot.title = element_text(hjust = -0.07, vjust = 0, size = rel(1.5)))

# get the frequency values for plotting along the y-axis
totalN <- as.data.frame(with(prevDF4, table(factorNew)))
totalN
totalN <- totalN[with(totalN, order(Freq, factorNew)), ]


#custom factor order
prevFactorOrder <- rev(c("Local stressors", "Chronic biotic stressors", 
                         "Space preemption", "Global stressors", "Lack of management", 
                         "Other"))

prevPapers$factor <- factor(prevPapers$factorNew, levels = prevFactorOrder)

######

ULClabel <- theme(plot.title = element_text(hjust = -0.07, vjust = 0, size = rel(1.5)))
	
panelE <- ggplot(prevPapers, aes(x = factor, y = prop)) +
	theme_classic(base_size = 12) + xlab("") + 
  ylab("Proportion") + coord_flip() + 
	geom_errorbar(aes(ymin = prop, ymax = prop + ci), 
	width = 0, color = "black", position = position_dodge(0.8)) + 
	labs(title = "E") + ULClabel +
	geom_text(label = "Literature examples", x = 2, y = 0.5, size = 4) +
	geom_bar(stat = "identity", 
	fill = "darkgray", color = "black", width = 0.8) +
	theme(axis.text.y = element_blank()) + 
	scale_y_continuous(limits = c(0, 0.85))

panelE

### with blank panel F
panelF <- ggplot(prevPapers, aes(x = factor, y = prop)) +
	theme_minimal(base_size = 12) + xlab("") + ylab("") + 
	geom_blank() + 
	theme(axis.ticks = element_blank(), axis.text = element_blank()) + 
	theme(panel.grid = element_blank()) 

### different layout
ULClabel <- theme(plot.title = element_text(hjust = -0.1, vjust = 0, size = rel(1.5)))
panelB2 <- ggplot(opinions, aes(x = factor2, y = proportion, fill = resilCat)) +
	theme_classic(base_size = 12) + xlab("") + ylab("Proportion") + 
	coord_flip() + 
	geom_errorbar(aes(ymin = proportion, ymax = proportion + ci), 
	width = 0, color = "black", position = position_dodge(0.8)) + 
	labs(title = "B") + ULClabel +
	geom_text(label = "Expert opinions", x = 1, y = 0.8, size = 4) +
	theme(legend.justification = c(1,0), legend.position = c(1, 0.1)) +
	theme(legend.title = element_blank()) +
	geom_bar(stat = "identity", position = position_dodge(0.8), 
	color = "black", width = 0.8) +
	scale_fill_manual(values = c("darkgray", "white")) +
	guides(fill = guide_legend(reverse = TRUE)) +
  theme(axis.text.y = element_blank()) 
  
panelB2

multiplot(panelA, panelB2, panelC, panelF, panelD, panelE, 
	layout = matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, byrow = TRUE))


###############################
# save as pdf
pdf("./figs/BS_Fig4.pdf", 14, 7)
multiplot(panelA, panelB2, panelC, panelF, panelD, panelE, 
          layout = matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, byrow = TRUE))
dev.off()
