#################################################
# O'Leary et al. 2015 
# Bright spots of hope: resilience of nearshore
# marine ecosystems to climatic impacts

# Figure 4
# Factors promoting and preventing resilience
# Author: Robin Elahi
# Date: 150707
#################################################

##### CHANGELOG #####
# 151118
# Uploaded new file from Jennifer O'Leary, with corrected expert examples
# Still need to wait for final corrections on expert papers information

### 151210
# Uploaded new file from JO, with final corrections on expert papers

###
rm(list=ls(all=TRUE)) # removes all previous material from R's memory

##### LOAD PACKAGES, DATA #####
# load packages
library(plyr)
library(dplyr)
library(reshape2)
library(ggplot2)

# load source data
source("./R/process_expert_survey.R")
source("./R/summarizeData_150204.R")
source("./R/multiplotF.R")

##### EXPERT EXAMPLES #####
# Get subset of relevant rows
names(dat)

datSub <- dat %>% filter(Disturbance_StrongReslience != "exclude - no disturbance found" &
                           Disturbance_StrongReslience != "non climatic")

# Get subset of relevant columns 
dat2 <- datSub %>%
  select(ecosystem, Length_StrongResilience, ResilienceFactorWriteIn1,
         ResilienceFactorWriteIn2, Disturbance_StrongReslience) %>%
  rename(factor1 = ResilienceFactorWriteIn1, 
         factor2 = ResilienceFactorWriteIn2, disturbLength1 = Length_StrongResilience)

dat2 <- droplevels(dat2)

with(dat2, table(factor1))
with(dat2, table(factor2))

#### Make sure that if factor1 is blank, then factor2 should also be blank
View(dat2)
# If not, then paste factor 2 into space for factor1
with(dat2, factor1 == "" & factor2 != "") # looks ok
dat2$suspect <- ifelse(dat2$factor1 == "" & dat2$factor2 != "", "bad", "good")

# Don't need to use the following
# dat2$factor1 <- with(dat2, ifelse(suspect == "good", 
#                                     as.character(factor1), as.character(factor2)))
####

### remove studies with a blank factor1
dat3 <- dat2 %>% filter(factor1 != "")
dat3 <- droplevels(dat3)
levels(dat3$factor1)

### create new names for factor1 column
factorList <- unique(dat3$factor1)
factorList
factorList2 <- c("Recruitment or connectivity", "Remaining biogenic habitat", 
                 "Functional diversity", "Genetic diversity", 
                 "Species interactions", "Other", 
                 "Physical setting", "Remoteness")
factorList2
factor1New <- mapvalues(dat3$factor1, from = factorList, to = factorList2)
dat3 <- cbind(factor1New, dat3)
names(dat3)

with(dat3, table(factor1New))
with(dat3, table(factor1))

# Subset desired columns
dat4 <- dat3 %>% select(factor1New, ecosystem, factor2, Disturbance_StrongReslience)
head(dat4)

########## create new names for factor2 column
factorList <- unique(dat4$factor2)
factorList
factorList2 <- c("Remaining biogenic habitat", "Genetic diversity", 
                 "Other", "", 
                 "Recruitment or connectivity", 
                 "Physical setting", "Functional diversity")
                 
factorList2
factor2New <- mapvalues(dat4$factor2, from = factorList, to = factorList2)
dat5 <- cbind(factor2New, dat4)
cbind(as.character(factor2New), as.character(dat4$factor2))

########## Get factor 1 and factor 2 in the same column
head(dat5)
datF1 <- data.frame(ecosystem = dat5$ecosystem, factorAll = dat5$factor1New, 
                    disturbance = dat5$Disturbance_StrongReslience)
datF2 <- data.frame(ecosystem = dat5$ecosystem, factorAll = dat5$factor2New, 
                    disturbance = dat5$Disturbance_StrongReslience)

# Remove blanks in datF2$factorAll
datF2 <- datF2 %>% filter(factorAll != "")

# Now combine the two dataframes
datL <- droplevels(rbind(datF1, datF2))
datL
names(datL)
unique(datL$disturbance)

with(datL, table(factorAll))

### GENERALIZED SCRIPT TO GET PERCENTAGES
tbl1 <- datL
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

# get the frequency values for plotting along the y-axis
totalN <- as.data.frame(with(datL, table(factorAll)))
totalN
totalN <- totalN[with(totalN, order(Freq, factorAll)), ]

# now to add 0's
frequency <- totalN$Freq
frequency <- c(0, frequency)
frequency

# how many yes responses for each habitat?
with(tbl1, table(ecosystem))
sum(with(tbl1, table(ecosystem)))

# Get summary data
examples <- summarySE(tbl3, measurevar = "per", groupvars = "factorAll")
examples

### Add a row for management
# need to change examples column to a string (not factor)
extraRow <- data.frame(factorAll = "Management", N = 6, per = 0, sd = 0, 
                       se = 0, ci = 0)
examples <- rbind(examples, extraRow)

# Reorder based on max to min, except for other
unique(examples$factorAll)
newFactorOrder <- rev(c("Recruitment or connectivity", "Remaining biogenic habitat", 
                        "Physical setting","Functional diversity", 
                        "Genetic diversity", "Remoteness", 
                        "Species interactions", "Management",
                        "Other"))

examples$factor2 <- factor(examples$factorAll, levels = newFactorOrder)
examples

### Plot
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
ggsave("./figs/Fig4_panelA.pdf", width = 5, height = 5)

##### EXPERT OPINIONS #####
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

# Reorder based on expert examples, above
# custom order of factors
opinions$factor2 <- factor(opinions$factor1, levels = newFactorOrder)
opinions

ULClabel <- theme(plot.title = element_text(hjust = -0.1, vjust = 0, size = rel(1.5)))

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
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(axis.text.y = element_blank()) 


##### EXPERT PAPERS #####
# load source data
library(dplyr)
source("./R/process_expert_papers.R")

summary(dat2)
names(dat2)
levels(dat2$ecosystem)

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

# Remove papers without evidence of resilience
paperDF2 <- paperDF %>% filter(ResilienceOutcome != "No")

#### Make sure that if factor1 is blank, then factor2 should also be blank ####
View(paperDF2)

# First need to change NAs to blanks ""
factor1 <- as.character(paperDF2$promoteFactor1)
factor2 <- as.character(paperDF2$promoteFactor2)
factor1[is.na(factor1)] <- ""
factor2[is.na(factor2)] <- ""

factor1 == "" & factor2 != "" # one mistake
suspect <- ifelse(factor1 == "" & factor2 != "", "bad", "good")

factor1new <- ifelse(suspect == "good", factor1, factor2)
factor2new <- ifelse(suspect != "good", "", factor2)

paperDF3 <- cbind(paperDF2, factor1new, factor2new)
View(paperDF3)

#####
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
factorList2 <- c("Recruitment or connectivity", "Management", 
                 "Other", "Physical setting", 
                 "Remaining biogenic habitat", "Remoteness", 
                "Species interactions", "Functional diversity",
                "Genetic diversity", "Management")
factorList2
promDF2$factorNew <- mapvalues(promDF2$promoteFactor, from = factorList, 
                               to = factorList2)
levels(promDF2$ResilienceResponse)

# Now get frequencies for the filtered dataset
tbl2 <- ddply(promDF2, .(ecosystem, factorNew), summarise, 
              freq = length(ecosystem), .drop = FALSE) # frequency, I want %
tbl2

n <- with(promDF2, table(ecosystem))
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
totalN <- as.data.frame(with(promDF2, table(factorNew)))
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
	scale_y_continuous(limits = c(0, 0.45)) +
	geom_text(label = "Literature examples", x = 1, y = 0.32, size = 4) +
	theme(axis.text.y = element_blank())

panelC

##### EXPERT OPINIONS - PREVENTING RES #####
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

###### PLOT PANEL D #####

ULClabel <- theme(plot.title = element_text(hjust = -0.1, vjust = 0, size = rel(1.5)))

panelD <- ggplot(prevSummary2, aes(x = factor2, y = value, 
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
	                                "Multiple", "Other"))) +
	scale_y_continuous(limits = c(0, 1))
panelD

##### LITERATURE - PREVENTING RES #####
# Check to see that preventFactor1 is empty if preventFactor2 is empty
View(paperDF2) # looks ok

f1DF <- with(paperDF2, data.frame(ecosystem = ecosystem, 
                                  ResilienceResponse = ResilienceResponse, 
                                  DisturbType1 = DisturbType1, 
                                  prevFactor = preventFactor1))

f2DF <- with(paperDF2, data.frame(ecosystem = ecosystem, 
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

factorList2 <- c("Global stressors", "Local anthropogenic stressors", 
                 "Local biotic stressors", "Lack of management", 
                 "Multiple", "Other", 
                 "Space preemption", "Lack of management")
            
factorList2
prevDF2$factorNew <- mapvalues(prevDF2$prevFactor, from = factorList, to = factorList2)

# prevDF2 is the full dataset; now filter to relevant criteria
unique(prevDF2$ResilienceResponse)
unique(prevDF2$DisturbType1)

# Now get frequencies for the filtered dataset
tbl2 <- ddply(prevDF2, .(ecosystem, factorNew), summarise, 
              freq = length(ecosystem), .drop = FALSE) # frequency, I want %
tbl2

n <- with(prevDF2, table(ecosystem))
str(n)
n <- as.data.frame(n)
n

merge(tbl2, n)
tbl3 <- merge(tbl2, n)
tbl3$prop <- with(tbl3, freq/Freq)
tbl3

detach("package:dplyr", unload = TRUE)
prevPapers <- summarySE(tbl3, measurevar = "prop", groupvars = "factorNew", 
                        na.rm = TRUE)
prevPapers

ULClabel <- theme(plot.title = element_text(hjust = -0.07, vjust = 0, size = rel(1.5)))

# get the frequency values for plotting along the y-axis
totalN <- as.data.frame(with(prevDF2, table(factorNew)))
totalN
totalN <- totalN[with(totalN, order(Freq, factorNew)), ]

#custom factor order
prevFactorOrder <- rev(c("Local anthropogenic stressors", "Local biotic stressors", 
                         "Space preemption", "Global stressors", "Lack of management", 
                         "Multiple", "Other"))

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

##### PLOTTING 5 PANELS #####

### with blank panel F
panelF <- ggplot(examples, aes(x = factor2, y = per)) + 
	theme_minimal(base_size = 12) + xlab("") + ylab("") + 
	geom_blank() + 
	theme(axis.ticks = element_blank(), axis.text = element_blank()) + 
	theme(panel.grid = element_blank()) 

### save as pdf
pdf("./figs/BS_Fig4_temp.pdf", 14, 7)
multiplot(panelA, panelB, panelC, panelF, panelD, panelE, 
          layout = matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, byrow = TRUE))
dev.off()
