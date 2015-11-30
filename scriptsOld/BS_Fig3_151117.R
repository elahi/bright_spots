#################################################
# O'Leary et al. 2015 
# Bright spots of hope: resilience of nearshore
# marine ecosystems to climatic impacts

# Figure 3
# Frequency of observed resilience
# Author: Robin Elahi
# Date: 150706
#################################################
# CHANGE LOG
# 151002
# Column H (Disturbance_StrongReslience) was updated by J O'Leary
# One set of bars only (climate disturbance and habitat-forming species only)
# Remove white bars (all respondents)

# 151112
# Uploaded new file from Jennifer O'Leary, with corrected expert examples
# Still need to wait for final corrections on expert papers information

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
# Ecosystem Resilience Survey = ERS
### ERS Question 5
###  In your research, have you encountered instances of notable RESILIENCE, 
# either through strong resistance to or fast recovery from climatic disturbances?

# load source data
source("./R/process_expert_survey.R")
names(dat)

# Filtering steps
# 97 respondents to start

# now filter to relevant climate criteria
unique(dat$Disturbance_StrongReslience)

# Exclude: "exclude - no disturbance found" and "non-climatic"
  
datSub <- dat %>% filter(Disturbance_StrongReslience != "exclude - no disturbance found" &
                          Disturbance_StrongReslience != "non-climatic" &
                          Disturbance_StrongReslience != "non climatic")

datSub <- droplevels(datSub)
unique(datSub$Disturbance_StrongReslience)

### ALL 97 RESPONDENTS
tbl2 <- ddply(dat, .(ecosystem, Resilience), summarise, 
              freq = length(Resilience), .drop = FALSE) # frequency, I want %
obs <- aggregate(freq ~ ecosystem, data = tbl2, sum)
total <- with(tbl2, sum(freq))

yes <- tbl2[tbl2$Resilience == "Yes", ]
propYes <- yes$freq/(obs$freq)

tbl3 <- cbind(obs, yes$freq, propYes)
names(tbl3) <- c("Ecosystem", "N", "Yes", "Proportion")

examples <- tbl3

# Reverse alphabetical
examples$Ecosystem <- with(examples, factor(Ecosystem, 
	levels = levels(Ecosystem)[order(levels(Ecosystem), decreasing = TRUE)]))
unique(examples$Ecosystem)

# Rename to full
fullDF <- examples
fullDF

### Subset data to climate disturbances only
datSub$Resilience # all yes!

tbl2 <- ddply(datSub, .(ecosystem, Resilience), summarise, 
              freq = length(Resilience), .drop = FALSE) # frequency, I want %

obs <- aggregate(freq ~ ecosystem, data = tbl2, sum)
total <- with(tbl2, sum(freq))

yes <- tbl2[tbl2$Resilience == "Yes", ]
propYes <- yes$freq/(obs$freq)

tbl3 <- cbind(obs, yes$freq, propYes)
names(tbl3) <- c("Ecosystem", "N", "Yes", "Proportion")

examples <- tbl3

# Reverse alphabetical
examples$Ecosystem <- with(examples, factor(Ecosystem, 
                                            levels = levels(Ecosystem)[order(levels(Ecosystem), decreasing = TRUE)]))
unique(examples$Ecosystem)

# rename
subDF <- examples

### Combine df's
fullDF$data <- "full"
subDF$data <- "subset"
examplesDF <- rbind(fullDF, subDF)

### Panel A; use ggplot2 
ULClabel <- theme(plot.title = element_text(hjust = -0.2, 
                                            vjust = 0, size = rel(1.5)))

subDF <- examplesDF[examplesDF$data == "subset", ]
fullDF <- examplesDF[examplesDF$data == "full", ]

subDFn <- examplesDF[examplesDF$data == "subset", ]$N
fullDFn <- examplesDF[examplesDF$data == "full", ]$N

panelA <- ggplot(subDF,aes(x = Ecosystem, y = Proportion)) + 
  theme_classic(base_size = 12) + xlab("") + ylab("Proportion") + 
  geom_bar(fill = "darkgray", color = "black", 
           stat = "identity", width = 0.8) +
  scale_y_continuous(limits = c(0, 1)) + 
  coord_flip() + 	
  guides(fill = guide_legend(reverse = TRUE)) +	
  geom_text(aes(x = 1:6, y = 0.05), data = subDF, 
            label = rev(subDFn), size = 3) + 
  labs(title = "A") + ULClabel + 
  geom_text(label = "Expert\nexamples", x = 1, y = 0.9, size = 3)	+
  theme(legend.position = "none")

panelA
multiplot(panelA, panelB, cols = 2)

################################
################################
# EXPERT PAPERS
################################
################################

# load source data
source("./R/process_expert_papers.R")

#################################################
# Get proportions for litSub (only habitat formers, whole comm, climatic dist)
summary(litSub)
totalN <- ddply(litSub, .(ecosystem), summarise, 
                totalN = length(ecosystem), .drop = FALSE) 
totalN

litSub2 <- litSub[litSub$ResilienceOutcome != "No", ]
litSub2
totalYes <- ddply(litSub2, .(ecosystem), summarise, 
                  freqYes = length(ecosystem), .drop = FALSE) 
summary(litSub2)

litData <- cbind(totalN, totalYes$freqYes)
colnames(litData)[3] <- "freqYes"
head(litData)
litData$dataset <- rep("subset", 6)
litData

#################################################
# Get proportions for litFull (98 papers)
summary(litFull)

totalNfull <- ddply(litFull, .(ecosystem), summarise, 
                    totalN = length(ecosystem), .drop = FALSE) 
totalNfull
levels(litFull$ResilienceOutcome)

litFull2 <- litFull[litFull$ResilienceOutcome == "Yes" |
               litFull$ResilienceOutcome == "Context-Dependent", ]
dim(litFull2)

totalYesFull <- ddply(litFull2, .(ecosystem), summarise, 
                      freqYesFull = length(ecosystem), .drop = FALSE) 
summary(totalYesFull)
totalYesFull

litDataFull <- cbind(totalNfull, totalYesFull$freqYesFull)
litDataFull
colnames(litDataFull)[3] <- "freqYes"
litDataFull$dataset <- rep("full", 6)

#################################################
# Combine litData and litFull
litData
litDataFull

litData2 <- rbind(litData, litDataFull)
litData2$prop <- with(litData2, freqYes/totalN)
colnames(litData2) <- c("Ecosystem", "N", "Yes", "Dataset", "Proportion")
litData2

unique(litData2$Ecosystem)
unique(litData2$Dataset)
litData2$Ecosystem <- with(litData2, factor(Ecosystem, 
	levels = levels(Ecosystem)[order(levels(Ecosystem), decreasing = TRUE)]))

litData2

###############################
# MULTIPANEL PLOT
subsetPapers <- litData2[litData2$Dataset == "subset", ]
fullPapers <- litData2[litData2$Dataset == "full", ]

subsetN <- litData2[litData2$Dataset == "subset", ]$N
fullN <- litData2[litData2$Dataset == "full", ]$N

ULClabel <- theme(plot.title = element_text(hjust = -0.2, 
                                            vjust = 0, size = rel(1.5)))

panelB <- ggplot(subsetPapers,aes(x = Ecosystem, y = Proportion)) + 
  theme_classic(base_size = 12) + xlab("") + ylab("Proportion") + 
  geom_bar(fill = "darkgray", color = "black", 
           stat = "identity", width = 0.8) +
  coord_flip() + 	
  guides(fill = guide_legend(reverse = TRUE)) +	
  geom_text(aes(x = 1:6, y = 0.05), data = subsetPapers, 
            label = rev(subsetN), size = 3) + 
  labs(title = "B") + ULClabel + 
  geom_text(label = "Literature\nexamples", x = 1, y = 0.8, size = 3) 	+
  theme(legend.position = "none")

panelB


###############################
# save as 7 x 3.5 pdf
pdf("./figs/BS_Fig3.pdf", 7, 3.5)
multiplot(panelA, panelB, cols = 2)
dev.off()	





