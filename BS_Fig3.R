#################################################
# O'Leary et al. 2015 
# Bright spots of hope: resilience of nearshore
# marine ecosystems to climatic impacts

# Figure 3
# Frequency of observed resilience
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
# Ecosystem Resilience Survey = ERS
### ERS Question 5
###  In your research, have you encountered instances of notable RESILIENCE, 
# either through strong resistance to or fast recovery from climatic disturbances?


# Filtering steps
# 97 respondents to start
# Remove non-climate disturbances; 57 remaining

# load source data
source("./R/process_expert_survey.R")
names(dat)

# now filter to relevant climate criteria
unique(dat$Disturbance_StrongReslience)

datSub <- dat %>% filter(Disturbance_StrongReslience == "Storms" |
                          Disturbance_StrongReslience == "Temperature" |
                          Disturbance_StrongReslience == "ENSO (temperature/storms)" | 
                          Disturbance_StrongReslience == "Multiple" |
                          Disturbance_StrongReslience == "Bleaching" |
                          Disturbance_StrongReslience == "Sea level rise/ Hydrodynamic change")
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
ULClabel <- theme(plot.title = element_text(hjust = -0.15, 
                                            vjust = 0, size = rel(1.5)))

subDFn <- examplesDF[examplesDF$data == "subset", ]$N
fullDFn <- examplesDF[examplesDF$data == "full", ]$N

panelA <- ggplot(examplesDF, aes(x = Ecosystem, y = Proportion, fill = rev(data))) + 
  theme_classic(base_size = 12) + xlab("") + ylab("Proportion") + 
  labs(title = "A") + ULClabel + 
  geom_bar(color = "black", stat = "identity", 
           position = position_dodge(0.8), width = 0.8) +
  coord_flip() + 	
  scale_fill_manual(values = c("darkgray", "white")) + 
  guides(fill = guide_legend(reverse = TRUE)) +	
  geom_text(aes(x = 0.8:5.8, y = 0.05), data = examplesDF[examplesDF$data == "subset", ], 
            label = rev(subDFn), size = 2.8) + 
  geom_text(aes(x = 1.25:6.25, y = 0.05), data = examplesDF[examplesDF$data == "full", ], 
            label = rev(fullDFn), size = 2.8) +	
  labs(title = "A") + ULClabel + 
  geom_text(label = "Expert examples", x = 1.25, y = 0.8, size = 3) 	+
  theme(legend.position = "none") 

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

subsetN <- litData2[litData2$Dataset == "subset", ]$N
fullN <- litData2[litData2$Dataset == "full", ]$N

ULClabel <- theme(plot.title = element_text(hjust = -0.1, vjust = 0, size = rel(1.5)))
panelB <- ggplot(litData2, aes(x = Ecosystem, y = Proportion, fill = rev(Dataset))) + 
	theme_classic(base_size = 12) + xlab("") + ylab("Proportion") + 
	labs(title = "B") + ULClabel + 
	geom_bar(color = "black", stat = "identity", 
		position = position_dodge(0.8), width = 0.8) +
	coord_flip() + 	
	scale_fill_manual(values = c("darkgray", "white")) + 
	guides(fill = guide_legend(reverse = TRUE)) +	
	geom_text(aes(x = 0.8:5.8, y = 0.05), data = litData2[litData2$Dataset == "subset", ], 
		label = rev(subsetN), size = 2.8) + 
	geom_text(aes(x = 1.25:6.25, y = 0.05), data = litData2[litData2$Dataset == "full", ], 
		label = rev(fullN), size = 2.8) +	
	labs(title = "B") + ULClabel + 
	geom_text(label = "Literature\nexamples", x = 2, y = 0.8, size = 3) 	+
	theme(legend.position = "none") +
	theme(axis.text.y = element_blank())

panelB


###############################
# save as 7 x 3.5 pdf
pdf("./figs/BS_Fig3.pdf", 7, 3.5)
multiplot(panelA, panelB, cols = 2)
dev.off()	





