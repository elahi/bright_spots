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

# 151130
# Following Jen's requested modifications
# Fig 3A: 2 bars per habitat: 
# 1st bar = fraction that saw climatic disturbance
# 2nd bar = of those that saw climatic disturbance, how many saw resilience

# 151201
# More changes to the data files - received from Jen on 1 Dec 2015
# (Final revisions)

# 160120
# Final, final revisions to data files, received from Jen on 20 Jan 2016

# 160128
# Changed formatting according to GDL

################################################################
### LOAD FILES AND PACKAGES
rm(list=ls(all=TRUE)) # removes all previous material from R's memory

# load packages

library(plyr)
library(dplyr)
library(ggplot2)
library(grid)

# load source functions
source("./R/multiplotF.R")

################################################################
###### EXPERT EXAMPLES

# Ecosystem Resilience Survey = ERS
### ERS Question 5
###  In your research, have you encountered instances of notable RESILIENCE, 
# either through strong resistance to or fast recovery from climatic disturbances?

# load source data
source("./R/process_expert_survey.R")

# Filtering steps
# 97 respondents to start

###### First step: what fraction of respondents, 
# gave relevant responses (i.e., climate-related) by ecosystem
head(dat)
names(dat)

# get relevant columns
dat2 <- dat %>% select(Ecosystem, ecosystem, Experience, Resilience, 
                       Disturbance_StrongReslience, Dummy)

# make new column
unique(dat2$Disturbance_StrongReslience)
dat2$relevance <- with(dat2, 
                       ifelse(Disturbance_StrongReslience != "exclude - no disturbance found" &
                                Disturbance_StrongReslience != "non climatic", 
                              "yes", "no"))
head(dat2)

# Create new table with Ecosystem, total sample size, and proportion of relevant responses
total <- dat2 %>% group_by(ecosystem) %>% summarise(freq = n()) 
yes <- dat2 %>% group_by(ecosystem, relevance) %>% summarise(freq = n()) %>% 
  filter(relevance == "yes")
total$propYes <- yes$freq/total$freq
total
names(total) <- c("Ecosystem", "N", "Proportion")
relevanceDat <- total

###### Second step: filter to observed disturbances and relevant 
# climate criteria and calculate proportion of observed resilience
unique(dat$Disturbance_StrongReslience)

datSub <- dat %>% filter(Disturbance_StrongReslience != "exclude - no disturbance found" &
                          Disturbance_StrongReslience != "non climatic")

# this is the same, but using Jen's dummy column
datSub <- dat %>% filter(Dummy == 1)

datSub <- droplevels(datSub)
unique(datSub$Disturbance_StrongReslience)
unique(datSub$Resilience)

# overall percentage of observed resilience, ignoring ecosystem types
datSub %>% group_by(Resilience) %>% summarise(freq = n())
57/(14+57)

# Create new table with Ecosystem, total sample size, and proportion of relevant responses
total <- datSub %>% group_by(ecosystem) %>% summarise(freq = n()) 
yes <- datSub %>% group_by(ecosystem, Resilience) %>% summarise(freq = n()) %>% 
  filter(Resilience == "Yes")
total
yes
total$propYes <- yes$freq/total$freq
total
names(total) <- c("Ecosystem", "N", "Proportion")
resilienceDat <- total

# Combine the two dataframes
relevanceDat
resilienceDat
relevanceDat$data <- "Observed climatic disturbance"
resilienceDat$data <- "Observed resilience"
examplesDF <- rbind(relevanceDat, resilienceDat)
examplesDF

# Change to factors
str(examplesDF)
examplesDF$Ecosystem <- as.factor(examplesDF$Ecosystem)
examplesDF$data <- as.factor(examplesDF$data)

# Reorder data levels
examplesDF$data <- factor(examplesDF$data, 
                        levels = c("Observed climatic disturbance", "Observed resilience"))
levels(examplesDF$data)

# Reverse alphabetical
examplesDF$Ecosystem <- 
  with(examplesDF, factor(Ecosystem, 
                          levels = levels(Ecosystem)[order(levels(Ecosystem), 
                                 decreasing = TRUE)]))
unique(examplesDF$Ecosystem)


###### Panel A ######
ULClabel <- theme(plot.title = element_text(hjust = -0.05, 
                                            vjust = 0, size = rel(1.5)))
relevance_N <- relevanceDat$N
resilience_N <- resilienceDat$N

examplesDF

# what is the overall percentage of observed resilience, across ecosystem types?
examplesDF %>% filter(data == "Observed resilience") %>% 
  summarise(mean = mean(Proportion), sd = sd(Proportion))

panelA <- ggplot(data = examplesDF, aes(Ecosystem, Proportion, 
                            fill = data)) + 
  theme_classic(base_size = 12) + xlab("") + ylab("Proportion of expert respondents") + 
  geom_bar(stat = "identity", position = "stack", color = "black") + 
  scale_fill_manual(values = c("white", "black")) + 
  facet_grid(. ~ data) + coord_flip() + 
  theme(legend.position = "none") + 
  scale_y_continuous(limits = c(0,1)) + 
  scale_x_discrete("", labels = c("Algal forests" = "Algal forests\n(18,12)", 
                                  "Coral reefs" = "Coral reefs\n(19,18)", 
                                  "Mangroves" = "Mangroves\n(15,10)", 
                                  "Oyster reefs" = "Oyster reefs\n(13,6)", 
                                  "Salt marshes" = "Salt marshes\n(15,12)", 
                                  "Seagrasses" = "Seagrasses\n(17,13)")) + 
  labs(title = "A") + ULClabel + 
  theme(panel.margin = unit(2, "lines")) + 
  theme(strip.background = element_blank())

multiplot(panelA, panelB, cols = 1)

###### EXPERT PAPERS #####
# load source data
source("./R/process_expert_papers.R")

# This is the frequency of all papers
litOrig %>% group_by(ecosystem) %>% summarise(freq = n())
# This is the frequency of relevant papers
litSub %>% group_by(ecosystem) %>% summarise(freq = n())

###### First step: what fraction of papers, 
# gave relevant responses (i.e., climate-related) by ecosystem

# Create new table with Ecosystem, total sample size, and 
# proportion of relevant papers
total <- litOrig %>% group_by(ecosystem) %>% summarise(freq = n())
total

yes <- litSub %>% group_by(ecosystem) %>% summarise(freq = n())
yes
# need to use ddply because I am missing oyster reefs otherwise
yes <- litSub %>% group_by(ecosystem) %>% 
  ddply(.(ecosystem), summarise, freq = length(ecosystem), .drop = FALSE)
yes

total$Resilience <- "Relevance"
total$propYes <- yes$freq/total$freq
total
names(total) <- c("Ecosystem", "N", "Resilience", "Proportion")
relDat <- total

###### Second step: Calculate proportion of observed resilience
# Create new table with Ecosystem, total sample size, and 
# proportion of relevant papers
total <- litSub %>% group_by(ecosystem) %>%
  ddply(.(ecosystem), summarise, freq = length(ecosystem), .drop = FALSE)

# use ddply because allows me to use .drop = FALSE (keeps all ecosystems)
no <- litSub %>% group_by(ResilienceOutcome) %>% 
  filter(ResilienceOutcome == "No") %>% 
  ddply(.(ecosystem), summarise, freq = length(ecosystem), 
        .drop = FALSE) 

yes <- litSub %>% group_by(ResilienceOutcome) %>% 
  filter(ResilienceOutcome == "Yes") %>% 
  ddply(.(ecosystem), summarise, freq = length(ecosystem), 
        .drop = FALSE) 

cd <- litSub %>% group_by(ResilienceOutcome) %>% 
  filter(ResilienceOutcome == "Context-Dependent") %>% 
  ddply(.(ecosystem), summarise, freq = length(ecosystem), 
        .drop = FALSE) 

total$propNo <- no$freq/total$freq
total$propYes <- yes$freq/total$freq
total$propCD <- cd$freq/total$freq
total$yesCD <- 1 - total$propNo

total

names(total) <- c("Ecosystem", "N", "No", "Yes", "Context", "YesCD")
total
resDat <- total

# make long
resDatL <- resDat %>% 
  tidyr::gather(key = Resilience, value = "Proportion", No:YesCD) %>%
  filter(Resilience == "Yes" | Resilience == "Context")
resDatL


# Combine the two dataframes
relDat$data <- "Observed climatic disturbance"
resDatL$data <- "Observed resilience"
papersDF <- rbind(relDat, resDatL)

# Change to factors
str(papersDF)
papersDF$Resilience <- as.factor(papersDF$Resilience)
papersDF$data <- as.factor(papersDF$data)

# Reorder data levels
papersDF$data <- factor(papersDF$data, 
                              levels = c("Observed climatic disturbance", "Observed resilience"))
levels(papersDF$data)

# Reverse alphabetical
papersDF$Ecosystem <- 
  with(papersDF, factor(Ecosystem, 
                          levels = levels(Ecosystem)[order(levels(Ecosystem), 
                                                           decreasing = TRUE)]))
unique(papersDF$Ecosystem)


### Panel B
relevance_N <- relDat$N
resilience_N <- resDatL$N[1:6]

papersDF

###### Panel B #####
panelB <- ggplot(data = papersDF, aes(Ecosystem, Proportion, 
                            fill = Resilience)) + 
  theme_classic(base_size = 12) + xlab("") + ylab("Proportion of recommended literature") + 
  geom_bar(stat = "identity", position = "stack", color = "black") + 
  scale_fill_manual(values = c("darkgray", "white", "black")) + 
  facet_grid(. ~ data) + coord_flip() + 
  theme(legend.justification = c(1,0), legend.position = c(1, 0.1)) +
  theme(legend.position = "none") + 
  scale_y_continuous(limits = c(0,1)) + 
  scale_x_discrete("", labels = c("Algal forests" = "Algal forests\n(29,16)", 
                                  "Coral reefs" = "Coral reefs\n(22,17)", 
                                  "Mangroves" = "Mangroves\n(23,9)", 
                                  "Oyster reefs" = "Oyster reefs\n(11,0)", 
                                  "Salt marshes" = "Salt marshes\n(23,2)", 
                                  "Seagrasses" = "Seagrasses\n(21,9)")) + 
  labs(title = "B") + ULClabel + 
  theme(panel.margin = unit(2, "lines")) + 
  theme(strip.background = element_blank())
  
panelB

###############################
# save as pdf
pdf("./figs/BS_Fig3.pdf", 7, 7)
multiplot(panelA, panelB, cols = 1)
dev.off()	



