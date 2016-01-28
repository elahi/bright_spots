#################################################
# O'Leary et al. 2015 
# Bright spots of hope: resilience of nearshore
# marine ecosystems to climatic impacts

# Plotting factors that promote and prevent resilience
# Author: Robin Elahi
# Date: 151211
#################################################

rm(list=ls(all=TRUE)) # removes all previous material from R's memory

##### LOAD PACKAGES, SOURCE DATA AND FUNCTIONS #####
# load packages
library(plyr)
library(dplyr)
library(reshape2)
library(ggplot2)

source("./R/summarizeData.R")

##### ggplot settings #####
theme_set(theme_classic(base_size = 10))
ULClabel <- theme(plot.title = element_text(hjust = -0.15, vjust = 0.5, size = rel(1.25)))

##### PANEL A: FACTORS PROMOTING RESILIENCE - EXPERT EXAMPLES #####
# load source data
source("./R/BS_resFactors_examples.R")

panelA <- ggplot(examples, aes(x = factor2, y = per)) + 
  xlab("Factors\npromoting resilience\n") + ylab("Proportion") + 
  geom_errorbar(aes(ymin = per, ymax = per + ci), 
                width = 0, color = "black") + 
  geom_bar(fill = "darkgray", position = position_dodge(0.8), 
  color = "black", stat = "identity", width = 0.8) + 
  labs(title = "A") + ULClabel + coord_flip() + 
  scale_x_discrete(labels = rev(c("Remaining\nbiogenic habitat", 
                                  "Recruitment or\nconnectivity", 
                                  "Physical\nsetting", 
                                  "Genetic\ndiversity", 
                                  "Functional\ndiversity", 
                                  "Species\ninteractions", 
                                  "Remoteness", 
                                  "Management", "Other"))) +
  scale_y_continuous(limits = c(0, 1)) +
  geom_text(label = "Expert examples", x = 1, y = 0.65, size = 3) 

panelA

##### PANEL B: FACTORS PROMOTING RESILIENCE - EXPERT OPINIONS #####
# load source data
source("./R/BS_resFactors_opinions.R")

panelB <- ggplot(opinions, aes(x = factor2, y = proportion, fill = resilCat)) +
  xlab("Factors\npromoting resilience\n") + ylab("Proportion") + coord_flip() + 
  # theme(axis.text.y = element_blank()) 
  geom_errorbar(aes(ymin = proportion, ymax = proportion + ci), 
                width = 0, color = "black", position = position_dodge(0.8)) + 
  labs(title = "B") + ULClabel +  
  scale_x_discrete(labels = rev(c("Remaining\nbiogenic habitat", 
                                  "Recruitment or\nconnectivity", 
                                  "Physical\nsetting", 
                                  "Genetic\ndiversity", 
                                  "Functional\ndiversity", 
                                  "Species\ninteractions", 
                                  "Remoteness", 
                                  "Management", "Other"))) +
  geom_text(label = "Expert opinions", x = 0.8, y = 0.65, size = 3) +
  theme(legend.justification = "center", legend.position = c(0.85, 0.5)) +
  theme(legend.title = element_blank()) +
  geom_bar(stat = "identity", position = position_dodge(0.8), 
           color = "black", width = 0.8) +
  scale_fill_manual(values = c("darkgray", "white")) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(legend.key.size = unit(0.35, "cm"))

panelB

##### PANEL D: FACTORS PREVENTING RESILIENCE - EXPERT OPINIONS #####
panelD <- ggplot(prevSummary2, aes(x = factor2, y = value, 
                                   fill = resilience)) + 
  xlab("Factors\npreventing resilience\n") + ylab("Proportion") + coord_flip() + 
  geom_errorbar(aes(ymin = value, ymax = value + ci), 
                width = 0, color = "black", position = position_dodge(0.8)) + 
  labs(title = "D") + ULClabel +
  geom_text(label = "Expert opinions", x = 0.8, y = 0.65, size = 3) +
  theme(legend.justification = "center", legend.position = c(0.85, 0.25)) +
  theme(legend.title = element_blank()) +
  geom_bar(stat = "identity", position = position_dodge(0.8), 
           color = "black", width = 0.8) +
  scale_fill_manual(values = c("darkgray", "white")) +
  guides(fill = guide_legend(reverse = TRUE))  +
  scale_x_discrete(labels = rev(c("Local\nanthropogenic\nstressors", 
                                  "Local biotic\nstressors", "Space\npreemption", 
                                  "Additional global\nstressors", "Lack of\nmanagement", 
                                  "Multiple", "Other"))) +
  scale_y_continuous(limits = c(0, 1)) +
  theme(legend.key.size = unit(0.35, "cm"))

panelD

##### PANEL C: FACTORS PROMOTING RESILIENCE - EXPERT LITERATURE #####
# load source data
source("./R/BS_resFactors_literature.R")

ULClabel <- theme(plot.title = element_text(hjust = -0.1, vjust = 0.5, size = rel(1.25)))

# Reorder based on max to min
panelC <- ggplot(papers, aes(x = factor2, y = prop)) + 
  xlab("") + ylab("Proportion") + 
	geom_errorbar(aes(ymin = prop, ymax = prop + ci), 
	width = 0, color = "black") + 
	coord_flip() + labs(title = "C") + ULClabel + 
  geom_bar(stat = "identity", position = position_dodge(0.8), 
           color = "black", fill = "darkgray", width = 0.8) +	
	scale_y_continuous(limits = c(0, 1)) +
	geom_text(label = "Recommended literature", x = 1, y = 0.75, size = 3) +
	theme(axis.text.y = element_blank()) 

panelC

##### PANEL E: FACTORS PREVENTING RESILIENCE - EXPERT LITERATURE #####
panelE <- ggplot(prevPapers, aes(x = factor, y = prop)) + 
  xlab("") + ylab("Proportion") + coord_flip() + 
  geom_errorbar(aes(ymin = prop, ymax = prop + ci), 
                width = 0, color = "black", position = position_dodge(0.8)) + 
  labs(title = "E") + ULClabel +
  geom_text(label = "Recommended literature", x = 1, y = 0.75, size = 3) +
  geom_bar(stat = "identity", 
           fill = "darkgray", color = "black", width = 0.8) +
  theme(axis.text.y = element_blank()) + 
  scale_y_continuous(limits = c(0, 1))

panelE

##### PANEL BLANK #####
panelBlank <- ggplot(examples, aes(x = factor2, y = per)) + 
  theme_minimal(base_size = 12) + xlab("") + ylab("") + 
  geom_blank() + 
  theme(axis.ticks = element_blank(), axis.text = element_blank()) + 
  theme(panel.grid = element_blank()) 

##### MULTI-PANEL PLOT #####
source("./R/multiplotF.R")

### THREE ROWS
pdf("./figs/BS_Fig4.pdf", width = 7, height = 10)
multiplot(panelA, panelBlank, panelB, panelC, panelD, panelE, 
          layout = matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, byrow = TRUE))
dev.off()


