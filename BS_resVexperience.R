#################################################
# O'Leary et al. 2015 
# Bright spots of hope: resilience of nearshore
# marine ecosystems to climatic impacts

# Supplemental Figure
# Observation of resilience ~ years of experience
# Author: Robin Elahi
# Date: 151001
#################################################


rm(list=ls(all=TRUE)) # removes all previous material from R's memory

# load packages
library(plyr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(AICcmodavg)

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

# create new ecosystem column (with easier names)
ecoList <- unique(dat$Ecosystem)
ecoList
str(ecoList)
levels(ecoList)
ecoList2 <- c("Algal forests", "Coral reefs", "Mangroves", "Oyster reefs", "Salt marshes", "Seagrasses")
dat$ecosystemNew <- mapvalues(dat$Ecosystem, from = ecoList, to = ecoList2)

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

datSub$resNum <- ifelse(datSub$Resilience == "Yes", 1, 0)

names(datSub)
unique(datSub$Ecosystem)

#################################################
# USE GLM
#################################################
head(datSub)
with(datSub, table(resNum, ecosystemNew))

# Set up candidate model list
Cand.mod <- list()

Cand.mod[[1]] <- glm(resNum ~ Experience*ecosystemNew, data = datSub, family = binomial())
Cand.mod[[2]] <- glm(resNum ~ Experience + ecosystemNew, data = datSub, family = binomial())
Cand.mod[[3]] <- glm(resNum ~ Experience , data = datSub, family = binomial())
Cand.mod[[4]] <- glm(resNum ~ ecosystemNew , data = datSub, family = binomial())
Cand.mod[[5]] <- glm(resNum ~ 1 , data = datSub, family = binomial())

#create a vector of names to trace back models in set
mod_numbers <- paste("Cand.mod", 1:length(Cand.mod), sep=" ")	
mod_text <- c("Experience * Ecosystem", "Experience + Ecosystem", "Experience", 
              "Ecosystem", "Null model")

#generate AICc table with numbers
mod.aicctab <- aictab(cand.set= Cand.mod, modnames=mod_numbers, sort=TRUE, 
                      second.ord=FALSE) # second.ord =TRUE means AICc is used (not AIC)
print(mod.aicctab, digits=2, LL=TRUE)

#generate AICc table with names
mod.aicctab <- aictab(cand.set= Cand.mod, modnames= mod_text, sort=TRUE, 
                      second.ord=FALSE) # second.ord =TRUE means AICc is used (not AIC)
print(mod.aicctab, digits=2, LL=TRUE)
write.csv(mod.aicctab, 'output/glm_results.csv')

summary(glm1)
anova(glm1, test = "Chisq")
anova(glm1, test = "LRT")

AIC(glm1, glm2, glm3, glm4)
anova(glm3, glm4)
plot(glm3)
summary(glm4)

# plot results
plot1 <- ggplot(datSub, aes(Experience, resNum, shape = ecosystemNew)) +
	theme_classic(base_size = 12) + xlab("Experience (years)") + 
	ylab("Have you observed resilience?\n (proportion)") + 
	# theme(legend.justification = c(1,0), legend.position = c(1, 0.01)) +
	theme(legend.title = element_blank()) + 
	geom_point(size = 2, alpha = 0.8, position = position_jitter(h = 0.05)) 

plot1

plot1 + stat_smooth(mapping = aes(shape = NULL), method = "glm", family = "binomial", 
                    color = "black", size = 1)

ggsave("./figs/BS_resVexperience.pdf", height = 3.5, width = 5)

# Can't test the interaction well because few "no's"
plot1 + facet_wrap(~ ecosystemNew)

