#################################################
# O'Leary et al. 2015 
# Bright spots of hope: resilience of nearshore
# marine ecosystems to climatic impacts

# Supplemental Figure
# Observation of resilience ~ years of experience
# Author: Robin Elahi
# Date: 151001
#################################################
# CHANGE LOG
# 160120
# Final, final revisions to data files, received from Jen on 20 Jan 2016

rm(list=ls(all=TRUE)) # removes all previous material from R's memory

# load packages
library(plyr)
library(dplyr)
# library(reshape2)
library(ggplot2)
library(AICcmodavg)

# load source functions
#source("./R/summarizeData_150204.R")
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

# create numeric response of resilience observed or not
dat$resNum <- with(dat, ifelse(Resilience == "Yes", 1, 0))
with(dat, table(resNum))

# filter to relevant responses using Jen's dummy column
datSub <- dat %>% filter(Dummy == 1)
with(datSub, table(resNum))

names(datSub)
unique(datSub$ecosystem)

#################################################
# USE GLM
#################################################
head(datSub)
names(datSub)
with(datSub, table(resNum, ecosystem))

# Set up candidate model list
Cand.mod <- list()

Cand.mod[[1]] <- glm(resNum ~ Experience * ecosystem, data = datSub, family = binomial())
summary(Cand.mod[[1]])
anova(Cand.mod[[1]], test = "Chisq")

Cand.mod[[2]] <- glm(resNum ~ Experience + ecosystem, data = datSub, family = binomial())
Cand.mod[[3]] <- glm(resNum ~ Experience , data = datSub, family = binomial())
summary(Cand.mod[[3]])
# Is the residual deviance significant?
1 - pchisq(70.379, 69) # NO

Cand.mod[[4]] <- glm(resNum ~ ecosystem , data = datSub, family = binomial())
Cand.mod[[5]] <- glm(resNum ~ 1 , data = datSub, family = binomial())

#create a vector of names to trace back models in set
mod_numbers <- paste("Cand.mod", 1:length(Cand.mod), sep=" ")	
mod_text <- c("Experience * Ecosystem", "Experience + Ecosystem", "Experience", 
              "Ecosystem", "Null model")

#generate AICc table with numbers
mod.aicctab <- aictab(cand.set= Cand.mod, modnames=mod_numbers, sort = TRUE, 
                      second.ord = TRUE) # second.ord =TRUE means AICc is used (not AIC)
print(mod.aicctab, digits=2, LL=TRUE)

#generate AICc table with names
mod.aicctab <- aictab(cand.set= Cand.mod, modnames= mod_text, sort = TRUE, 
                      second.ord = FALSE) # second.ord =TRUE means AICc is used (not AIC)
print(mod.aicctab, digits=2, LL=TRUE)
write.csv(mod.aicctab, 'output/glm_results.csv')

# plot results
plot1 <- ggplot(datSub, aes(Experience, resNum, shape = ecosystem)) +
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
plot1 + facet_wrap(~ ecosystem)

anova(Cand.mod[[5]], Cand.mod[[3]])
