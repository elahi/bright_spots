#################################################
# O'Leary et al. 2015 
# Bright spots of hope: resilience of nearshore
# marine ecosystems to climatic impacts

# Decision tree for expert suggested literature
# Author: Robin Elahi
# Date: 150701
#################################################
# CHANGE LOG
# 151001 - JO updated papers list file
# PapersList2_NL_150526_JO_Oct1.csv
# 151201 - JO updated papers list file
# BS_RecommendedLiterature_Final_Nov30_2015.csv
#################################################

dat <- read.csv("./data/BS_RecommendedLiterature_Final_Nov30_2015.csv", 
                header=TRUE, na.strings = "NA")
head(dat)
unique(dat$Paper)
names(dat)

###########################
###########################
### Clean up  the data frame
# create new ecosystem column (with easier names)
ecoList <- unique(dat$EcosystemType)
ecoList
ecoList2 <- c("Seagrasses", "Algal forests", "Coral reefs", 
              "Oyster reefs", "Mangroves", "Salt marshes")

dat$ecosystemNew <- mapvalues(dat$EcosystemType, from = ecoList, 
                              to = ecoList2)
dat %>% select(EcosystemType, ecosystemNew)

# select relevant columns and rename
dat2 <- dat %>% select(Paper, PaperType, ecosystemNew, EcosystemType, 
                       ResilienceOutcome, ResilienceCat, ResilienceResponse,
                        DisturbType1, DisturbType2, DISTURBANCE.COMPILED) %>%
  rename(ecosystem = ecosystemNew)
dat2 <- droplevels(dat2)

# This is the frequency of all papers
dat2 %>% group_by(ecosystem) %>% summarise(freq = n())

# rename
litOrig <- dat2

###########################
###########################
### Need to get relevant papers

# Remove rows where the paper could not be found
unique(dat2$PaperType)
summary(dat2$PaperType)
dat2 <- dat2 %>% filter(PaperType != "can't find paper")

# Remove rows where the paper was not relevant to resilience
unique(dat2$PaperType)
dat3 <- dat2 %>% filter(PaperType != "Not Relevant")

# Remove rows where the paper was on restoration 
summary(dat3$ResilienceCat)
dat4 <- dat3 %>% filter(ResilienceCat != "Restoration")

# Remove rows where paper had no ResilienceResponse group? 
# (e.g., habitat forming species)
summary(dat4$ResilienceResponse)
dat4 %>% filter(ResilienceResponse == "" ) 
# this paper was in reference to non climatic disturbance so will
# be removed anyway

dat4 <- droplevels(dat4)
unique(dat4$PaperType)
unique(dat4$DisturbType1)

### Now include only habitat forming species AND whole community
unique(dat4$ResilienceResponse)
dat5 <- dat4 %>% filter(ResilienceResponse == "HabitatFormingSpp" |
                               ResilienceResponse == "WholeCommunity")
dat5 <- droplevels(dat5)

### Remove non climatic disturbances
unique(dat5$DISTURBANCE.COMPILED)
litSub <- dat5 %>% filter(DISTURBANCE.COMPILED != "non climatic")

#################################################
