#################################################
# O'Leary et al. 2015 
# Bright spots of hope: resilience of nearshore
# marine ecosystems to climatic impacts

# Decision tree for expert suggested literature
# Author: Robin Elahi
# Date: 150701
#################################################
# CHANGE LOG
# 151001
# J OLeary updated papers list file:
# PapersList2_NL_150526_JO_Oct1.csv



##########################################################################
# Decision tree for expert suggested papers
# Start with 131 suggested papers (rows).  
# Removed 4, where paper could not be found (127). 
# Removed 28, where paper was not relevant to the resilience question (99)
# Removed 1, where paper did not give a ResilienceResponse group 
# (e.g., habitat forming species)
# 
# So, full lit dataset has 98 rows.  
# 
# Including only habitat forming species and whole community - down to 82 rows. 
# Including only climatic disturbances, down to 51 rows.  
# (Did not remove restoration)
##########################################################################

dat <- read.csv("./data/PapersList2_NL_150526_JO_Oct1.csv", header=TRUE, na.strings="NA")

dim(dat)
names(dat)

# Remove rows where the paper could not be found
dat2 <- dat %>% filter(PaperType == "Empirical" | PaperType == "Review" |
                         PaperType == "NotRelevant" |PaperType == "Modelling")
dim(dat2) 

# Remove rows where the paper was not relevant to resilience
dat3 <- dat2 %>% filter(PaperType != "NotRelevant" )
dim(dat3) # 99 rows, keeping restoration 

# Remove rows where paper had no ResilienceResponse group (e.g., habitat forming species)
summary(dat3$ResilienceResponse)
dat3 %>% filter(ResilienceResponse == "" ) # this paper does not appear relevant
dat4 <- dat3 %>% filter(ResilienceResponse != "" )

dat4 <- droplevels(dat4)
unique(dat4$PaperType)
unique(dat4$DisturbType1)

#################################################
# rename to dat
dat <- dat4

# create new ecosystem column (with easier names)
ecoList <- unique(dat$EcosystemType)
ecoList
str(ecoList)
levels(ecoList)
ecoList2 <- c("Algal forests", "Coral reefs", "Mangroves", "Oyster reefs", 
              "Salt marshes", "Seagrasses")
ecosystemNew <- mapvalues(dat$EcosystemType, from = ecoList, to = ecoList2)
ecosystemNew 

dat2 <- cbind(ecosystemNew, dat)
names(dat2)

# select relevant columns and rename
dat3 <- dat2 %>% select(ecosystemNew, ResilienceOutcome, ResilienceResponse,
                        DisturbType1, DisturbType2) %>%
  rename(ecosystem = ecosystemNew)
dat3 <- droplevels(dat3)
dim(dat3)

# Need two sets of data - entire set, 
# and habitat forming species in the context of climatic stressors
unique(dat3$ResilienceResponse)
unique(dat3$ResilienceOutcome)
unique(dat3$DisturbType1)
summary(dat3)

#################################################
#################################################
litFull <- dat3
#################################################
#################################################

# only want to include habitat forming species AND whole community
# and climatic disturbances
lit <- droplevels(dat3[dat3$ResilienceResponse == "HabitatFormingSpp" |
                         dat3$ResilienceResponse == "WholeCommunity", ])
unique(lit$DisturbType1)
unique(lit$ResilienceOutcome)

# keep only climatic disturbances
lit2 <- droplevels(lit[lit$DisturbType1 == "Storms" |
                         lit$DisturbType1 == "Temperature" |
                         lit$DisturbType1 == "ENSO" |
                         lit$DisturbType1 == ">2 stressors" |
                         lit$DisturbType1 == "Bleaching" |
                         lit$DisturbType1 == "Sea level rise/hydrodymic change"
                       , ])

#################################################
#################################################
litSub <- lit2
#################################################
#################################################