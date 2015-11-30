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
head(dat)
unique(dat$Paper)

# create new ecosystem column (with easier names)
ecoList <- unique(dat$EcosystemType)
ecoList
str(ecoList)
levels(ecoList)
ecoList2 <- c("Algal forests", "Coral reefs", "Mangroves", "Oyster reefs", 
              "Salt marshes", "Seagrasses")
dat$ecosystemNew <- mapvalues(dat$EcosystemType, from = ecoList, to = ecoList2)

# select relevant columns and rename
dat2 <- dat %>% select(Paper, ecosystemNew, ResilienceOutcome, ResilienceResponse,
                        DisturbType1, DisturbType2, PaperType) %>%
  rename(ecosystem = ecosystemNew)
dat2 <- droplevels(dat2)

# This is the frequency of all papers
dat2 %>% group_by(ecosystem) %>% summarise(freq = n())

### Need to get relevant papers

# Remove rows where the paper could not be found
unique(dat2$PaperType)
dat2 <- dat2 %>% filter(PaperType != "can't find paper")

# Remove rows where the paper was not relevant to resilience
unique(dat2$PaperType)
dat3 <- dat2 %>% filter(PaperType != "NotRelevant")
dim(dat3) # 90 rows, keeping restoration 

# Remove rows where paper had no ResilienceResponse group (e.g., habitat forming species)
summary(dat3$ResilienceResponse)
dat3 %>% filter(ResilienceResponse == "" ) # this paper does not appear relevant
dat4 <- dat3 %>% filter(ResilienceResponse != "" )

dat4 <- droplevels(dat4)
unique(dat4$PaperType)
unique(dat4$DisturbType1)

#################################################
# rename
litOrig <- dat2
litFull <- dat4
#################################################

### Now include only habitat forming species AND whole community
# and climatic disturbances
unique(litFull$ResilienceResponse)
litSub <- litFull %>% filter(ResilienceResponse == "HabitatFormingSpp" |
                               ResilienceResponse == "WholeCommunity")
litSub <- droplevels(litSub)

unique(litSub$DisturbType1)
unique(litSub$DisturbType2)
unique(litSub$ResilienceOutcome)

# For disturbType1
# keep only climatic disturbances
litSub1 <- litSub %>% filter(DisturbType1 == "Storms" |
                               DisturbType1 == "Temperature" |
                               DisturbType1 == "ENSO" |
                               DisturbType1 == ">2 stressors" |
                               DisturbType1 == "Sea level rise/hydrodymic change")
# For disturbType2
# keep only climatic disturbances
litSub2 <- litSub %>% filter(DisturbType2 == "Storms" |
                               DisturbType2 == "Temperature" |
                               DisturbType2 == "ENSO" |
                               DisturbType2 == ">2 stressors" |
                               DisturbType2 == "Sea level rise/hydrodymic change")

head(litSub)

# now merge the two datasets, so that I get all the studies that have at least 1 climate-related
# disturbance, regardless of where it was listed
litSub3 <- full_join(litSub1, litSub2)
head(litSub3)

#################################################
#################################################
litSub <- litSub3
#################################################
#################################################