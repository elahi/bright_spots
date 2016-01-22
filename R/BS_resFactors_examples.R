#################################################
# O'Leary et al. 2015 
# Bright spots of hope: resilience of nearshore
# marine ecosystems to climatic impacts

# Prepare expert examples data
# (to plot factors that promote and prevent resilience)
# Author: Robin Elahi
# Date: 151211
#################################################

source("./R/process_expert_survey.R")

##### GET RELEVANT ROWS AND COLUMNS #####
# Get subset of relevant rows
names(dat)

datSub <- dat %>% filter(Disturbance_StrongReslience != "exclude - no disturbance found" &
                           Disturbance_StrongReslience != "non climatic")
summary(datSub$DRes_SpacePrem)

# this is the same, but using Jen's dummy column
datSub <- dat %>% filter(Dummy == 1)
summary(datSub$DRes_SpacePrem)

# Get subset of relevant columns 
dat2 <- datSub %>%
  select(ecosystem, Length_StrongResilience, ResilienceFactorWriteIn1,
         ResilienceFactorWriteIn2, Disturbance_StrongReslience) %>%
  rename(factor1 = ResilienceFactorWriteIn1, 
         factor2 = ResilienceFactorWriteIn2, disturbLength1 = Length_StrongResilience)

dat2 <- droplevels(dat2)

with(dat2, table(factor1))
with(dat2, table(factor2))

##### CHECK FACTORS 1 AND 2 #####
### If factor1 is blank, then factor2 should also be blank
# If not, then paste factor 2 into space for factor1
with(dat2, factor1 == "" & factor2 != "") # looks ok
dat2$suspect <- ifelse(dat2$factor1 == "" & dat2$factor2 != "", "bad", "good")

# Don't need to use the following
# dat2$factor1 <- with(dat2, ifelse(suspect == "good", 
#                                     as.character(factor1), as.character(factor2)))

##### COMBINE FACTORS 1 AND 2 #####
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

### create new names for factor2 column
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

### Get factor 1 and factor 2 in the same column
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

##### GENERALIZED SCRIPT TO GET PERCENTAGES #####
### Rename relevant dataframe
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

##### CALCULATE MEANS AND ERROR #####

detach("package:dplyr", unload = TRUE)

### Get summary data
examples <- summarySE(tbl3, measurevar = "per", groupvars = "factorAll")
examples

### Add a row for management
# need to change examples column to a string (not factor)
extraRow <- data.frame(factorAll = "Management", N = 6, per = 0, sd = 0, 
                       se = 0, ci = 0)
examples <- rbind(examples, extraRow)

##### PREPARE FOR PLOTTING #####
# Reorder based on max to min, except for other
unique(examples$factorAll)
newFactorOrder <- rev(c("Remaining biogenic habitat", "Recruitment or connectivity", 
                        "Physical setting", "Genetic diversity", 
                        "Functional diversity", "Species interactions",
                         "Remoteness", "Management", "Other"))

examples$factor2 <- factor(examples$factorAll, levels = newFactorOrder)
examples
