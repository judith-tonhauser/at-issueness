# at-issueness exp 1A
# preprocessing

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source('../../../helpers.R')

# load required packages for pre-processing data
library(tidyverse)
library(readr)

theme_set(theme_bw())

# read in the raw data
d = read_tsv("../data/combined.tsv")

# remove instructions rows
d = d %>% 
  filter(trial_index != "0")

# replace participant_id by random number
length(unique(d$participant_id)) #
d$participantID <- match(d$participant_id, unique(sort(d$participant_id)))

# select needed columns
d = d %>%
  select(c(stimulus, response, trial_index, participantID))

# unpack demographics info
dg <- d %>%
  filter(trial_index == "11") %>%
  select(c(participantID,response))
#view(dg)

table(dg$response)

# age
dg$age = as.numeric(gsub("\\D", "", dg$response))
table(dg$age)

# gender
dg$gender = case_when(grepl("female", dg$response) ~ "female",
                      grepl("male", dg$response) ~ "male",
                      grepl("non-binary", dg$response) ~ "non-binary",
                      TRUE ~ "preferNoToSay")
table(dg$gender)

# language
dg$language = case_when(grepl("language\"\":\"\"yes", dg$response) ~ "English",
                      TRUE ~ "notSpeakerOfEnglish")
table(dg$language)

# American English
dg$amE = case_when(grepl("amE\"\":\"\"yes", dg$response) ~ "AmE",
                        TRUE ~ "notAmE")
table(dg$amE)

# comments
dg$comments = gsub(".*comments", "", dg$response)
table(dg$comments)
                      
# remove response column from demographics data
dg = dg %>%
  select(-c(response))
summary(dg)

# add demographics data back to data
d = left_join(d, dg, by = "participantID")

# remove demographics rows
d = d %>%
  filter(trial_index != 11)
#view(d)

# make response a numeric column, with values between 0 and 1
summary(d$response)
d$response <- as.numeric(d$response)
d$response <- d$response/100
table(d$response)

# create useful columns from "stimulus" column
table(d$stimulus)

# target vs control
d$target = case_when(grepl("Ann, who",d$stimulus) ~ "goodControl1",
                     grepl("Samantha, who",d$stimulus) ~ "goodControl2",
                     grepl("John's kids",d$stimulus) ~ "badControl1",
                     grepl("blueberries",d$stimulus) ~ "badControl2",
                     TRUE ~ "target")
table(d$target)

# predicate
d$predicate = case_when(grepl("is right that", d$stimulus) ~ "be right",
  grepl("knows that", d$stimulus) ~ "know",
  grepl("said", d$stimulus) ~ "say",
  grepl("discovered", d$stimulus) ~ "discover",
  grepl("thinks", d$stimulus) ~ "think",
  grepl("confessed",d$stimulus) ~ "confess",
  TRUE ~ "control")
table(d$predicate)  

# CC
d$cc = case_when(grepl("studied", d$stimulus) ~ "Emma studied on Saturday morning",
                        grepl("had a drink", d$stimulus) ~ "Tony had a drink last night",
                        grepl("cupcake", d$stimulus) ~ "Danny ate the last cupcake",
                        grepl("cocktails", d$stimulus) ~ "Mia drank two cocktails last night",
                        grepl("miles", d$stimulus) ~ "Jackson ran 10 miles",
                        grepl("tattoo",d$stimulus) ~ "Sophia got a tattoo",
                        TRUE ~ "control")
table(d$cc)
view(d)

# what is at-issue?
# "why"-question: main clause at-issue
# "what"-question: cc at-issue, but some controls also have "what"-question
d$ai = case_when(grepl("Why", d$stimulus) ~ "mc",
                 grepl("blueberries", d$stimulus) ~ "control",
                 grepl("Ann", d$stimulus) ~ "control",
                 grepl("Samantha", d$stimulus) ~ "control",
                 grepl("garage", d$stimulus) ~ "control",
                 TRUE ~ "cc")
table(d$ai)

# remove stimulus column now that everything has been extracted from it
d = d %>%
  select(-c(stimulus))

# check that all predicates and all contents are being presented to participants
table(d$predicate) #6 predicates
table(d$cc) # 6 contents
table(d$predicate, d$cc)

# did every participant see both cc and mc at-issueness?
count = d %>%
  filter(target == "target") %>%
  group_by(participantID, ai) %>%
  summarize(count = n())
count
  
# participant info
table(d$age) #xx-xx
length(which(is.na(d$age))) # 0 missing values
# exclude outliers (0, 3330) before calculating mean
#mean(d[10 < d$age & d$age < 100,]$age,na.rm=TRUE) #40.8

d %>% 
  select(gender, participantID) %>% 
  unique() %>% 
  group_by(gender) %>% 
  summarize(count=n())

# 1 female          xx
# 2 male            xx
# 3 non-binary        xx
# 4 preferNoToSay     x

### exclude non-English speakers and non-American English speakers
# exclude non-English speakers
length(which(is.na(d$language))) #no missing responses
table(d$language) 

d <- d %>%
  filter(language != "notSpeakerOfEnglish") %>%  droplevels()
length(unique(d$participantID)) #

# exclude non-American English speakers
length(which(is.na(d$amE))) #0 (everybody responded)
table(d$amE) 

d <- d %>%
  filter(amE != "notAmE") %>%  droplevels()
length(unique(d$participantID)) #

# change response from character to numeric
d$response <- as.numeric(d$response)
str(d$response)

# exclude participants based on controls
table(d$predicate)

# good controls
controls.good = d %>%
  filter(target == "goodControl1" | target == "goodControl2")
table(controls.good$target)

# plot responses by good controls, to see if they worked as expected
good.means = controls.good %>%
  group_by(target) %>%
  summarize(Mean = mean(response),CI.Low = ci.low(response), CI.High=ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin = Mean-CI.Low, YMax = Mean+CI.High)
good.means

ggplot(good.means, aes(x=target,y=Mean)) +
  geom_point() +
  geom_errorbar(aes(ymin=YMin, ymax=YMax))+
  ylab("Mean response")

# plot responses by participant
good.means.p = controls.good %>%
  group_by(participantID) %>%
  summarize(Mean = mean(response),CI.Low = ci.low(response), CI.High=ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin = Mean-CI.Low, YMax = Mean+CI.High)
good.means.p

# mean response across good controls
round(mean(good.means.p$Mean),2) #.xx

ggplot(good.means.p, aes(x=participantID,y=Mean)) +
  geom_point() +
  geom_errorbar(aes(ymin=YMin, ymax=YMax))+
  #geom_text(aes(label=participantID), vjust = 1, cex= 5)+
  ylab("Mean response")

# get the participants whose response to the good controls is more than 2 sd below group mean
tmp.good <- good.means.p[good.means.p$Mean < (mean(good.means.p$Mean) - 2*sd(good.means.p$Mean)),]
tmp.good
length(unique(tmp.good$participantID)) #xx participants

# bad controls
controls.bad = d %>%
  filter(target == "badControl1" | target == "badControl2")
table(controls.bad$target)

# plot responses by good controls, to see if they worked as expected
bad.means = controls.bad %>%
  group_by(target) %>%
  summarize(Mean = mean(response),CI.Low = ci.low(response), CI.High=ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin = Mean-CI.Low, YMax = Mean+CI.High)
bad.means

ggplot(bad.means, aes(x=target,y=Mean)) +
  geom_point() +
  geom_errorbar(aes(ymin=YMin, ymax=YMax))+
  ylab("Mean response")

# plot responses by participant
bad.means.p = controls.bad %>%
  group_by(participantID) %>%
  summarize(Mean = mean(response),CI.Low = ci.low(response), CI.High=ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin = Mean-CI.Low, YMax = Mean+CI.High)
bad.means.p

# mean response across bad controls
round(mean(bad.means.p$Mean),2) #.xx

ggplot(bad.means.p, aes(x=participantID,y=Mean)) +
  geom_point() +
  geom_errorbar(aes(ymin=YMin, ymax=YMax))+
  #geom_text(aes(label=participantID), vjust = 1, cex= 5)+
  ylab("Mean response")

# get the participants whose response to the bad controls is more than 2 sd above group mean
tmp.bad <- bad.means.p[bad.means.p$Mean < (mean(bad.means.p$Mean) - 2*sd(bad.means.p$Mean)),]
tmp.bad
length(unique(tmp.bad$participantID)) #xx participants

# exclude all participants identified above
d <- d %>%
  filter(!(participantID %in% tmp.good$participantID)) %>%
  filter(!(participantID %in% tmp.bad$participantID)) %>%
  droplevels()
length(unique(d$participantID)) #370, so 23 participants excluded

# exclude participants who always clicked on roughly the same point on the scale 
# on the target trials
variances = d %>%
  filter(target == "target") %>%
  group_by(participantID) %>%
  summarize(Variance = var(response)) %>%
  mutate(TooSmall = Variance < mean(Variance) - 2*sd(Variance))
variances

lowvarworkers = as.character(variances[variances$TooSmall,]$participantID)
summary(variances)
lowvarworkers # 0 participants consistently clicked on roughly the same point on the scale

# lvw = d %>%
#   filter(as.character(participantID) %in% lowvarworkers) %>%
#   droplevels() %>%
#   mutate(Participant = as.factor(as.character(participantID)))
# 
# ggplot(lvw,aes(x=Participant,y=response)) +
#   geom_point()
# 
# # exclude the participants identified above
# d <- droplevels(subset(d, !(d$participantID %in% lowvarworkers)))
# length(unique(d$participantID)) #

# age and gender of remaining participants
table(d$age) #19-80
length(which(is.na(d$age))) # 0 missing values
# exclude outliers (0, 3330) before calculating mean
#mean(d[10 < d$age & d$age < 100,]$age,na.rm=TRUE) #40.7

d %>% 
  select(gender, participantID) %>% 
  unique() %>% 
  group_by(gender) %>% 
  summarize(count=n())
# 1 female          xx
# 2 male            xx
# 3 non-binary        x
# 4 preferNoToSay     x

write_tsv(d, file="../data/d.tsv")

