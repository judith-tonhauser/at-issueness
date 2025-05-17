# EXP2 Asking whether
# preprocessing

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source('../../../helpers.R')

# load required packages for pre-processing data
library(tidyverse)

theme_set(theme_bw())

# read in the raw data
d = read_csv("../data/combined.csv")

# remove rows with info about instructions and final screens
d <- d %>% filter(!(trial_type %in% c("survey-text", "html-button-response")))

# replace participant_id by random number
length(unique(d$participant_id)) #80
d$participantID <- match(d$participant_id, unique(sort(d$participant_id)))

# how many participants?
length(unique(d$participantID)) #80

# select relevant columns
d = d %>%
  select(c(stimulus, response, trial_index, trial_type, participantID))

# unpack demographics info
dg <- d %>%
  filter(trial_type == "survey") %>%
  select(c(participantID,response))
#view(dg)

table(dg$response)

# age
dg$age = as.numeric(gsub("\\D", "", dg$response))
table(dg$age) #ages between 20 and 74
mean(dg$age)

# gender
dg$gender = case_when(grepl("female", dg$response) ~ "female",
                      grepl("male", dg$response) ~ "male",
                      grepl("non-binary", dg$response) ~ "non-binary",
                      TRUE ~ "preferNoToSay")
table(dg$gender)

# female          male    non-binary preferNoToSay 
#48              30            1          1

# language
dg$language = case_when(grepl("language\":\"yes", dg$response) ~ "English",
                      TRUE ~ "notSpeakerOfEnglish")
table(dg$language) #80 English speakers

# American English
dg$amE = case_when(grepl("amE\":\"yes", dg$response) ~ "AmE",
                        TRUE ~ "notAmE")
table(dg$amE) #2 non-Ame speakers

# comments
dg$comments = gsub(".*comments", "", dg$response)
table(dg$comments)
                      
# remove response column from demographics data
dg = dg %>%
  select(-c(response))
summary(dg)

# add demographics data back to data
d = left_join(d, dg, by = "participantID")

# remove unneeded columns from data
d = d %>%
  filter(trial_type != "survey")

d = d %>%
  select(-c(trial_index,trial_type))

# make response a numeric column, with values between 0 and 1
summary(d$response)
d$response <- as.numeric(d$response)
d$response <- d$response/100
table(d$response)

# create useful columns
d$expression <- case_when(
  grepl("right", d$stimulus) ~ "be right",
  grepl("know", d$stimulus) ~ "know",
  grepl("discover", d$stimulus) ~ "discover",
  grepl("confirm", d$stimulus) ~ "confirm",
  grepl("confess", d$stimulus) ~ "confess",
  grepl("French course", d$stimulus) ~ "controlGood",
  grepl("loves apples", d$stimulus) ~ "controlBad",
  grepl("find Jack|criticize|manager|forgive|chef|boss|neighbor", d$stimulus) ~ "fnrrc",
  grepl("run away|feel guilty|caught by the police|sweet tooth|apologize|feel upset|driver's license", d$stimulus) ~ "mnrrc",
  TRUE ~ "ERROR"
)
table(d$expression)

# items
d$cc = case_when(grepl("saw the murder", d$stimulus) ~ "saw the murder",
                         grepl("cheated on his wife", d$stimulus) ~ "cheated on his wife",
                         grepl("stole the money", d$stimulus) ~ "stole the money",
                         grepl("ate the last cupcake", d$stimulus) ~ "ate the last cupcake",
                         grepl("broke the plate", d$stimulus) ~ "broke the plate",
                         grepl("lost his key", d$stimulus) ~ "lost his key",
                         grepl("bought a new car", d$stimulus) ~ "bought a new car",
                         grepl("French course", d$stimulus) ~ "control",
                         grepl("loves apples",d$stimulus) ~ "control",
                         TRUE ~ "ERROR")
table(d$cc) 

# remove stimulus column now that everything has been extracted from it
d = d %>%
  select(-c(stimulus))

# check that all predicates and all contents are being presented to participants
table(d$expression) 
table(d$cc) 
table(d$expression, d$cc)

  
# participant info
table(d$age) #20-74
length(which(is.na(d$age))) # 0 missing values
# exclude outliers (0, 3330) before calculating mean
mean(d$age) #38.53

d %>% 
  select(gender, participantID) %>% 
  unique() %>% 
  group_by(gender) %>% 
  summarize(count=n())

# 1 female          48
# 2 male            30
# 3 non-binary      1
# 4 preferNoToSay   1


### exclude non-English speakers and non-American English speakers
# exclude non-English speakers
length(which(is.na(d$language))) #no missing responses
table(d$language) 

d <- d %>%
  filter(language != "notSpeakerOfEnglish") %>%  droplevels()
length(unique(d$participantID)) #80 #0 participants excluded

# exclude non-American English speakers
length(which(is.na(d$amE))) #2 
table(d$amE) 

d <- d %>%
  filter(amE != "notAmE") %>%  droplevels()
length(unique(d$participantID)) #78 # 2 participants excluded

# exclude participants based on controls
names(d)
table(d$expression)

# plot response by type of control
ggplot(data=d[d$expression == "controlGood",], aes(x=participantID, y=response)) +
  geom_point() 
ggplot(data=d[d$expression == "controlBad",], aes(x=participantID, y=response)) +
  geom_point() 

# good control
goodControl = d %>%
  filter(expression == "controlGood")
nrow(goodControl) #78 participants 

# identify participants whose response to the good control is more than 2sd lower than the mean
mean(goodControl$response) # .96
mean(goodControl$response) - 2*sd(goodControl$response) # 0.637682
outliers.good = goodControl[goodControl$response < (mean(goodControl$response) - 2*sd(goodControl$response)),]
nrow(outliers.good)
# 2 participants
# 71 37

# bad control
badControl = d %>%
  filter(expression == "controlBad")
nrow(badControl) #78 participants 

# identify participants whose response to the bad control is more than 2sd higher than the mean
mean(badControl$response) # .017
mean(badControl$response) + 2*sd(badControl$response) # 0.2433328
outliers.bad = badControl[badControl$response > (mean(badControl$response) + 2*sd(badControl$response)),]
nrow(outliers.bad)
# 2 participants
# 80 69

# remove the participants who got either one of the controls wrong
d = d %>%
  filter(!(participantID %in% outliers.good$participantID)) %>%
  filter(!(participantID %in% outliers.bad$participantID))
length(unique(d$participantID)) # 74 (so 4 participants excluded)

# adjust the names of the expressions
d = d %>%
  mutate(expression = recode(expression, "right" = "be right", "controlBad" = "NAI MC", "controlGood" = "AI MC",
                             "mnrrc" = "medial NRRC", "fnrrc" = "final NRRC"))


# age and gender of remaining participants
table(d$age) #22-74
length(which(is.na(d$age))) # 0 missing values
# exclude outliers (0, 3330) before calculating mean
mean(d$age) #38.7

d %>% 
  select(gender, participantID) %>% 
  unique() %>% 
  group_by(gender) %>% 
  summarize(count=n())
# 1 female          45
# 2 male            27
# 3 non-binary      1
# 4 preferNoToSay   1

nrow(d)

# code response such that 1 = not-at-issue and 0 = at-issue
d$response = 1-d$response
table(d$response)

means.exp2 = d %>%
  filter(!(expression == "AI MC" | expression == "NAI MC")) %>%
  group_by(expression) %>%
  summarize(Mean = mean(response))
means.exp2

write_csv(d, file="../data/cd.csv")

