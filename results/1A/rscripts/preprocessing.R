# at-issueness exp 1A
# preprocessing

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source('../../helpers.R')

# load required packages for pre-processing data
library(tidyverse)
library(readr)

theme_set(theme_bw())

# read in the raw data
d = read_tsv("../data/combined.tsv")
# parsing warnings are due to the rows with demographics info and final screens

# remove rows with info about instructions and final screens
d = d %>% drop_na(task)

# replace participant_id by random number
length(unique(d$participant_id)) #398
d$participantID <- match(d$participant_id, unique(sort(d$participant_id)))

# how many participants?
length(unique(d$participantID)) #398

# from Prolific
# £12.66/hr
# median time: 00:06:24

# select relevant columns
d = d %>%
  select(c(stimulus, response, trial_index, task, participantID))

# unpack demographics info
dg <- d %>%
  filter(task == "demographics") %>%
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

# female          male    non-binary preferNoToSay 
#187           201             8             2

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

# make response a numeric column, with values between 0 and 1
#view(d)
d = d %>%
  filter(task != "demographics")
summary(d$response)
d$response <- as.numeric(d$response)
d$response <- d$response/100
table(d$response)

# create useful columns
d$expression = case_when(grepl("annoyed", d$stimulus) ~ "annoyed",
  grepl("know that", d$stimulus) ~ "know",
  grepl("discover", d$stimulus) ~ "discover",
  grepl("reveal", d$stimulus) ~ "reveal",
  grepl("see", d$stimulus) ~ "see",
  grepl("establish", d$stimulus) ~ "establish",
  grepl("pretend", d$stimulus) ~ "pretend",
  grepl("think", d$stimulus) ~ "think",
  grepl("suggest", d$stimulus) ~ "suggest",
  grepl("prove", d$stimulus) ~ "prove",
  grepl("demonstrate", d$stimulus) ~ "demonstrate",
  grepl("say", d$stimulus) ~ "say",
  grepl("hear", d$stimulus) ~ "hear",
  grepl("confess", d$stimulus) ~ "confess", 
  grepl("inform", d$stimulus) ~ "inform", 
  grepl("announce", d$stimulus) ~ "announce", 
  grepl("acknowledge", d$stimulus) ~ "acknowledge", 
  grepl("admit", d$stimulus) ~ "admit", 
  grepl("confirm", d$stimulus) ~ "confirm",
  grepl("right", d$stimulus) ~ "right",
  grepl("also", d$stimulus) ~ "also",
  grepl("too", d$stimulus) ~ "too",
  grepl("it Jack", d$stimulus) ~ "cleft",
  grepl("vaping", d$stimulus) ~ "stop",
  grepl("history again", d$stimulus) ~ "again",
  grepl("Dune", d$stimulus) ~ "continue",
  grepl("new hat", d$stimulus) ~ "controlGood1",
  grepl("this pizza", d$stimulus) ~ "controlGood2",
  grepl("Hendrick's car", d$stimulus) ~ "controlGood3",
  grepl("Mary's aunt", d$stimulus) ~ "controlGood4",
  TRUE ~ "practice")
table(d$expression)   

d$cc = case_when(grepl("Charley speaks Spanish", d$stimulus) ~ "Charley speaks Spanish",
                         grepl("Jon walks to work", d$stimulus) ~ "Jon walks to work",
                         grepl("Julian dances salsa", d$stimulus) ~ "Julian dances salsa",
                         grepl("Owen shoveled snow last winter", d$stimulus) ~ "Owen shoveled snow last winter",
                         grepl("Josh learned to ride a bike yesterday", d$stimulus) ~ "Josh learned to ride a bike yesterday",
                         grepl("Tony had a drink last night", d$stimulus) ~ "Tony had a drink last night",
                         grepl("Jayden rented a car", d$stimulus) ~ "Jayden rented a car",
                         grepl("Jackson ran 10 miles", d$stimulus) ~ "Jackson ran 10 miles",
                         grepl("Frank got a cat", d$stimulus) ~ "Frank got a cat",
                         grepl("Zoe calculated the tip", d$stimulus) ~ "Zoe calculated the tip",
                         grepl("Danny ate the last cupcake", d$stimulus) ~ "Danny ate the last cupcake",
                         grepl("Grace visited her sister", d$stimulus) ~ "Grace visited her sister",
                         grepl("Emily bought a car yesterday", d$stimulus) ~ "Emily bought a car yesterday",
                         grepl("Isabella ate a steak on Sunday", d$stimulus) ~ "Isabella ate a steak on Sunday", 
                         grepl("Mia drank 2 cocktails last night", d$stimulus) ~ "Mia drank 2 cocktails last night", 
                         grepl("Sophia got a tattoo", d$stimulus) ~ "Sophia got a tattoo", 
                         grepl("Olivia sleeps until noon", d$stimulus) ~ "Olivia sleeps until noon", 
                         grepl("Emma studied on Saturday morning", d$stimulus) ~ "Emma studied on Saturday morning", 
                         grepl("Josie went on vacation to France", d$stimulus) ~ "Josie went on vacation to France",
                         grepl("Mary is pregnant", d$stimulus) ~ "Mary is pregnant",
                         TRUE ~ "noCC")
table(d$cc) 

d$context = case_when(grepl("I don't know if", d$stimulus) ~ "explicitIgnorance",
                 grepl("Mary is taking a prenatal yoga class", d$stimulus) ~ "factH",
                 grepl("Mary is a middle school student", d$stimulus) ~ "factL",
                 grepl("Josie loves France", d$stimulus) ~ "factH",
                 grepl("Josie doesn't have a passport", d$stimulus) ~ "factL",
                 grepl("Emma is in law school", d$stimulus) ~ "factH",
                 grepl("Emma is in first grade", d$stimulus) ~ "factL",
                 grepl("Olivia works the third shift", d$stimulus) ~ "factH",
                 grepl("Olivia has two small children", d$stimulus) ~ "factL",
                 grepl("Sophia is a hipster", d$stimulus) ~ "factH",
                 grepl("Sophia is a high end fashion model", d$stimulus) ~ "factL",
                 grepl("Mia is a college student", d$stimulus) ~ "factH",
                 grepl("Mia is a nun", d$stimulus) ~ "factL",
                 grepl("Isabella is from Argentina", d$stimulus) ~ "factH",
                 grepl("Isabella is a vegetarian", d$stimulus) ~ "factL",
                 grepl("Emily has been saving for a year", d$stimulus) ~ "factH",
                 grepl("Emily never has any money", d$stimulus) ~ "factL",
                 grepl("Grace loves her sister", d$stimulus) ~ "factH",
                 grepl("Grace hates her sister", d$stimulus) ~ "factL",
                 grepl("Zoe is a math major", d$stimulus) ~ "factH",
                 grepl("Zoe is 5 years old", d$stimulus) ~ "factL",
                 grepl("Danny loves cake", d$stimulus) ~ "factH",
                 grepl("Danny is a diabetic", d$stimulus) ~ "factL",
                 grepl("Frank has always wanted a pet", d$stimulus) ~ "factH",
                 grepl("Frank is allergic to cats", d$stimulus) ~ "factL",
                 grepl("Jackson is training for a marathon", d$stimulus) ~ "factH",
                 grepl("Jackson is obese", d$stimulus) ~ "factL",
                 grepl("Jayden's car is in the shop", d$stimulus) ~ "factH",
                 grepl("Jayden doesn't have a driver's license", d$stimulus) ~ "factL",
                 grepl("Tony really likes to party with his friends", d$stimulus) ~ "factH",
                 grepl("Tony has been sober for 20 years", d$stimulus) ~ "factL",
                 grepl("Josh is a 5-year old boy", d$stimulus) ~ "factH",
                 grepl("Josh is a 75-year old man", d$stimulus) ~ "factL",
                 grepl("Owen lives in Chicago", d$stimulus) ~ "factH",
                 grepl("Owen lives in New Orleans", d$stimulus) ~ "factL",
                 grepl("Julian is Cuban", d$stimulus) ~ "factH",
                 grepl("Julian is German", d$stimulus) ~ "factL",
                 grepl("Jon lives 2 blocks away from work", d$stimulus) ~ "factH",
                 grepl("Jon lives 10 miles away from work", d$stimulus) ~ "factL",
                 grepl("Charley lives in Mexico", d$stimulus) ~ "factH",
                 grepl("Charley lives in Korea", d$stimulus) ~ "factL",
                 TRUE ~ "other")
table(d$context) 

# remove stimulus column now that everything has been extracted from it
d = d %>%
  select(-c(stimulus))

# check that all predicates and all contents are being presented to participants
table(d$expression) #20 predicates
table(d$cc) # 20 contents
table(d$expression, d$cc)

# check that contexts and expressions are randomly combined
table(d$context, d$expression) # good

# did every participant see 12 predicate/CC combinations in explicit ignorance context
# and 8 in neutral context?
count = d %>%
  filter(task == "target") %>%
  filter(expression != "controlGood1" & expression != "controlGood2" &
           expression != "controlGood3" & expression != "controlGood4"
         & expression != "again"
         & expression != "also" & expression != "too" & expression != "continue"
         & expression != "stop" & expression != "cleft") %>%
  group_by(participantID, context) %>%
  summarize(count = n())
count

  
# participant info
table(d$age) #19-80
length(which(is.na(d$age))) # 0 missing values
# exclude outliers (0, 3330) before calculating mean
mean(d[10 < d$age & d$age < 100,]$age,na.rm=TRUE) #40.8

d %>% 
  select(gender, participantID) %>% 
  unique() %>% 
  group_by(gender) %>% 
  summarize(count=n())

# 1 female          187
# 2 male            201
# 3 non-binary        8
# 4 preferNoToSay     2

# how many trials per participant?
# view(d) 

trialCount = d %>% 
  select(trial_index, participantID) %>% 
  unique() %>% 
  group_by(participantID) %>% 
  summarize(count=n())
trialCount
# participants who had to redo practice trials have different trial numbers!

# # create separate data for practice trials
# table(d$task)
# practice = d %>%
#   filter(task == "practice") %>%
#   select(c(participantID, response, trial_index))
# practice
# # participants who did the 4 practice trials without errors should have alternating
# # values above 60, below 40, above 60, below 40
# 
# # remove practice trials from data
# d = d %>%
#   filter(task != "practice")
# view(d)
# 
# # how many target trials per participant?
# trialCount = d %>% 
#   select(trial_index, participantID) %>% 
#   unique() %>% 
#   group_by(participantID) %>% 
#   summarize(count=n())
# trialCount # now everybody has 30 trials
# 
# # make a trial number
# table(d$trial_index, d$participantID) 
# table(d$participantID,d$trial_index) 
# # participants who had to repeat practice trials have other trial numbers
# # specifically, the more often they had to repeat the practice trials
# # the later their target trials start
# 
# d$trial = d$trial_index
# table(d$trial, d$participantID)
# table(d$participantID,d$trial)
# # for participants who don't have a trial 10, subtract 1
# # for participant 3 subtract 2 for trials above 11
# d[d$participantID == 3 & d$trial > 11,]$trial = d[d$participantID == 3 & d$trial > 11,]$trial - 2
# # everybody: trials above 9 subtract 9
# d[d$trial > 9,]$trial = d[d$trial > 9,]$trial - 9
# # now everybody has 30 trials between 1 and 30
# 
# # remove original trial_index
# d = d %>%
#   select(-c(trial_index))

### exclude non-English speakers and non-American English speakers
# exclude non-English speakers
length(which(is.na(d$language))) #no missing responses
table(d$language) 

d <- d %>%
  filter(language != "notSpeakerOfEnglish") %>%  droplevels()
length(unique(d$participantID)) #396 #2 participants excluded

# exclude non-American English speakers
length(which(is.na(d$amE))) #0 (everybody responded)
table(d$amE) 

d <- d %>%
  filter(amE != "notAmE") %>%  droplevels()
length(unique(d$participantID)) #393 # 3 participants excluded


# change response from character to numeric
d$response <- as.numeric(d$response)

# exclude participants based on good controls
names(d)
table(d$expression)
table(d$task, d$expression)

# good controls
controls = d %>%
  filter(grepl("control", expression))
table(controls$expression)

# plot responses by control
c.means = controls %>%
  group_by(expression) %>%
  summarize(Mean = mean(response),CI.Low = ci.low(response), CI.High=ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin = Mean-CI.Low, YMax = Mean+CI.High)
c.means


ggplot(c.means, aes(x=expression,y=Mean)) +
  geom_point() +
  geom_errorbar(aes(ymin=YMin, ymax=YMax))+
  #geom_text(aes(label=participantID), vjust = 1, cex= 5)+
  ylab("Mean response")

# controlGood3 didn't work: here the mean naturalness rating is way lower
# makes sense (in hindsight: question presupposes that he has a car, but the statement says
# that he was looking to buy one)

# "item_id" : "controlGood1",
# "utterance" : "Does Samantha have a new hat?",
# "fact" : "I don't know if Samantha has a new hat"
# },
# {
#   "item_id" : "controlGood2",
#   "utterance" : "Does this pizza have mushrooms on it?",
#   "fact" : "I don't know if this pizza has mushrooms on it"
# },
# {
#   "item_id" : "controlGood3",
#   "utterance" : "Was Hendrick's car expensive?",
#   "fact" : "Hendrick was looking to buy a car"
# },
# {
#   "item_id" : "controlGood4",
#   "utterance" : "Is Mary's aunt sick?",
#   "fact" : "Mary visited her aunt yesterday"

# don't use controlGood3 to exclude participants data

# good controls
controls = d %>%
  filter(expression == "controlGood1" | expression == "controlGood2" | expression == "controlGood4")
table(controls$expression)

# plot responses by control
c.means = controls %>%
  group_by(expression) %>%
  summarize(Mean = mean(response),CI.Low = ci.low(response), CI.High=ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin = Mean-CI.Low, YMax = Mean+CI.High)
c.means


ggplot(c.means, aes(x=expression,y=Mean)) +
  geom_point() +
  geom_errorbar(aes(ymin=YMin, ymax=YMax))+
  #geom_text(aes(label=participantID), vjust = 1, cex= 5)+
  ylab("Mean response")

# plot responses by participant
c.means = controls %>%
  group_by(participantID) %>%
  summarize(Mean = mean(response),CI.Low = ci.low(response), CI.High=ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin = Mean-CI.Low, YMax = Mean+CI.High)
c.means

# mean response across good controls
round(mean(controls$response),2) #.86

ggplot(c.means, aes(x=participantID,y=Mean)) +
  geom_point() +
  geom_errorbar(aes(ymin=YMin, ymax=YMax))+
  #geom_text(aes(label=participantID), vjust = 1, cex= 5)+
  ylab("Mean response")

# get the participants whose response to the good controls is more than 2 sd below group mean
tmp <- c.means[c.means$Mean < (mean(c.means$Mean) - 2*sd(c.means$Mean)),]
tmp
length(unique(tmp$participantID)) #23 participants

# look at the main clauses that these "outlier" Turkers did
# make data subset of just the outliers
outliers <- tmp %>%
  filter(participantID %in% tmp$participantID)
outliers = droplevels(outliers)

ggplot(outliers, aes(x=participantID,y=Mean)) +
  geom_point() +
  geom_errorbar(aes(ymin=YMin, ymax=YMax))+
  #geom_text(aes(label=participantID), vjust = 1, cex= 5)+
  ylab("Mean response to good controls")

# exclude all outliers identified above
d <- d %>%
  filter(!(participantID %in% outliers$participantID)) %>%
  droplevels()
length(unique(d$participantID)) #370, so 23 participants excluded

# exclude turkers who always clicked on roughly the same point on the scale 
# on the target trials
table(d$task)
variances = d %>%
  filter(task == "target") %>%
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

# adjust the names of the predicates
d = d %>%
  mutate(expression = recode(expression, "right" = "be right", "inform_Sam" = "inform", "annoyed" = "be annoyed"))


# age and gender of remaining participants
table(d$age) #19-80
length(which(is.na(d$age))) # 0 missing values
# exclude outliers (0, 3330) before calculating mean
mean(d[10 < d$age & d$age < 100,]$age,na.rm=TRUE) #40.7

d %>% 
  select(gender, participantID) %>% 
  unique() %>% 
  group_by(gender) %>% 
  summarize(count=n())
# 1 female          175
# 2 male            185
# 3 non-binary        8
# 4 preferNoToSay     2

write_tsv(d, file="../data/cd.tsv")

# how many data points per predicate/CC/context combination?
names(d)

tmp = d %>%
  filter(cc != "noCC") %>%
  group_by(expression,cc,context) %>% 
  tally
tmp
nrow(tmp) # 1185 (if everything had been chosen, should be 1200 = 3 contexts x 400 predicate/cc combinations)
min(tmp$n) #1
mean(tmp$n) #6.2
max(tmp$n) #23
#view(tmp)

# how many data points per predicate/context combination?
tmp = d %>%
  filter(cc != "noCC") %>%
  group_by(expression,context) %>% 
  tally
tmp
nrow(tmp) # 60 combinations (3 contexts x 20 predicates)
sd(tmp$n)
min(tmp$n) #59
mean(tmp$n) #123
median(tmp$n) #80
max(tmp$n) #234

# how many data points for each predicate in the explicit ignorance context?
tmp = d %>%
  filter(context == "explicitIgnorance") %>%
  filter(cc != "noCC") %>%
  group_by(expression,context) %>% 
  tally
tmp
nrow(tmp) # 20 combinations (1 contexts x 20 predicates)
sd(tmp$n) #8.7
min(tmp$n) #200
mean(tmp$n) #222
median(tmp$n) #224.5
max(tmp$n) #234

# how many data points for each predicate in the two neutral contexts?
tmp = d %>%
  filter(context != "explicitIgnorance") %>%
  filter(cc != "noCC") %>%
  group_by(expression,context) %>% 
  tally
tmp
nrow(tmp) # 40 combinations (2 contexts x 20 predicates)
sd(tmp$n) #8
min(tmp$n) #59
mean(tmp$n) #74
median(tmp$n) #75
max(tmp$n) #87

