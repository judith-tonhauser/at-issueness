# EXP2 Asking whether
# preprocessing

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source('../../../helpers.R')

# load required packages for pre-processing data
library(tidyverse)
library(readr)
library(dplyr)
library(stringr)

theme_set(theme_bw())

# read in the raw data
d = read_csv("../data/combined.csv")

# parsing warnings are due to the rows with demographics info and final screens (ignore)

# bind the data
#d = rbind(d1,d2)

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

# make response a numeric column, with values between 0 and 1
#view(d)
d = d %>%
  filter(trial_type != "survey")
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
  grepl("French course", d$stimulus) ~ "controlGood1",
  grepl("loves apples", d$stimulus) ~ "controlGood2",
  grepl("find Jack|criticize|manager|forgive|chef|boss|neighbor", d$stimulus) ~ "fnrrc",
  grepl("run away|feel guilty|caught by the police|sweet tooth|apologize|feel upset|driver's license", d$stimulus) ~ "mnrrc",
  TRUE ~ "practice"
)

# Create a frequency table of the new expression column
table(d$expression)


d$cc = case_when(grepl("saw the murder", d$stimulus) ~ "saw the murder",
                         grepl("cheated on his wife", d$stimulus) ~ "cheated on his wife",
                         grepl("stole the money", d$stimulus) ~ "stole the money",
                         grepl("ate the last cupcake", d$stimulus) ~ "ate the last cupcake",
                         grepl("broke the plate", d$stimulus) ~ "broke the plate",
                         grepl("lost his key", d$stimulus) ~ "lost his key",
                         grepl("bought a new car", d$stimulus) ~ "bought a new car",
                         TRUE ~ "noCC")
table(d$cc) 



# remove stimulus column now that everything has been extracted from it
d = d %>%
  select(-c(stimulus))

# check that all predicates and all contents are being presented to participants
table(d$expression) #5predicates, 2nrrcs, 2 control
table(d$cc) # 7 lexical contents
table(d$expression, d$cc)

  
# participant info
table(d$age) #20-74
length(which(is.na(d$age))) # 0 missing values
# exclude outliers (0, 3330) before calculating mean
mean(d[10 < d$age & d$age < 100,]$age,na.rm=TRUE) #38.53

d %>% 
  select(gender, participantID) %>% 
  unique() %>% 
  group_by(gender) %>% 
  summarize(count=n())

# 1 female          48
# 2 male            30
# 3 non-binary      1
# 4 preferNoToSay   1

# how many trials per participant?
 view(d) 

trialCount = d %>% 
  select(trial_index, participantID) %>% 
  unique() %>% 
  group_by(participantID) %>% 
  summarize(count=n())
trialCount
# participants who had to redo practice trials have different trial numbers!

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


# change response from character to numeric
d$response <- as.numeric(d$response)

# exclude participants based on good controls
names(d)
table(d$expression)

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
c.means #our controls are perfect!


ggplot(c.means, aes(x=expression,y=Mean)) +
  geom_point() +
  geom_errorbar(aes(ymin=YMin, ymax=YMax))+
  #geom_text(aes(label=participantID), vjust = 1, cex= 5)+
  ylab("Mean response")


# good controls
controls = d %>%
  filter(expression == "controlGood1" | expression == "controlGood2")
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
round(mean(controls$response),2) #.49

ggplot(c.means, aes(x=participantID,y=Mean)) +
  geom_point() +
  geom_errorbar(aes(ymin=YMin, ymax=YMax))+
  #geom_text(aes(label=participantID), vjust = 1, cex= 5)+
  ylab("Mean response")

# get the participants whose response to the good controls is more than 2 sd below group mean
# get the participants whose response to the good controls is either more than 2 sd below or above group mean
tmp <- c.means[c.means$Mean < (mean(c.means$Mean) - 2*sd(c.means$Mean)) | 
                 c.means$Mean > (mean(c.means$Mean) + 2 * sd(c.means$Mean)),]
tmp
length(unique(tmp$participantID)) #3 participants do not pass the controls


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
length(unique(d$participantID)) #75, so 3 participants excluded


# age and gender of remaining participants
table(d$age) #20-74
length(which(is.na(d$age))) # 0 missing values
# exclude outliers (0, 3330) before calculating mean
mean(d[10 < d$age & d$age < 100,]$age,na.rm=TRUE) #38.41

d %>% 
  select(gender, participantID) %>% 
  unique() %>% 
  group_by(gender) %>% 
  summarize(count=n())
# 1 female          46
# 2 male            27
# 3 non-binary      1
# 4 preferNoToSay   1

#write_csv(d, file="../data/cd.csv")

# how many data points per predicate/CC/context combination?
names(d)

# exclude the fillers and the controls
tmp = d %>%
  filter(cc != "noCC") #%>%
  #group_by(expression,cc) %>% 
  #tally
#view(tmp)
write_csv(tmp, file="../data/cd.csv")
