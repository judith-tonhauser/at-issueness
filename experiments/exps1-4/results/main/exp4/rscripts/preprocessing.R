# Exp4 Yes, but test
# preprocessing.R

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source('helpers.R')

# load required packages
library(tidyverse)
library(dplyr)
library(stringr)
library(dichromat)
library(forcats)
library(ggrepel)
theme_set(theme_bw())

# load raw data
d = read.csv("../data/combined.csv")
head(d)
nrow(d) 
names(d)
length(unique(d$participant_id)) #525 participants
d <- d %>% filter(!(trial_type %in% c("survey-text", "html-button-response")))


# replace participant_id by random number
length(unique(d$participant_id)) #80
d$participantID <- match(d$participant_id, unique(sort(d$participant_id)))

# how many participants? 80
length(unique(d$participantID))

d = d %>%
  select(participantID,response,trial_type,trial_index)
nrow(d) #800

# unpack demographics info
dg <- d %>%
  filter(trial_type == "survey") %>%
  select(c(participantID,response))
#view(dg)

table(dg$response)

# age
dg$age = as.numeric(gsub("\\D", "", dg$response))
table(dg$age) #19-67

# gender
dg$gender = case_when(grepl("female", dg$response) ~ "female",
                      grepl("male", dg$response) ~ "male",
                      grepl("non-binary", dg$response) ~ "non-binary",
                      TRUE ~ "preferNoToSay")
table(dg$gender) #female 48, male 30, non-binary 2

# language
dg$language = case_when(grepl("language\":\"yes", dg$response) ~ "English",
                        TRUE ~ "notSpeakerOfEnglish")
table(dg$language) 

# American English
dg$amE = case_when(grepl("amE\":\"yes", dg$response) ~ "AmE",
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
view(d)

#filter the survey 
d = d %>%
  filter(trial_type != "survey")
summary(d$response)

#view(d)
#separate the question and response within the response
d = d %>% 
  separate(col = response,
           into = c("question", "answer"),
           sep = "</em><br><br><b>")
#view(d)

# create useful columns (for question)
d$expression <- case_when(
  grepl("is right", d$question) ~ "be right",
  grepl("knows", d$question) ~ "know",
  grepl("discovered", d$question) ~ "discover",
  grepl("confirmed", d$question) ~ "confirm",
  grepl("confessed", d$question) ~ "confess",
  grepl("French course", d$question) ~ "controlGood1",
  grepl("has a cat", d$question) ~ "controlGood2",
  grepl("police found|criticized|manager|forgave|chef|boss|neighbor", d$question) ~ "fnrrc",
  grepl("ran away|feels guilty|caught by the police|sweet tooth|apologized|feels upset|driver's license", d$question) ~ "mnrrc",
  TRUE ~ "practice"
)
table(d$expression)

# create useful columns (for answer)
d$choice <- case_when(
  grepl("Yes, and", d$answer) ~ "Yes, and",
  grepl("Yes, but", d$answer) ~ "Yes, but",
  grepl("No,", d$answer) ~ "No,",
  TRUE ~ "practice"
)
table(d$choice)

view(d)

# add nResponse column (numeric variant of yes/no response)
d = d %>%
  mutate(nResponse = ifelse(choice == "No,", 1, 0))

#view(d)
d$cc = case_when(grepl("saw the murder", d$question) ~ "saw the murder",
                 grepl("cheated on his wife", d$question) ~ "cheated on his wife",
                 grepl("stole the money", d$question) ~ "stole the money",
                 grepl("ate the last cupcake", d$question) ~ "ate the last cupcake",
                 grepl("broke the plate", d$question) ~ "broke the plate",
                 grepl("lost his key", d$question) ~ "lost his key",
                 grepl("bought a new car", d$question) ~ "bought a new car",
                 TRUE ~ "noCC")
table(d$cc) 

# remove question column now that everything has been extracted from it
d = d %>%
  select(-c(question))

d = d %>%
  select(-c(answer))
#view(d)

# check that all predicates and all contents are being presented to participants
table(d$expression) #5 predicates 1 mnrrc 1 fnrrc
table(d$cc) # 7 lexical contents
table(d$expression, d$cc)

# participant info
table(d$age) #19-67
length(which(is.na(d$age))) # 0 missing values
# exclude outliers (0, 3330) before calculating mean
mean(d[10 < d$age & d$age < 100,]$age,na.rm=TRUE) #37.99

d %>% 
  select(gender, participantID) %>% 
  unique() %>% 
  group_by(gender) %>% 
  summarize(count=n())

# 1 female          48
# 2 male            30
# 3 non-binary      2
# 4 preferNoToSay   0


#view(d) 

trialCount = d %>% 
  select(trial_index, participantID) %>% 
  unique() %>% 
  group_by(participantID) %>% 
  summarize(count=n())
trialCount

### exclude non-English speakers and non-American English speakers
# exclude non-English speakers
length(which(is.na(d$language))) #no missing responses
table(d$language) 

d <- d %>%
  filter(language != "notSpeakerOfEnglish") %>%  droplevels()
length(unique(d$participantID)) #78 #2 participants excluded

# exclude non-American English speakers
length(which(is.na(d$amE))) 
table(d$amE) 

d <- d %>%
  filter(amE != "notAmE") %>%  droplevels()
length(unique(d$participantID)) #76 # 2 participants excluded

# exclude participants based on good controls
names(d)
table(d$expression)

# good controls
controls = d %>%
  filter(grepl("control", expression))
table(controls$expression)

# make control c1 data subset
c1 <- subset(d, d$expression == "controlGood1")
c1 <- droplevels(c1)
head(c1)
nrow(c1) 

# proportion of "no" answers to control1
c1 %>%
  count(choice) %>%
  mutate(prop =  n / sum(n)) %>%
  filter(choice == "No,")

# make control c2 data subset
c2 <- subset(d, d$expression == "controlGood2")
c2 <- droplevels(c2)
head(c2)
nrow(c2) 

# proportion of "yes, and" answers to control2
c2 %>%
  count(choice) %>%
  mutate(prop =  n / sum(n)) %>%
  filter(choice == "Yes, and")

# proportion of "no" responses to control1 by participant
controlresponses1 = c1 %>%
  count(participantID, choice) %>%
  group_by(participantID) %>%
  filter(choice == "No")

ggplot(controlresponses1, aes(x=n)) +
  geom_histogram()

table(controlresponses1$n)

# proportion of "yes, and" responses to control2 by participant
controlresponses2 = c2 %>%
  count(participantID, choice) %>%
  group_by(participantID) %>%
  filter(choice == "Yes, and")

ggplot(controlresponses2, aes(x=n)) +
  geom_histogram()

table(controlresponses2$n)



#find out participants who choose the wrong answers of the controls
outlier_Turkers1 = c1 %>%
  group_by(participantID) %>%
  filter(choice == "Yes, and" | choice == "Yes, but")
outlier_Turkers1 #4 participants got the wrong answers of the control 1

outlier_Turkers2 = c2 %>%
  group_by(participantID) %>%
  filter(choice == "Yes, but" | choice == "No")
outlier_Turkers2 #2 participants got the wrong answer of the control 2

# remove participants who gave "yes/1" response to at least one control
d <- droplevels(subset(d, !(d$participantID %in% outlier_Turkers1$participantID)))
length(unique(d$participant_id)) #4 excluded

d <- droplevels(subset(d, !(d$participantID %in% outlier_Turkers2$participantID)))
length(unique(d$participant_id)) #0 excluded (because two participants who got the wrong answers of control 2 have already been excluded (they also got the wrong answers of control 1))

# clean data
# exclude the fillers and the controls
tmp = d %>%
  filter(cc != "noCC") #%>%
#group_by(expression,cc) %>% 
#tally
tmp
nrow(tmp) # 20 (if everything had been chosen, should be 1200 = 3 contexts x 400 predicate/cc combinations)
min(tmp$n) #1
mean(tmp$n) #1.05
max(tmp$n) #1.05
#view(tmp)
write_csv(tmp, file="../data/cd.csv")
# how many data points per predicate/context combination?


