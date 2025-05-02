# Exp4 Yes, but diagnostic
# preprocessing.R

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# load required packages
library(tidyverse)

source('../../../helpers.R')

theme_set(theme_bw())

# read in the raw data
d = read_csv("../data/combined.csv")

# remove rows with info about instructions and final screens
d <- d %>% filter(!(trial_type %in% c("survey-text", "html-button-response")))

# replace participant_id by random number
length(unique(d$participant_id)) #80
d$participantID <- match(d$participant_id, unique(sort(d$participant_id)))
table(d$participantID)

# how many participants?
length(unique(d$participantID)) #80

# select relevant columns
d = d %>%
  select(c(stimulus, response, trial_index, trial_type, participantID))

# unpack demographics info
dg <- d %>%
  filter(trial_type == "survey") %>%
  select(c(participantID,response))

table(dg$response)

# age
dg$age = as.numeric(gsub("\\D", "", dg$response))
table(dg$age) #19-67
mean(dg$age)

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
#view(d)

# remove unneeded columns from data
d = d %>%
  filter(trial_type != "survey") %>%
  select(-c(stimulus))

d = d %>%
  select(-c(trial_index,trial_type))

#separate the statement and dissent within the response
d = d %>% 
  separate(col = response,
           into = c("statement", "dissent"),
           sep = "</em><br><br><b>")
#view(d)

# create expression column
d$expression <- case_when(
  grepl("is right", d$statement) ~ "be right",
  grepl("knows", d$statement) ~ "know",
  grepl("discovered", d$statement) ~ "discover",
  grepl("confirmed", d$statement) ~ "confirm",
  grepl("confessed", d$statement) ~ "confess",
  grepl("French course", d$statement) ~ "controlGood",
  grepl("has a cat", d$statement) ~ "controlBad",
  grepl("police found|criticized|manager|forgave|chef|boss|neighbor", d$statement) ~ "fnrrc",
  grepl("ran away|feels guilty|caught by the police|sweet tooth|apologized|feels upset|driver's license", d$statement) ~ "mnrrc",
  TRUE ~ "ERROR"
)
table(d$expression)

# create choice column "yes, and" / "yes, but" / "no"
d$choice <- case_when(
  grepl("Yes, and", d$dissent) ~ "Yes, and",
  grepl("Yes, but", d$dissent) ~ "Yes, but",
  grepl("No,", d$dissent) ~ "No,",
  TRUE ~ "ERROR"
)
table(d$choice)

# add nResponse column (numeric variant of yes/no response)
d = d %>%
  mutate(nResponse = ifelse(choice == "No,", 1, 0))

# item
d$cc = case_when(grepl("saw the murder", d$statement) ~ "saw the murder",
                 grepl("cheated on his wife", d$statement) ~ "cheated on his wife",
                 grepl("stole the money", d$statement) ~ "stole the money",
                 grepl("ate the last cupcake", d$statement) ~ "ate the last cupcake",
                 grepl("broke the plate", d$statement) ~ "broke the plate",
                 grepl("lost his key", d$statement) ~ "lost his key",
                 grepl("bought a new car", d$statement) ~ "bought a new car",
                 grepl("French course", d$statement) ~ "control",
                 grepl("has a cat",d$statement) ~ "control",
                 TRUE ~ "ERROR")
table(d$cc) 

# remove statement and dissent columns
d = d %>%
  select(-c(statement,dissent))


# check that all predicates and all contents are being presented to participants
table(d$expression) 
table(d$cc) 
table(d$expression, d$cc)

# participant info
table(d$age) #19-67
length(which(is.na(d$age))) # 0 missing values
mean(d$age,na.rm=TRUE) #37.99

d %>% 
  select(gender, participantID) %>% 
  unique() %>% 
  group_by(gender) %>% 
  summarize(count=n())

# 1 female          48
# 2 male            30
# 3 non-binary      2
# 4 preferNoToSay   0

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

# exclude participants based on controls
names(d)
table(d$expression)

# plot response by type of control
ggplot(data=d[d$expression == "controlGood",], aes(x=participantID, y=nResponse)) +
  geom_point() 
# some got the good control wrong
ggplot(data=d[d$expression == "controlBad",], aes(x=participantID, y=nResponse)) +
  geom_point() 
# everybody got the bad control right

# good control
goodControl = d %>%
  filter(expression == "controlGood")
nrow(goodControl) #76 participants 

# identify participants who didn't respond "no/1" to the good control 
outliers.good = goodControl[goodControl$nResponse != 1,]
nrow(outliers.good)
# 4 participants out of 76 (72/76 = .95 got it right)
# 12 65 30 17

# bad control
badControl = d %>%
  filter(expression == "controlBad")
nrow(badControl) #76 participants 

# identify participants whose response to the bad control is more than 2sd higher than the mean
outliers.bad = badControl[badControl$nResponse == 1,]
nrow(outliers.bad)
# 0 participants

# remove the participants who got either one of the controls wrong
d = d %>%
  filter(!(participantID %in% outliers.good$participantID)) %>%
  filter(!(participantID %in% outliers.bad$participantID))
length(unique(d$participantID)) # 72 (so 4 participants excluded)

# adjust the names of the expressions
d = d %>%
  mutate(expression = recode(expression, "right" = "be right", "controlBad" = "NAI MC", "controlGood" = "AI MC",
                             "mnrrc" = "medial NRRC", "fnrrc" = "final NRRC"))

# participant info
table(d$age) #19-67
length(which(is.na(d$age))) # 0 missing values
mean(d$age) #38.47

d %>% 
  select(gender, participantID) %>% 
  unique() %>% 
  group_by(gender) %>% 
  summarize(count=n())

# 1 female          43
# 2 male            27
# 3 non-binary      2
# 4 preferNoToSay   0

nrow(d)

write_csv(d, file="../data/cd.csv")
