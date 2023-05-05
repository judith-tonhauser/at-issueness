# at-issueness: pilot 1 (unembedded sentences)
# preprocessing.R

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# load relevant packages and set background color
library(tidyverse)
theme_set(theme_bw())

# load the data
d = read_csv("../data/list1.csv") 
nrow(d) #8 participants
head(d)
summary(d) 

# replace timestamps with participant ID
colnames(d)[1] ="participantID"
d$participantID <- match(d$participantID, unique(sort(d$participantID)))
table(d$participantID)

# replace question column headers with shorter code (Xenia, I've given the items labels but
# you might have better ones, of course!)
colnames(d)
colnames(d)[2] ="afd-rechspopulistisch"
colnames(d)[3] ="gruen-neuzugezogen"
colnames(d)[4] ="spd-mutter"
colnames(d)[5] ="afd-rechstreu"
colnames(d)[6] ="gruen-asylindustrie"
colnames(d)[7] ="spd-fachkraeftemangel"
colnames(d)[8] ="afd-fluchtursachen"
colnames(d)[9] ="gruen-heimat"
colnames(d)[10] ="afd-fatal"
colnames(d)[11] ="spd-kriminell"
colnames(d)[12] ="gruen-inlaenderfreundlich"
colnames(d)[13] ="afd-nationalstaat"
colnames(d)[14] ="gruen-renten"
colnames(d)[15] ="spd-mehrwert"
# questions about the participants
colnames(d)[16] ="person-age"
colnames(d)[17] ="person-geschlecht"
colnames(d)[18] ="person-bildung"
colnames(d)[19] ="person-migration"
colnames(d)[20] ="person-ethnicity"
colnames(d)[21] ="person-partei"
# wahl-o-mat questions
colnames(d)[22] ="wom-nachzug"
colnames(d)[23] ="wom-kopftuch"
colnames(d)[24] ="wom-staatsangehoerigkeit"
colnames(d)[25] ="wom-islam"
colnames(d)[26] ="wom-asyl"

colnames(d)

# get the data from wide into long format ----

# identify the columns that need to be pivoted (responses to trials, not info about age etc)
colnames(d)[1:26] #2-15

tmp = d %>%
  pivot_longer(cols = "afd-rechspopulistisch":"spd-mehrwert",
               names_to = "question", 
               values_to = "response")
view(tmp)

# now that we see that tmp works, make d = tmp
d = tmp

length(unique(d$participantID)) #8 participants

# read in the subject information
# ds = read_csv("../data/experiment-subject_information.csv")
# length(unique(ds$workerid)) #600
# 
# 
# # merge subject information into data
# d = d %>%
#   left_join(ds, by=c("workerid")) 
# nrow(d) #31200

# now make separate columns for party and items from the question column
table(d$question)
d = d %>% 
  mutate(party = case_when(grepl("afd", question) ~ "afd",
                           grepl("spd", question) ~ "spd",
                           grepl("gruen", question) ~ "gruen",
                           TRUE ~ "ERROR"))
table(d$party)

d$item = d$question
d$item = gsub("afd-|spd-|afd-", "", d$item)
table(d$item)

# remove the question column 
d <- d %>% select(-question)

names(d)
summary(d)

# plot the raw data
ggplot(d, aes(x=participantID,y=response)) +
  geom_point() +
  facet_wrap(party ~ item)

########### THIS IS AS FAR AS IVE ADJUSTED THE CODE ################

# remove participants' data based on native speaker status

# exclude anybody who didn't include English among languages spoken
view(d$language)

d <- d %>%
  filter(!is.na(language)) %>%
  filter(language != "Bulgarian" & language != "Hindi" & 
           language != "Italian" & language != "Kannada" & 
           language != "khmer" & language != "Nepali" &
           language != "Polish" & language != "Spanish" &
           language != "Tamil" & language != "Telugu" &
           language != "United States" & language != "Vietnamese") %>% 
  droplevels()
length(unique(d$workerid)) #581 (data from 19 participants excluded)

# exclude non-American English speakers
#length(unique(d$workerid)) #581
#length(which(is.na(d$american))) #104 missing responses = 2 participants
#table(d$american) 

# d <- d %>%
#   filter(american == "Yes") %>%
#   droplevels()
# length(unique(d$workerid)) #573 (data from 8 participants excluded)

# remove participants' data based on responses to controls

controls <- droplevels(subset(d, d$ITEM == "ITEM" | d$ITEM === "ITEM2"))

# plot the controls data
ggplot(controls, aes(x=PARTICIPANT,y=RESPONSE)) +
  geom_point() +
  facet_wrap(. ~ ITEM)

# exclude participants who didn't get the controls right
tmp <- droplevels(subset(controls,controls$Response == ???))
nrow(tmp) #6 participants
tmp

d <- droplevels(subset(d, !(d$workerid %in% tmp$workerid)))
nrow(d) #XX 

# histogram of raw responses 
ggplot(data=controls, aes(controls$Response)) + 
  geom_histogram(binwidth = 0.25) +
  xlab("Raw responses") 
# most responses are 1 and 2

# histogram of mean responses 
agr = aggregate(Response ~  item, data=d, FUN="mean")
agr

ggplot(data=agr, aes(agr$Response)) + 
  geom_histogram(binwidth = 0.25) +
  xlab("By-participant mean responses") 
# most participants have a mean of 2 or lower

# age and gender of remaining participants
table(d$Answer.age) #21-76
mean(d$Answer.age) #36
table(d$Answer.gender)
#"female"   "male" 
#2480     2560 
#62       64


## save cleaned up data ----
write_csv(d, "../data/d.csv", row.names=FALSE)

