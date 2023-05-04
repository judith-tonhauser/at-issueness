# at-issueness: pilot 1 (unembedded sentences)
# preprocessing.R

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# load relevant packages and set background color
library(tidyverse)
theme_set(theme_bw())

# load the data and bind it
d = read_csv("../data/experiment-trials.csv") 
nrow(d) #31200 = 600 participants x 52 trials (2x20 target + 2x6 filler/control)
head(d)
summary(d) 

length(unique(d$workerid)) #600

# read in the subject information
ds = read_csv("../data/experiment-subject_information.csv")
length(unique(ds$workerid)) #600


# merge subject information into data
d = d %>%
  left_join(ds, by=c("workerid")) 
nrow(d) #31200

# plot the raw data
ggplot(d, aes(x=PARTICIPANT,y=RESPONSE)) +
  geom_point() +
  facet_wrap(. ~ ITEM)

# remove participants' data based on native speaker status

# exclude anybody who didn't include English among languages spoken
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
length(unique(d$workerid)) #581
length(which(is.na(d$american))) #104 missing responses = 2 participants
table(d$american) 

d <- d %>%
  filter(american == "Yes") %>%
  droplevels()
length(unique(d$workerid)) #573 (data from 8 participants excluded)

# remove participants' data based on responses to controls

controls <- droplevels(subset(d, d$ITEM == "ITEM"))

# plot the controls data
ggplot(controls, aes(x=PARTICIPANT,y=RESPONSE)) +
  geom_point() +
  facet_wrap(. ~ ITEM)

# exclude participants who didn't get the controls right
tmp <- droplevels(subset(controls,controls$Response == ???))
nrow(tmp) #6 participants
tmp

d <- droplevels(subset(d, !(d$workerid %in% tmp$workerid)))
nrow(d) 

# histogram of raw responses 
ggplot(data=controls, aes(controls$Response)) + 
  geom_histogram(binwidth = 0.25) +
  xlab("Raw responses") 
# most responses are 1 and 2

# histogram of mean responses 
agr = aggregate(Response ~  workerid, data=controls, FUN="mean")
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
write_csv(cd, "../data/d.csv", row.names=FALSE)

