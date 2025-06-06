# Exp 5
# projection and at-issueness (asking whether)
# preprocessing

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# load required packages
library(tidyverse)

# load helpers
source('../../helpers.R')

# set theme
theme_set(theme_bw())

# read in the raw data
d = read_csv("../data/experiment-trials.csv")
nrow(d) #15600 / 300 = 52 trials (the experiment was done 300 times, as planned)
head(d)
summary(d) 

length(unique(d$workerid)) #289 unique Turkers
# so 11 of the 300 trials were done by Turkers who had already done the exp

# count of how often each Turker did the experiment
count = d %>%
  dplyr :: select(workerid) %>%
  group_by(workerid) %>%
  tally(sort=T)
count
#View(count)
# 5 Turkers did the experiment more than once, namely 16 times together
# workerid 26, 16, 44, 55, 119

# because we have no way of determining which one was their first submission,
# we removed the data from Turkers who took the experiment more than once 
workers = d %>%
  group_by(workerid) %>%
  count() %>%
  filter(n == 52) %>%
  droplevels()
workers
#View(workers)

d = d %>%
  filter(workerid %in% as.character(workers$workerid))

length(unique(d$workerid)) #284 
nrow(d) #14768 / 52 = 284 (5 Turkers did the experiment more than once, for a total of 16 times)

# read in the subject information
ds = read_csv("../data/experiment-subject_information.csv")
length(unique(ds$workerid)) #289 (as above: 11 times no new workerid)
nrow(ds) #300 
head(ds)
summary(d) # experiment took 9 minutes (median), 10 minutes (mean)

# look at Turkers' comments
unique(ds$comments)

workers = ds %>%
  group_by(workerid) %>%
  count() %>%
  filter(n < 2) %>%
  droplevels()
workers
#View(workers)

ds = ds %>%
  filter(workerid %in% as.character(workers$workerid))

length(unique(ds$workerid)) #284 
nrow(ds) #284

# merge subject information into data
d = d %>%
  left_join(ds, by=c("workerid"))

# age info (for all 284 unique Turkers that participated)
table(d$age) #19-74
mean(d$age) #38.2
ggplot(d, aes(x=age)) +
  geom_histogram()

nrow(d) #14768 / 52 = 284 

# no gender information

# change the response for ai condition so that what was 0/not-at-issue is now 1/not-at-issue
# by subtracting the ai responses from 1
table(d$question_type,d$response)
d[d$question_type == "ai",]$response = 1 - d[d$question_type == "ai",]$response

# make a trial number
unique(d$slide_number_in_experiment) #slide numbers from 5 to 57
d$trial = d$slide_number_in_experiment - 4
unique(d$trial) # trial numbers from 1 to 53 (27 missing because instruction)
d[d$trial > 26,]$trial = d[d$trial > 26,]$trial - 1
unique(d$trial) # trials from 1 to 52

## exclude participants' data ----

### exclude non-American English speakers
length(unique(d$workerid)) #284
length(which(is.na(d$language))) #no missing responses
table(d$language) 

# exclude anybody who didn't include English among languages spoken
d <- d %>%
  filter(language != "Spanish" & language != "Korean" & language != "romanian" 
         & language != "United States") %>%
  droplevels()
length(unique(d$workerid)) #280 (data from 4 Turkers excluded)

# exclude non-American English speakers
length(unique(d$workerid))# 280
length(which(is.na(d$american))) #0 (everybody responded)
table(d$american) 

d <- d %>%
  filter(american == "Yes") %>%
  droplevels()
length(unique(d$workerid)) #277 (data from 3 Turkers excluded)

# exclude Turkers based on main clause controls

# main clauses
names(d)
d.MC <- d %>%
  filter(short_trigger == "MC") %>%
  droplevels()
nrow(d.MC) #3324 / 277 Turkers = 12 (6 main clause controls in each of the two blocks)

# projection of main clause data
table(d$question_type)
d.MC.Proj <- d.MC %>%
  filter(question_type == "projective") %>%
  droplevels()
nrow(d.MC.Proj) #1662 / 277 Turkers = 6 main clause controls in projection block

# group projection mean (all Turkers, all clauses)
round(mean(d.MC.Proj$response),2) #.14

# calculate each Turkers mean response to the projection of main clauses
p.means = d.MC.Proj %>%
  group_by(workerid) %>%
  summarize(Mean = mean(response), CI.Low=ci.low(response), CI.High=ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin = Mean-CI.Low, YMax = Mean+CI.High)

ggplot(p.means, aes(x=workerid,y=Mean)) +
  geom_point() +
  geom_errorbar(aes(ymin=YMin, ymax=YMax))+
  geom_text(aes(label=workerid), vjust = 1, cex= 5)+
  ylab("Projection response mean")

# ai of main clause data
d.MC.AI <- d.MC %>%
  filter(question_type == "ai") %>%
  droplevels()
nrow(d.MC.AI) #1662 / 277 Turkers = 6 main clause controls in ai block

# group not-at-issueness mean (all Turkers, all clauses)
round(mean(d.MC.AI$response),2) #.05

# calculate each Turkers mean response to the projection of main clauses
ai.means = d.MC.AI %>%
  group_by(workerid) %>%
  summarize(Mean = mean(response), CI.Low=ci.low(response), CI.High=ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin = Mean-CI.Low, YMax = Mean+CI.High)

ggplot(ai.means, aes(x=workerid,y=Mean)) +
  geom_point() +
  geom_errorbar(aes(ymin=YMin, ymax=YMax))+
  geom_text(aes(label=workerid), vjust = 1, cex= 5)+
  ylab("NAI response mean")

# look at Turkers whose response mean on projection and ainess of main clauses is more than 2
# standard deviations away from the overall mean

# get the Turkers who are more than 2 standard deviations above the mean on projection 
p <- p.means[p.means$Mean > (mean(p.means$Mean) + 2*sd(p.means$Mean)),]
p #23 Turkers 

# get the Turkers who are more than 2 standard deviations above the mean on ai 
ai <- ai.means[ai.means$Mean > (mean(ai.means$Mean) + 2*sd(ai.means$Mean)),]
ai #17 Turkers

# make data subset of just the outliers
outliers <- d.MC %>%
  filter(workerid %in% p$workerid | workerid %in% ai$workerid)
outliers = droplevels(outliers)
nrow(outliers) #420 / 12 = 35 outlier Turkers

# exclude all outlier Turkers identified above
d <- d %>%
  filter(!(workerid %in% p$workerid | workerid %in% ai$workerid)) %>%
  droplevels()
length(unique(d$workerid)) # 242 remaining Turkers (35 Turkers excluded)

# variance

# exclude participants who always clicked on roughly the same point on the scale 
# ie participants whose variance in overall response distribution is more 
# than 2 sd below mean by-participant variance
table(d$trigger)
table(d$question_type)

variances = d %>%
  filter(trigger != "MC") %>%
  group_by(workerid) %>%
  summarize(Variance = var(response)) %>%
  mutate(TooSmall = Variance < mean(Variance) - 2*sd(Variance))

#View(variances)

ggplot(variances,aes(x=workerid,y=Variance)) +
  geom_point()

lowvarworkers = as.character(variances[variances$TooSmall,]$workerid)
summary(variances)
lowvarworkers # 3 participants had lower mean variance

lvw = d %>%
  filter(as.character(workerid) %in% lowvarworkers) %>%
  droplevels() %>%
  mutate(Participant = as.factor(as.character(workerid)))

ggplot(lvw[lvw$trigger != "MC",],aes(x=Participant,y=response,color=trigger_class)) +
  geom_point()

# manual inspection shows that these three participants used the whole scale, so no reason to exclude them

# exclude 0 participant with really low variance 
#d <- droplevels(subset(d, !(d$workerid %in% lowvarworkers)))
length(unique(d$workerid)) #242 participants remain

# write cleaned data to file
write_csv(d, file="../data/data_preprocessed.csv")

# info on remaining participants
table(d$age) #21-74
length(which(is.na(d$age))) # 0 missing values
mean(d$age,na.rm=TRUE) #39.2

# no gender information available

# data points (at-issueness ratings only)
nrow(d)/2


