# q-expectedness: pilot 1
# preprocessing.R

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# load relevant packages and set background color
library(tidyverse)
theme_set(theme_bw())

# load the data
d = read_csv("../data/data.csv") 
nrow(d) #39 rows
head(d)
summary(d) 
table(d$Timestamp) # only 37 different times
d <- tibble::rowid_to_column(d, "participantID")
d = d %>%
  select(-c(Timestamp))

# replace timestamps with participant ID
#colnames(d)[1] ="participantID"
#d$participantID <- match(d$participantID, unique(sort(d$participantID)))
table(d$participantID)

# replace question column headers with shorter code
colnames(d)
colnames(d)[2] ="qexp-discover-CC"
colnames(d)[3] ="qexp-discover-MC"
colnames(d)[4] ="qexp-realize-CC"
colnames(d)[5] ="qexp-control"
colnames(d)[6] ="qexp-realize-MC"
colnames(d)[7] ="qexp-notice-MC"
colnames(d)[8] ="qexp-notice-CC"
colnames(d)[9] ="proj-discover-CC"
colnames(d)[10] ="proj-realize-MC"
colnames(d)[11] ="proj-notice-MC"
colnames(d)[12] ="proj-control"
colnames(d)[13] ="proj-discover-MC"
colnames(d)[14] ="proj-realize-CC"
colnames(d)[15] ="proj-notice-CC"

colnames(d)

# get the data from wide into long format ----

# identify the columns that need to be pivoted (responses to trials, not info about age etc)
colnames(d)[2:15] #2-15

tmp = d %>%
  pivot_longer(cols = "qexp-discover-CC":"proj-notice-CC",
               names_to = "question", 
               values_to = "response")
view(tmp)

# now that we see that tmp works, make d = tmp
d = tmp

length(unique(d$participantID)) #39 participants

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
  mutate(block = case_when(grepl("qexp", question) ~ "qexp",
                           grepl("proj", question) ~ "proj",
                           TRUE ~ "ERROR"))
table(d$block)

d$item = d$question
d$item = gsub("qexp-|proj-", "", d$item)
table(d$item)

d$verb = d$item
d$verb = gsub("-CC|-MC", "", d$verb)
table(d$verb)

d$about = d$item
d$about = gsub("notice-|realize-|discover-", "", d$about)
table(d$about)

# remove the question column 
d <- d %>% select(-c(question,item))

names(d)
summary(d)

# remove participants' data based on responses to controls

controls <- droplevels(subset(d, d$verb == "control"))

# plot the controls data
ggplot(controls, aes(x=participantID,y=response)) +
  geom_point() +
  facet_wrap(. ~ block)

# exclude participants who didn't give a rating of at least 4 on the projection controls
remove.proj <- droplevels(subset(controls, controls$block == "proj" & controls$response < 4))
#view(remove.proj)

d <- droplevels(subset(d, !(d$participantID %in% remove.proj$participantID)))
length(unique(d$participantID)) #29

# target data only
d <- droplevels(subset(d, !d$verb == "control"))


# plot the data
qexp = d %>%
  filter(block == "qexp") %>%
  group_by(verb, about) %>%
  summarize(mean.qexp = mean(response), CILow=ci.low(response),CIHigh=ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin=mean.qexp-CILow,YMax=mean.qexp+CIHigh) %>%
  select(!c(CILow,CIHigh))
qexp

ggplot(qexp, aes(x=about,y=mean.qexp)) +
  geom_point() +
  geom_errorbar(aes(ymin = YMin, ymax = YMax), width = 0.2) +
  xlab("what A's utterance was about") +
  ylab("mean expectedness of topic of next sentence") +
  facet_wrap(. ~ verb)
ggsave(f="../graphs/qexp.pdf",height=3,width=5)
# suggests that manipulation mostly worked: except for realize-CC, 
# A's utterance was set up in such a way that the topic of the next sentence was 
# what I thought it would be

projection = d %>%
  filter(block == "proj") %>%
  group_by(verb, about) %>%
  summarize(mean.proj = mean(response), CILow=ci.low(response),CIHigh=ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin=mean.proj-CILow,YMax=mean.proj+CIHigh) %>%
  select(!c(CILow,CIHigh))
projection

ggplot(projection, aes(x=about,y=mean.proj)) +
  geom_point() +
  geom_errorbar(aes(ymin = YMin, ymax = YMax), width = 0.2) +
  xlab("what A's utterance was about") +
  ylab("mean projection of the CC") +
  facet_wrap(. ~ verb)


ggplot(d[d$block == "proj",], aes(x=response, group = about, color = about, fill = about)) +
  geom_bar(position = "dodge2") +
  xlab("what A's utterance was about") +
  ylab("mean projection of the CC") +
  facet_wrap(. ~ verb)
ggsave(f="../graphs/projection.pdf",height=2,width=5)


# plot participants' ratings
# transform the data to wider format
head(d)
tmp = d %>%
  pivot_wider(names_from = "block", 
               values_from = "response")
view(tmp)

d2 = tmp
names(d2)
head(d2)

ggplot(d2, aes(x=qexp,y=proj)) +
  geom_point() +
  geom_smooth(method="lm",colour="blue") +
  facet_wrap(verb ~ AI)
ggsave(f="../graphs/qexp.pdf",height=3,width=5)



## save cleaned up data ----
write_csv(d, "../data/d.csv", row.names=FALSE)

