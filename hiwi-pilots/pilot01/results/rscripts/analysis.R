# stop/manner/again project analysis

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

source("helpers.r")
library(tidyverse)
library(xtable)
theme_set(theme_bw())

# load data
d = read.csv("../data/cd.csv")
summary(d)
nrow(d) #6840 / 40 judgments = 171 AmE Turkers (5 non-native speakers already excluded)
names(d)
head(d)

# fix the type of the columns
str(d$workerid)
d$workerid <- as.factor(d$workerid)

d$Answer.age <- gsub("\"", "", d$Answer.age)
d$Answer.age <- as.numeric(d$Answer.age)
table(d$Answer.age)

str(d$Response)
d$Response <- as.numeric(d$Response)

## spread of responses to target items ----

# target data
t <- droplevels(subset(d, !(d$verb == "control1" | 
                               d$verb == "control2" | 
                               d$verb == "control3" | 
                               d$verb == "control4")))
nrow(t) #6156 = 171 Turkers x 36 ratings

# by-participant plots to target items
# to identify people who always gave the same response

agr = aggregate(Response ~  workerid, data=t, FUN="mean")
agr

t$workerid <- factor(t$workerid, levels=agr[order(agr$Response), "workerid"])
agr$workerid <-factor(agr$workerid, levels=agr[order(agr$Response), "workerid"])

ggplot(t, aes(x=workerid,y=Response)) +
  geom_point(position=myjit,size=2,aes(color=target)) +
  geom_point(data=agr,size=4)
ggsave(f="../graphs/target-responses-by-participant.pdf",height=4,width=15)

# exclude participants who only responded 1
tmp <- droplevels(subset(agr,agr$Response == 1))
nrow(tmp) #6 participants
tmp
d <- droplevels(subset(d, !(d$workerid %in% tmp$workerid)))
nrow(d) #6600 = 165 Turkers x 40 ratings (6 removed)

## look at controls, for exclusion of participants ----
table(d$verb)

# judgments on the four main clause controls
controls <- droplevels(subset(d, d$verb == "control1" | 
                                d$verb == "control2" | 
                                d$verb == "control3" | 
                                d$verb == "control4"))
nrow(controls) #660 = 165 Turkers x 4 controls
table(controls$Response)

# what did they respond to the 4 controls? 
# did they do worse on the later ones (perhaps due to fatigue), i.e., control4?
table <- table(controls$sound,controls$Response)
table
print(xtable(table, type = "latex"), floating = FALSE,
      file = "../tables/responses-to-controls.tex")
# not obvious that they did worse on the later ones

# group mean response to controls 
mean(controls$Response) #1.7

# histogram of raw responses 
ggplot(data=controls, aes(controls$Response)) + 
  geom_histogram(binwidth = 0.25) +
  xlab("Raw responses") 
ggsave(f="../graphs/controls-histogram.pdf",height=2,width=3)
# most responses are 1 and 2

# histogram of mean responses 
agr = aggregate(Response ~  workerid, data=controls, FUN="mean")
agr

ggplot(data=agr, aes(agr$Response)) + 
  geom_histogram(binwidth = 0.25) +
  xlab("By-participant mean responses") 
ggsave(f="../graphs/controls-mean-histogram.pdf",height=2,width=3)
# most participants have a mean of 2 or lower

# plot responses to 4 controls by participant
agr = aggregate(Response ~  workerid, data=controls, FUN="mean")
agr

controls$workerid2 <-factor(controls$workerid, levels=agr[order(agr$Response), "workerid"])
agr$workerid2 <-factor(agr$workerid, levels=agr[order(agr$Response), "workerid"])

ggplot(controls, aes(x=workerid2,y=Response)) +
  geom_point(size=2,aes(color=verb)) +
  geom_point(data=agr,size=4,color='red')
ggsave(f="../graphs/controls-responses-by-participant.pdf",height=4,width=15)

# data set when nobody is excluded for responses to controls
big <- d
nrow(big) #6600 / 40 = 165 (171 - 6 who only responded 1)
  
# exclude participants whose mean response is higher than 2
wrong.controls <- droplevels(subset(agr, agr$Response > "2"))
nrow(wrong.controls) #39 Turkers with means higher than 2
cd <- droplevels(subset(d, !d$workerid %in% wrong.controls$workerid))
nrow(cd) #5040, i.e. 126 Turkers (excluded 39)

# mean rating of controls by remaining 126 participants
controls <- droplevels(subset(cd, cd$verb == "control1" | 
                                cd$verb == "control2" | 
                                cd$verb == "control3" | 
                                cd$verb == "control4"))
nrow(controls) #504 = 126 Turkers x 4 controls

# group mean response to controls 
mean(controls$Response) #1.2

# age and gender of remaining participants
table(cd$Answer.age) #21-76
mean(cd$Answer.age) #36
table(cd$Answer.gender)
#"female"   "male" 
#2480     2560 
#62       64

# create target data
table(cd$target) #1512 per target content, 504 controls
t <- droplevels(subset(cd, !(cd$verb == "control1" | 
                              cd$verb == "control2" | 
                              cd$verb == "control3" | 
                              cd$verb == "control4")))
nrow(t) #4536
table(t$target,t$prosody)

## save data sets ----
# target data: participants excluded, only target data
write.csv(t, "../data/target-data.csv", row.names=FALSE)
# clean data: participants excluded, target and controls
write.csv(cd, "../data/clean-data.csv", row.names=FALSE)
# big data: nobody excluded, target and controls
write.csv(big, "../data/all-data.csv", row.names=FALSE)


t = read.csv("../data/target-data.csv")
cd = read.csv("../data/clean-data.csv")
big = read.csv("../data/all-data.csv")

## number of ratings --

# number of ratings by item
tmp <- as.data.frame(table(t$sound))
min(tmp$Freq) #5
max(tmp$Freq) #16
mean(tmp$Freq) #10.5

# number of ratings by condition
table(t$target,t$prosody)

## plots ----

# box plot by expression, including controls, with raw ratings
mean_proj = aggregate(Response~target, data=cd, FUN="mean")
mean_proj$YMin = mean_proj$Response - aggregate(Response~target, data=cd, FUN="ci.low")$Response
mean_proj$YMax = mean_proj$Response + aggregate(Response~target, data=cd, FUN="ci.high")$Response
mean_proj

mean_proj$target <- factor(mean_proj$target, levels=mean_proj[order(mean_proj$Response), "target"])
cd$target <- factor(cd$target, levels=mean_proj[order(mean_proj$Response), "target"])

dodge = position_dodge(.9)

ggplot(cd, aes(x=target, y=Response)) + 
  geom_boxplot(lwd=1,width=0.2,position=dodge) +
  stat_summary(fun.y=mean, geom="point", color="black",fill="black", shape=21, size=3,position=position_dodge(.9)) +
  geom_jitter(alpha = 1/15, width=0.25) +
  scale_x_discrete(labels=c("control"="control","manner"="manner adverbs","stop" = "stop","again"="again")) +
  theme(text = element_text(size=12)) +
  ylab("Certainty ratings")+
  xlab("Expression")
ggsave(f="../graphs/boxplot-projection-with-controls.pdf",height=3,width=6.5)

# boxy plot by expression and prosody, excluding controls, with raw ratings
table(t$target)
t$targetNew <- t$target
t$targetNew <- revalue(t$targetNew, c("manner"="manner adverbs"))
table(t$targetNew)

mean_proj = aggregate(Response~targetNew+prosody, data=t, FUN="mean")
mean_proj$YMin = mean_proj$Response - aggregate(Response~targetNew+prosody, data=t, FUN="ci.low")$Response
mean_proj$YMax = mean_proj$Response + aggregate(Response~targetNew+prosody, data=t, FUN="ci.high")$Response
mean_proj

table(t$targetNew)
t$targetNew <- factor(t$targetNew, levels=mean_proj[order(mean_proj[mean_proj$prosody == "aux",]$Response), "targetNew"])

dodge = position_dodge(.9)

ggplot(t, aes(x=prosody, y=Response)) + 
  geom_boxplot(lwd=1,width=0.2,position=dodge) +
  stat_summary(fun.y=mean, geom="point", color="black",fill="black", shape=21, size=3,position=position_dodge(.9)) +
  geom_jitter(alpha = 1/15, width=0.25) +
  theme(text = element_text(size=12)) +
  facet_grid(.~targetNew) +
  ylab("Certainty ratings")+
  xlab("Expression")
ggsave(f="../graphs/boxplot-projection-by-prosody.pdf",height=3,width=6.5)

## plots by-participant ----
# each participant rated 12 again items and 12 stop items

# again

# histogram of raw responses by prosodic condition
ggplot(data=t[t$target == "again",], aes(x=t[t$target == "again",]$Response,fill=t[t$target == "again",]$prosody)) + 
  geom_histogram(binwidth = 0.5, position="dodge") +
  scale_fill_manual(values=cbPalette, name = "Prosodic condition") +
  theme(legend.position = "top",legend.box = "horizontal") +
  xlab("Raw responses") 
ggsave(f="../graphs/histogram-raw-responses-again-by-prosody.pdf",height=3,width=4.2)

# histogram of by-verb mean responses 
agr = aggregate(Response ~  verb + prosody, data=t[t$target == "again",], FUN="mean")
agr$CILow = aggregate(Response ~ verb + prosody, data=t[t$target == "again",], FUN="ci.low")$Response
agr$CIHigh = aggregate(Response ~ verb + prosody, data=t[t$target == "again",], FUN="ci.high")$Response
agr$YMin = agr$Response - agr$CILow
agr$YMax = agr$Response + agr$CIHigh
dodge = position_dodge(.9)
agr

agr$verb <- factor(agr$verb, levels=agr[order(agr[agr$prosody == "aux",]$Response), "verb"])

ggplot(agr, aes(x=prosody,y=Response)) +
  geom_bar(stat="identity",position=dodge) +
  geom_bar(stat="identity",color="black",position=dodge) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  facet_wrap(~ verb, nrow=3) +
  ylab("Mean certainty rating") +
  xlab("Prosodic condition")
ggsave(f="../graphs/bar-plot-by-verb-mean-responses-again.pdf",height=5,width=7)

# histogram of by-participant mean responses 
agr = aggregate(Response ~  workerid, data=t[t$target == "again",], FUN="mean")
agr

ggplot(data=agr, aes(agr$Response)) + 
  geom_histogram(binwidth = 0.25) +
  xlab("By-participant mean responses") 
ggsave(f="../graphs/histogram-by-participant-mean-responses-again.pdf",height=3,width=4.2)

# stop
variances = t %>%
  subset(t$target == "stop" & t$prosody == "target") %>%
  group_by(workerid) %>%
  summarise(ProjMean=mean(Response),Proj.ci.low=ci.low(Response),Proj.ci.high=ci.high(Response))
variances = as.data.frame(variances)
variances

ggplot(variances, aes(x=reorder(workerid,ProjMean),y=ProjMean)) +
  geom_point() +
  #stat_summary(fun.y=mean, geom="point",color="gray70",  size=2,position=position_dodge(.9)) +
  #geom_errorbar(aes(ymin=ProjMean-Proj.ci.low,ymax=ProjMean+Proj.ci.high)) +
  theme(text = element_text(size=12),axis.text.x=element_blank(),axis.ticks.x=element_blank()) +
  #facet_grid(. ~ target) +
  xlab("Participant") +
  ylab("Mean projectivity rating for stop with prosody on stop")
ggsave("../graphs/mean-projectivity-stop-target-prosody-by-participant.pdf",height=3,width=6.5)

### by-verb, by-content and by-prosody ----

# 12 verbs
# content = name/adjunct clause combination
# each verb was combined with 3 contents = 36 contents total
# prosody is a factor with 4 levels (aux, subj, verb, target)

# within each target expression (again, manner, stop), each main clause within a particular content
# was realized with the same recording of an adjunct clause. So, by-item variability could be due to
# i) prosodic condition (main clause), ii) other prosodic properties of the main clause, iii) content,
# and iv) prosodic realization of the adjunct clause (same across prosodic conditions)

#We look at whether the particular
# adjunct clause recording (e.g., the final rise) influenced ratings by comparing ratings within
# a target expression: e.g., compare in "stop" the ratings for aux prosody across different contents
# the variability could be due to content or could be due to the prosodic realization of the adjunct
# clause. we can find out which one it is, by seeing whether content or prosodic realization of the
# adjunct clause is a better predictor of the variability

# verb 
agr = aggregate(Response ~  verb + target, data=t, FUN="mean")
agr$CILow = aggregate(Response ~ verb + target, data=t, FUN="ci.low")$Response
agr$CIHigh = aggregate(Response ~ verb + target, data=t, FUN="ci.high")$Response
agr$YMin = agr$Response - agr$CILow
agr$YMax = agr$Response + agr$CIHigh
dodge = position_dodge(.9)
agr

ggplot(agr, aes(x=verb,y=Response)) +
  geom_bar(stat="identity",position=dodge) +
  geom_bar(stat="identity",color="black",position=dodge) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  facet_grid(. ~ target)
ggsave(f="../graphs/response-mean-by-verb.pdf",height=4,width=14)

# content 
agr = aggregate(Response ~  content + target, data=t, FUN="mean")
agr$CILow = aggregate(Response ~ content + target, data=t, FUN="ci.low")$Response
agr$CIHigh = aggregate(Response ~ content + target, data=t, FUN="ci.high")$Response
agr$YMin = agr$Response - agr$CILow
agr$YMax = agr$Response + agr$CIHigh
dodge = position_dodge(.9)
agr

ggplot(agr, aes(x=content,y=Response)) +
  geom_bar(stat="identity",position=dodge) +
  geom_bar(stat="identity",color="black",position=dodge) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_grid(. ~ target)
ggsave(f="../graphs/response-mean-by-content.pdf",height=4,width=14)

# prosody 
agr = aggregate(Response ~  prosody + target, data=t, FUN="mean")
agr$CILow = aggregate(Response ~ prosody + target, data=t, FUN="ci.low")$Response
agr$CIHigh = aggregate(Response ~ prosody + target, data=t, FUN="ci.high")$Response
agr$YMin = agr$Response - agr$CILow
agr$YMax = agr$Response + agr$CIHigh
dodge = position_dodge(.9)
agr

ggplot(agr, aes(x=prosody,y=Response)) +
  geom_bar(stat="identity",position=dodge) +
  geom_bar(stat="identity",color="black",position=dodge) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_grid(. ~ target)
ggsave(f="../graphs/response-mean-by-prosody.pdf",height=4,width=14)

# content and prosody, for "stop"
agr = aggregate(Response ~  prosody + content, data=t[t$target == "stop",], FUN="mean")
agr$CILow = aggregate(Response ~ prosody + content, data=t[t$target == "stop",], FUN="ci.low")$Response
agr$CIHigh = aggregate(Response ~ prosody + content, data=t[t$target == "stop",], FUN="ci.high")$Response
agr$YMin = agr$Response - agr$CILow
agr$YMax = agr$Response + agr$CIHigh
dodge = position_dodge(.9)
agr

ggplot(agr, aes(x=content,y=Response)) +
  geom_bar(stat="identity",position=dodge) +
  geom_bar(stat="identity",color="black",position=dodge) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_grid(. ~ prosody)
ggsave(f="../graphs/response-mean-stop-by-prosody-and-content.pdf",height=4,width=14)

# content and prosody, for "manner"
agr = aggregate(Response ~  prosody + content, data=t[t$target == "manner",], FUN="mean")
agr$CILow = aggregate(Response ~ prosody + content, data=t[t$target == "manner",], FUN="ci.low")$Response
agr$CIHigh = aggregate(Response ~ prosody + content, data=t[t$target == "manner",], FUN="ci.high")$Response
agr$YMin = agr$Response - agr$CILow
agr$YMax = agr$Response + agr$CIHigh
dodge = position_dodge(.9)
agr

ggplot(agr, aes(x=content,y=Response)) +
  geom_bar(stat="identity",position=dodge) +
  geom_bar(stat="identity",color="black",position=dodge) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_grid(. ~ prosody)
ggsave(f="../graphs/response-mean-manner-by-prosody-and-content.pdf",height=4,width=14)

# content and target for "aux" prosody
agr = aggregate(Response ~  target + content, data=t[t$prosody == "aux",], FUN="mean")
agr$CILow = aggregate(Response ~ target + content, data=t[t$prosody == "aux",], FUN="ci.low")$Response
agr$CIHigh = aggregate(Response ~ target + content, data=t[t$prosody == "aux",], FUN="ci.high")$Response
agr$YMin = agr$Response - agr$CILow
agr$YMax = agr$Response + agr$CIHigh
dodge = position_dodge(.9)
agr

ggplot(agr, aes(x=content,y=Response)) +
  geom_bar(stat="identity",position=dodge) +
  geom_bar(stat="identity",color="black",position=dodge) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_grid(. ~ target)
ggsave(f="../graphs/response-mean-aux-prosody-by-target-and-content.pdf",height=4,width=14)


# the variability we see here, within a prosodic condition, could be due to content, the prosodic
# realization of the main clause or the prosodic realization of the adjunct clause

# stop with verb "clean"
agr = aggregate(Response ~  prosody + content, data=t[t$target == "stop" & t$verb == "clean",], FUN="mean")
agr$CILow = aggregate(Response ~ prosody + content, data=t[t$target == "stop" & t$verb == "clean",], FUN="ci.low")$Response
agr$CIHigh = aggregate(Response ~ prosody + content, data=t[t$target == "stop" & t$verb == "clean",], FUN="ci.high")$Response
agr$YMin = agr$Response - agr$CILow
agr$YMax = agr$Response + agr$CIHigh
dodge = position_dodge(.9)
agr

ggplot(agr, aes(x=content,y=Response)) +
  geom_bar(stat="identity",position=dodge) +
  geom_bar(stat="identity",color="black",position=dodge) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_grid(. ~ prosody)

# stop with verb "giggle"
agr = aggregate(Response ~  prosody + content, data=t[t$target == "stop" & t$verb == "giggle",], FUN="mean")
agr$CILow = aggregate(Response ~ prosody + content, data=t[t$target == "stop" & t$verb == "giggle",], FUN="ci.low")$Response
agr$CIHigh = aggregate(Response ~ prosody + content, data=t[t$target == "stop" & t$verb == "giggle",], FUN="ci.high")$Response
agr$YMin = agr$Response - agr$CILow
agr$YMax = agr$Response + agr$CIHigh
dodge = position_dodge(.9)
agr

ggplot(agr, aes(x=content,y=Response)) +
  geom_bar(stat="identity",position=dodge) +
  geom_bar(stat="identity",color="black",position=dodge) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_grid(. ~ prosody)

## models on target data set ----
library(ordinal)
library(lsmeans)

# fix columns
str(t$Response)
t$Response <- as.factor(t$Response)
cd$Response <- as.factor(cd$Response)

str(cd$target)
str(cd$workerid)
str(cd$verb)
str(cd$sound)
str(cd$content)

# ...are the contents projective? ----
# predict response from expression, with control as an initial reference level

# reverse pairwise comparisons with controls and each other
cd$target <- relevel(cd$target, ref = "control")
model = clmm(Response ~ target + (1+target|workerid) + (1|verb) + (1|content) + (1|sound), data=cd, REML=F)
summary(model)

comparison = lsmeans(model, revpairwise~target,adjust="tukey")
comparison

# $lsmeans
# target     lsmean        SE df asymp.LCL asymp.UCL
# control -4.209636 0.3929169 NA -4.979739 -3.439533
# stop     2.173143 0.2350173 NA  1.712518  2.633768
# again    1.682846 0.2105976 NA  1.270082  2.095610
# manner  -2.387107 0.1921984 NA -2.763809 -2.010405
# 
# Confidence level used: 0.95 
# 
# $contrasts
# contrast           estimate        SE df z.ratio p.value
# stop - control    6.3827788 0.4935660 NA  12.932  <.0001
# again - control   5.8924816 0.4816880 NA  12.233  <.0001
# again - stop     -0.4902972 0.1423254 NA  -3.445  0.0032
# manner - control  1.8225289 0.3978503 NA   4.581  <.0001
# manner - stop    -4.5602499 0.3390739 NA -13.449  <.0001
# manner - again   -4.0699526 0.3116068 NA -13.061  <.0001
# 
# P value adjustment: tukey method for comparing a family of 4 estimates 

# ...is projectivity influenced by prosody? ----
# verb omitted as random effect because variance close to 0
t$target <- relevel(t$target, ref = "stop")
t$prosody <- relevel(t$prosody, ref = "aux")

# is the interaction significant?
m.1 <- clmm(Response ~ target * prosody + (1+target|workerid) + (1+prosody|workerid) + (1|sound), data=t)
summary(m.1)

m.2 <- clmm(Response ~ target + prosody + (1+target|workerid) + (1+prosody|workerid) + (1|sound), data=t)
summary(m.2)

anova(m.1,m.2)
# model with interaction is not significantly better

# pairwise comparisons, within each target expression, without controls

# again

# revalue 'target' so that it is 2nd member of all comparisons
library(plyr)

table(t$prosody)
t$prosodyNew <- t$prosody
t$prosodyNew <- revalue(t$prosodyNew, c("target"="z"))
table(t$prosodyNew)
t$prosodyNew <- relevel(t$prosodyNew, ref = "z")

model = clmm(Response ~ prosodyNew + (1+prosodyNew|workerid) + (1|verb) + (1|content) + (1|sound), data=t[t$target == "again",], REML=F)
summary(model)

comparison = lsmeans(model, pairwise~prosodyNew,adjust="tukey")
comparison

# $lsmeans
# prosodyNew   lsmean        SE df asymp.LCL asymp.UCL
# z          2.192146 0.2663010 NA 1.6702057  2.714086
# aux        1.450395 0.2610144 NA 0.9388166  1.961974
# subject    1.670415 0.2646140 NA 1.1517808  2.189049
# verb       1.416371 0.2572463 NA 0.9121772  1.920564
# 
# Confidence level used: 0.95 
# 
# $contrasts
# contrast          estimate        SE df z.ratio p.value
# z - aux         0.74175061 0.1864668 NA   3.978  0.0004
# z - subject     0.52173121 0.1918274 NA   2.720  0.0331
# z - verb        0.77577533 0.1879209 NA   4.128  0.0002
# aux - subject  -0.22001940 0.1815416 NA  -1.212  0.6192
# aux - verb      0.03402472 0.1768262 NA   0.192  0.9975
# subject - verb  0.25404412 0.1811290 NA   1.403  0.4977
# 
# P value adjustment: tukey method for comparing a family of 4 estimates 

# stop
model = clmm(Response ~ prosodyNew + (1+prosodyNew|workerid) + (1|verb) + (1|content) + (1|sound), data=t[t$target == "stop",], REML=F)
summary(model)

comparison = lsmeans(model, pairwise~prosodyNew,adjust="tukey")
comparison

# prosodyNew   lsmean        SE df asymp.LCL asymp.UCL
# z          2.440342 0.2770136 NA  1.897406  2.983279
# aux        1.738649 0.2391537 NA  1.269916  2.207382
# subject    1.985318 0.2467128 NA  1.501770  2.468866
# verb       2.235284 0.2867580 NA  1.673248  2.797319
# 
# Confidence level used: 0.95 
# 
# $contrasts
# contrast         estimate        SE df z.ratio p.value
# z - aux         0.7016936 0.1943603 NA   3.610  0.0017
# z - subject     0.4550246 0.1917048 NA   2.374  0.0822
# z - verb        0.2050589 0.2002099 NA   1.024  0.7352
# aux - subject  -0.2466690 0.1793459 NA  -1.375  0.5148
# aux - verb     -0.4966347 0.1991025 NA  -2.494  0.0608
# subject - verb -0.2499657 0.1905458 NA  -1.312  0.5553
# 
# P value adjustment: tukey method for comparing a family of 4 estimates

# manner
model = clmm(Response ~ prosodyNew + (1+prosodyNew|workerid) + (1|verb) + (1|content) + (1|sound), data=t[t$target == "manner",], REML=F)
summary(model)

comparison = lsmeans(model, pairwise~prosodyNew,adjust="tukey")
comparison

# prosodyNew    lsmean        SE df asymp.LCL asymp.UCL
# z          -2.109561 0.2394808 NA -2.578934 -1.640187
# aux        -2.979075 0.2874651 NA -3.542497 -2.415654
# subject    -2.816239 0.2600785 NA -3.325983 -2.306494
# verb       -2.787813 0.2512960 NA -3.280344 -2.295282
# 
# Confidence level used: 0.95 
# 
# $contrasts
# contrast          estimate        SE df z.ratio p.value
# z - aux         0.86951465 0.2316294 NA   3.754  0.0010
# z - subject     0.70667792 0.1920966 NA   3.679  0.0013
# z - verb        0.67825220 0.1839256 NA   3.688  0.0013
# aux - subject  -0.16283673 0.2154891 NA  -0.756  0.8743
# aux - verb     -0.19126245 0.2213292 NA  -0.864  0.8233
# subject - verb -0.02842572 0.1936398 NA  -0.147  0.9989
# 
# P value adjustment: tukey method for comparing a family of 4 estimates 

# manner, with controls (shows that each condition is projective, compared to controls)
library(plyr)

table(cd$prosody)
cd$prosodyNew <- cd$prosody
cd$prosodyNew <- revalue(cd$prosodyNew, c("control"="a"))
table(cd$prosodyNew)

cd$prosodyNew <- relevel(cd$prosodyNew, ref = "a")
model = clmm(Response ~ prosodyNew + (1+prosodyNew|workerid) + (1|content) + (1|sound), data=cd[cd$target == "manner" | cd$target == "control",], REML=F)
summary(model)

comparison = lsmeans(model, revpairwise~prosodyNew,adjust="tukey")
comparison
# $lsmeans
# prosodyNew    lsmean        SE df asymp.LCL asymp.UCL
# a          -4.642443 0.3431147 NA -5.314936 -3.969951
# target     -2.103535 0.2290342 NA -2.552433 -1.654636
# aux        -2.981589 0.2784672 NA -3.527374 -2.435803
# subject    -2.845896 0.2560580 NA -3.347760 -2.344032
# verb       -2.791516 0.2410092 NA -3.263885 -2.319146
# 
# Confidence level used: 0.95 
# 
# $contrasts
# contrast            estimate        SE df z.ratio p.value
# target - a        2.53890859 0.3638231 NA   6.978  <.0001
# aux - a           1.66085454 0.3839094 NA   4.326  0.0001
# aux - target     -0.87805405 0.2313259 NA  -3.796  0.0014
# subject - a       1.79654723 0.3623404 NA   4.958  <.0001
# subject - target -0.74236136 0.1978679 NA  -3.752  0.0016
# subject - aux     0.13569269 0.2199195 NA   0.617  0.9724
# verb - a          1.85092741 0.3620374 NA   5.113  <.0001
# verb - target    -0.68798118 0.1846749 NA  -3.725  0.0018
# verb - aux        0.19007287 0.2223905 NA   0.855  0.9132
# verb - subject    0.05438018 0.1980370 NA   0.275  0.9988
# 
# P value adjustment: tukey method for comparing a family of 5 estimates 

## models on big target data set (no participants excluded based on controls) ----
library(ordinal)
library(lsmeans)

# fix columns
str(big$Response)
big$Response <- as.factor(big$Response)

# make big target data set
bt <- droplevels(subset(big, !(big$verb == "control1" | 
                                 big$verb == "control2" | 
                                 big$verb == "control3" | 
                                 big$verb == "control4")))
nrow(bt) #5940 = 36 items for each of the 165 participants

# ...are the contents projective? ----
# predict response from expression, with control as an initial reference level

# reverse pairwise comparisons with controls and each other
big$target <- relevel(big$target, ref = "control")
model = clmm(Response ~ target + (1+target|workerid) + (1|verb) + (1|content) + (1|sound), data=big, REML=F)
summary(model)

comparison = lsmeans(model, revpairwise~target,adjust="tukey")
comparison

# target     lsmean        SE df asymp.LCL asymp.UCL
# control -3.631677 0.3776802 NA -4.371917 -2.891438
# again    1.425280 0.1685523 NA  1.094924  1.755637
# manner  -1.774678 0.1723765 NA -2.112529 -1.436826
# stop     1.725985 0.1915943 NA  1.350467  2.101503
# 
# Confidence level used: 0.95 
# 
# $contrasts
# contrast           estimate        SE df z.ratio p.value
# again - control   5.0569576 0.4548893 NA  11.117  <.0001
# manner - control  1.8569999 0.3280531 NA   5.661  <.0001
# manner - again   -3.1999577 0.2655305 NA -12.051  <.0001
# stop - control    5.3576623 0.4763948 NA  11.246  <.0001
# stop - again      0.3007047 0.1161281 NA   2.589  0.0474
# stop - manner     3.5006625 0.2941806 NA  11.900  <.0001
# 
# P value adjustment: tukey method for comparing a family of 4 estimates 

# ...is projectivity influenced by prosody? ----
# pairwise comparisons, within each target expression, without controls

# again

# revalue 'target' so that it is 2nd member of all comparisons
library(plyr)

table(bt$prosody)
bt$prosodyNew <- bt$prosody
bt$prosodyNew <- revalue(bt$prosodyNew, c("target"="z"))
table(bt$prosodyNew)
bt$prosodyNew <- relevel(bt$prosodyNew, ref = "z")

model = clmm(Response ~ prosodyNew + (1+prosodyNew|workerid) + (1|verb) + (1|content) + (1|sound), data=bt[bt$target == "again",], REML=F)
summary(model)

comparison = lsmeans(model, pairwise~prosodyNew,adjust="tukey")
comparison

# $lsmeans
# prosodyNew   lsmean        SE df asymp.LCL asymp.UCL
# z          1.813171 0.2139226 NA 1.3938903  2.232452
# aux        1.223751 0.2074450 NA 0.8171660  1.630336
# subject    1.348815 0.2054446 NA 0.9461513  1.751479
# verb       1.222504 0.2024245 NA 0.8257596  1.619249
# 
# Confidence level used: 0.95 
# 
# $contrasts
# contrast           estimate        SE df z.ratio p.value
# z - aux         0.589420105 0.1504696 NA   3.917  0.0005
# z - subject     0.464355596 0.1509465 NA   3.076  0.0113
# z - verb        0.590666612 0.1509483 NA   3.913  0.0005
# aux - subject  -0.125064508 0.1451157 NA  -0.862  0.8245
# aux - verb      0.001246508 0.1449558 NA   0.009  1.0000
# subject - verb  0.126311016 0.1456573 NA   0.867  0.8218
# 
# P value adjustment: tukey method for comparing a family of 4 estimates

# stop
model = clmm(Response ~ prosodyNew + (1+prosodyNew|workerid) + (1|verb) + (1|content) + (1|sound), data=bt[bt$target == "stop",], REML=F)
summary(model)

comparison = lsmeans(model, pairwise~prosodyNew,adjust="tukey")
comparison

# prosodyNew   lsmean        SE df asymp.LCL asymp.UCL
# z          1.955438 0.2261580 NA 1.5121770  2.398700
# aux        1.367633 0.1969952 NA 0.9815294  1.753736
# subject    1.600271 0.1992182 NA 1.2098109  1.990732
# verb       1.781860 0.2205105 NA 1.3496679  2.214053
# 
# Confidence level used: 0.95 
# 
# $contrasts
# contrast         estimate        SE df z.ratio p.value
# z - aux         0.5878056 0.1532568 NA   3.835  0.0007
# z - subject     0.3551671 0.1507271 NA   2.356  0.0857
# z - verb        0.1735780 0.1540639 NA   1.127  0.6730
# aux - subject  -0.2326386 0.1414106 NA  -1.645  0.3532
# aux - verb     -0.4142277 0.1523032 NA  -2.720  0.0331
# subject - verb -0.1815891 0.1472497 NA  -1.233  0.6057
# 
# P value adjustment: tukey method for comparing a family of 4 estimates


# manner
model = clmm(Response ~ prosodyNew + (1+prosodyNew|workerid) + (1|verb) + (1|content) + (1|sound), data=bt[bt$target == "manner",], REML=F)
summary(model)

comparison = lsmeans(model, pairwise~prosodyNew,adjust="tukey")
comparison

# prosodyNew    lsmean        SE df asymp.LCL asymp.UCL
# z          -1.552527 0.1919740 NA -1.928789 -1.176265
# aux        -2.217726 0.2394757 NA -2.687090 -1.748362
# subject    -2.057096 0.2186814 NA -2.485704 -1.628489
# verb       -2.062010 0.2220145 NA -2.497150 -1.626869
# 
# Confidence level used: 0.95 
# 
# $contrasts
# contrast           estimate        SE df z.ratio p.value
# z - aux         0.665198840 0.1751268 NA   3.798  0.0008
# z - subject     0.504569281 0.1503170 NA   3.357  0.0044
# z - verb        0.509482464 0.1472982 NA   3.459  0.0030
# aux - subject  -0.160629559 0.1608756 NA  -0.998  0.7503
# aux - verb     -0.155716376 0.1696369 NA  -0.918  0.7953
# subject - verb  0.004913183 0.1513648 NA   0.032  1.0000

# P value adjustment: tukey method for comparing a family of 4 estimates 

# manner, with controls (shows that each condition is projective, compared to controls)
library(plyr)

table(big$prosody)
big$prosodyNew <- big$prosody
big$prosodyNew <- revalue(big$prosodyNew, c("control"="a"))
table(big$prosodyNew)

big$prosodyNew <- relevel(big$prosodyNew, ref = "a")
model = clmm(Response ~ prosodyNew + (1+prosodyNew|workerid) + (1|content) + (1|sound), data=big[big$target == "manner" | big$target == "control",], REML=F)
summary(model)

comparison = lsmeans(model, revpairwise~prosodyNew,adjust="tukey")
comparison


# prosodyNew    lsmean        SE df asymp.LCL asymp.UCL
# a          -3.841701 0.3461313 NA -4.520106 -3.163296
# aux        -2.158016 0.2282471 NA -2.605372 -1.710659
# subject    -2.021893 0.2121401 NA -2.437680 -1.606106
# target     -1.520047 0.1842977 NA -1.881264 -1.158830
# verb       -2.018626 0.2122838 NA -2.434695 -1.602558
# 
# Confidence level used: 0.95 
# 
# $contrasts
# contrast             estimate        SE df z.ratio p.value
# aux - a           1.683685680 0.3007207 NA   5.599  <.0001
# subject - a       1.819808791 0.2849199 NA   6.387  <.0001
# subject - aux     0.136123111 0.1603898 NA   0.849  0.9153
# target - a        2.321654764 0.3094567 NA   7.502  <.0001
# target - aux      0.637969084 0.1699528 NA   3.754  0.0016
# target - subject  0.501845973 0.1533070 NA   3.273  0.0094
# verb - a          1.823074930 0.2914804 NA   6.255  <.0001
# verb - aux        0.139389250 0.1674383 NA   0.832  0.9206
# verb - subject    0.003266139 0.1504474 NA   0.022  1.0000
# verb - target    -0.498579834 0.1481000 NA  -3.367  0.0068
# 
# P value adjustment: tukey method for comparing a family of 5 estimates

### Phonetic cues to projectivity ----

#...adjunct-clauses (with L* on aux) ----
# to analyze utterance final rise and rise on auxiliary 

# read in data
# annotated for 1: f0 at beginning of adjunct clause, 2: beginning of utt-final rise, 3: end of rise
# and for: a1: L of L* on auxiliary, a2: highest point of subject 
d = read.csv("../f0-analyses-of-stimuli/adjunct-clauses/f0-values.csv")
names(d)
# remove control utterances
table(d$File)
d <- droplevels(subset(d,!d$File == "control1" & !d$File == "control2" & !d$File == "control3" & !d$File == "control4"))
nrow(d) #540 (540 / 5 f0 annotations = 108 utterances, 108 = 12 verbs x 3 contents x 3 target expressions)
View(d)
summary(d)
View(t)

# convert to wide format to merge with target data (t)
library(magrittr)
requireNamespace("tidyr")
requireNamespace("dplyr")

d <- d %>% 
  tidyr::gather_(key="variable", value="value", c("time", "f0")) %>%  # Make it even longer.
  dplyr::mutate(                                                  # Create the spread key.
    label_by_variable   = paste0(variable, "", label)) %>% 
  dplyr::select(File, label_by_variable, value) %>%                  # Retain these three.
  tidyr::spread(key=label_by_variable, value=value)

View(d)
summary(d)

# add new predictors
# f0 of utterance final rise
d$risef0 <- d$f03 - d$f02
# slope (rate of pitch change) of utterance final rise 
d$riseTime <- d$time3 - d$time2
d$riseSlope <- d$risef0 / d$riseTime

#f0 of rise on Aux
d$riseAuxf0 <- d$f0a2 - d$f0a1
# slope (rate of pitch change) of rise on Aux
d$riseAuxTime <- d$timea2 - d$timea1
d$riseAuxSlope <- d$riseAuxf0 / d$riseAuxTime

head(d)

# to merge with target data set: create column that codes content and target, but not prosody
d$contentTarget <- d$File
d$contentTarget <- gsub("\\-A","",d$contentTarget)

head(t)
names(t)
t$contentTarget <- t$sound
t$contentTarget <- gsub("\\-[A-Z].wav","",t$contentTarget)

t <- merge(t,d,by="contentTarget")
head(t)
table(t$prosody)

# save new t 
# target data: participants excluded, only target data
write.csv(t, "../data/target-data.csv", row.names=FALSE)

t = read.csv("../data/target-data.csv")

## ...plots ----
names(t)

# f0 of L on Aux
ggplot(t, aes(x=reorder(content,f0a1,mean),y=f0a1,color=target)) +
  geom_point() +
  stat_summary(fun.y=mean, geom="point", color="black",fill="gray70", shape=21, size=3,position=position_dodge(.9)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# f0 of H after Aux
ggplot(t, aes(x=reorder(content,f0a2,mean),y=f0a2,color=target)) +
  geom_point() +
  stat_summary(fun.y=mean, geom="point", color="black",fill="gray70", shape=21, size=3,position=position_dodge(.9)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# rise of LH on Aux
ggplot(t, aes(x=reorder(content,riseAuxf0,mean),y=riseAuxf0,color=target)) +
  geom_point() +
  stat_summary(fun.y=mean, geom="point", color="black",fill="gray70", shape=21, size=3,position=position_dodge(.9)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# slope of rise of LH on Aux
ggplot(t, aes(x=reorder(content,riseAuxSlope,mean),y=riseAuxSlope,color=target)) +
  geom_point() +
  stat_summary(fun.y=mean, geom="point", color="black",fill="gray70", shape=21, size=3,position=position_dodge(.9)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# f0 at beginning of adjunct clause
ggplot(t, aes(x=reorder(content,f01,mean),y=f01,color=target)) +
  geom_point() +
  stat_summary(fun.y=mean, geom="point", color="black",fill="gray70", shape=21, size=3,position=position_dodge(.9)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# there is variability in the f0 at the beginning of the adjunct clauses between the different contents

ggplot(t, aes(x=reorder(verb,f01,mean),y=f01,color=target)) +
  geom_point() +
  stat_summary(fun.y=mean, geom="point", color="black",fill="gray70", shape=21, size=3,position=position_dodge(.9)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# there is variability in the f0 at the beginning of the adjunct clauses between the different verbs

ggplot(t, aes(x=reorder(sound,f01,mean),y=f01,color=target)) +
  geom_point() +
  facet_grid(. ~ target) +
  #stat_summary(fun.y=mean, geom="point", color="black",fill="gray70", shape=21, size=3,position=position_dodge(.9)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# there is variability in the f0 at the beginning of the adjunct clauses between the different items

# extent of final rise
ggplot(t, aes(x=reorder(content,risef0,mean),y=risef0,color=target)) +
  geom_point() +
  stat_summary(fun.y=mean, geom="point", color="black",fill="gray70", shape=21, size=3,position=position_dodge(.9)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(t, aes(x=reorder(verb,risef0,mean),y=risef0,color=target)) +
  geom_point() +
  stat_summary(fun.y=mean, geom="point", color="black",fill="gray70", shape=21, size=3,position=position_dodge(.9)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(t, aes(x=reorder(sound,risef0,mean),y=risef0,color=target)) +
  geom_point() +
  facet_grid(. ~ target) +
  #stat_summary(fun.y=mean, geom="point", color="black",fill="gray70", shape=21, size=3,position=position_dodge(.9)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# slope of final rise
ggplot(t, aes(x=reorder(content,riseSlope,mean),y=riseSlope,color=target)) +
  geom_point() +
  stat_summary(fun.y=mean, geom="point", color="black",fill="gray70", shape=21, size=3,position=position_dodge(.9)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(t, aes(x=reorder(verb,riseSlope,mean),y=riseSlope,color=target)) +
  geom_point() +
  stat_summary(fun.y=mean, geom="point", color="black",fill="gray70", shape=21, size=3,position=position_dodge(.9)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(t, aes(x=reorder(sound,riseSlope,mean),y=riseSlope,color=target)) +
  geom_point() +
  facet_grid(. ~ target) +
  #stat_summary(fun.y=mean, geom="point", color="black",fill="gray70", shape=21, size=3,position=position_dodge(.9)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# does Response vary with f0? we need to look at this by target because Response varies greatly by target
names(t)
table(t$prosody)
str(t$Response)
t$Response <- as.numeric(t$Response)

# Response by rise on auxiliary (subset of target data with Aux prosody)
ggplot(t[t$prosody == "aux",], aes(x=riseAuxf0,y=Response)) +
  geom_point() +
  #stat_summary(fun.data=t) + 
  geom_smooth(method='lm') +
  facet_grid(~ target) 

# Response by slope of on auxiliary (subset of target data with Aux prosody)
ggplot(t[t$prosody == "aux",], aes(x=riseAuxSlope,y=Response)) +
  geom_point() +
  #stat_summary(fun.data=t) + 
  geom_smooth(method='lm') +
  facet_grid( ~ target) 

# Response by f0 at beginning of adjunct clause
ggplot(t, aes(x=f01,y=Response)) +
  geom_point() +
  #stat_summary(fun.data=t) + 
  geom_smooth(method='lm') +
  facet_grid(prosody ~ target) 
# for again: the higher the f0 at the beginning of the adjunct clause, the lower the response

# Response by extent of f0 rise
ggplot(t, aes(x=risef0,y=Response)) +
  geom_point() +
  #stat_summary(fun.data=t) + 
  geom_smooth(method='lm') +
  facet_grid(prosody ~ target) 

ggplot(t, aes(x=risef0,y=Response)) +
  geom_point() +
  #stat_summary(fun.data=t) + 
  geom_smooth(method='lm') +
  facet_grid( ~ target) 

# Response by slope
ggplot(t, aes(x=riseSlope,y=Response)) +
  geom_point() +
  #stat_summary(fun.data=t) + 
  geom_smooth(method='lm') +
  facet_grid(prosody ~ target) 
# the greater the slope, the higher the response

# Response by target and prosody (Konstanz talk)
agr = t %>%
  group_by(target) %>%
  summarise(meanResponse=mean(Response),ci.low=ci.low(Response),ci.high=ci.high(Response)) %>%
  mutate(YMin=meanResponse-ci.low,YMax=meanResponse+ci.high)
agr

agr2 = t %>%
  group_by(target,prosody) %>%
  summarise(meanResponse=mean(Response),ci.low=ci.low(Response),ci.high=ci.high(Response)) %>%
  mutate(YMin=meanResponse-ci.low,YMax=meanResponse+ci.high)
agr2


ggplot(agr, aes(target,meanResponse)) +
  geom_col() +
  facet_grid(~ target,drop=TRUE,scales = "free_x") +
  #geom_bar(stat="identity",position=dodge,color="black") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  expand_limits(y=5) +
  ylab("Mean certainty rating") +
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())
ggsave(f="../graphs/mean-rating-by-target.pdf",height=3.5,width=8)

ggplot(agr2, aes(prosody,meanResponse)) +
  geom_col() +
  facet_grid(~ target) +
  #geom_bar(stat="identity",position=dodge,color="black") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  expand_limits(y=5) +
  ylab("Mean certainty rating") +
  xlab("Location of L* pitch accent")
ggsave(f="../graphs/mean-rating-by-prosody-and-target.pdf",height=3.5,width=8)

t$target <- factor(t$target, levels=unique(agr$target[order(agr$meanResponse)]), ordered=TRUE)
table(t$target)

ggplot(t, aes(x=riseSlope,y=Response)) +
  geom_point(alpha = .15,position = "jitter") +
  facet_grid(~ target) +
  scale_x_continuous("Slope of utterance-final f0 rise",limits=c(200,500)) +
  scale_y_discrete("Certainty rating",limits=c(1,2,3,4,5)) +
  geom_smooth(method='lm')
ggsave(f="../graphs/slope-of-final-rise-by-target.pdf",height=3.5,width=8)
  
# the greater the slope, the higher the response

# Response by slope, "stop" 
ggplot(t[t$target == "again",], aes(x=riseSlope,y=Response)) +
  geom_point() +
  #stat_summary(fun.data=t) + 
  geom_smooth(method='lm') +
  facet_grid( ~ prosody) 

## ...models ----
library(ordinal)

# fix columns
str(t$Response)
t$Response <- as.factor(t$Response)

m.1 <- clmm(Response ~ target + prosody + risef0  + (1|workerid) + (1|sound), data=t)
summary(m.1) # risef0 is significant * with positive coefficient
m.2 <- clmm(Response ~ target + prosody  + (1|workerid) + (1|workerid) + (1|sound), data=t)
anova(m.1,m.2) #m.1 is better 

m.1b <- clmm(Response ~ target * riseSlope + prosody + (1|workerid) + (1|sound), data=t)
summary(m.1b) # riseSlope is significant ** with positive coefficient
m.1b2 <- clmm(Response ~ target + riseSlope + prosody + (1|workerid) + (1|sound), data=t)
summary(m.1b2)
anova(m.1b,m.1b2) # m.1b is not better
m.2b <- clmm(Response ~ target + prosody + (1|workerid) + (1|sound), data=t)
anova(m.1b2,m.2b) #m.1b2 is better

# the larger the rise and the steeper the slope, the higher the response

# on Aux prosody subset
t$target <- relevel(t$target, ref = "stop")
m.3 <- clmm(Response ~ riseAuxf0 + (1|workerid) + (1|sound), data=t[t$prosody == "aux",])
summary(m.3)
m.3b <- clmm(Response ~ target + riseAuxSlope + (1|workerid) + (1|sound), data=t[t$prosody == "aux",])
summary(m.3b)
anova(m.3,m.3b)

# GAMM analysis ----
# This analysis is based on f0 values extracted every 10ms in the subject and the predicate
# The f0 contour is plotted and modeled for the subject and the predicate together
# Praat script: MAR-pitch-over-time.praat
library(mgcv) 
library(itsadug)

d <- read.csv("../f0-analyses-of-stimuli/main-clauses/f0values.csv")
head(d)
nrow(d) 
length(unique(d$File)) #432

# create columns for conditions

# prosodic condition
d$prosody <- d$File
d$prosody <- gsub("[a-z]*-","", d$prosody)
d$prosody <- as.factor(d$prosody)
table(d$prosody)

# target expression
d$target <- d$File
d$target <- gsub("[a-z]+-[a-z]+-","", d$target)
d$target <- gsub("-[A-Z]","", d$target)
d$target <- as.factor(d$target)
table(d$target)

# content
d$content <- d$File
d$content <- gsub("-[a-z]+-[A-Z]","", d$content)
d$content <- as.factor(d$content)
table(d$content)

# plot raw f0 values by content to see if some contents extracted worse than others
# only did two sample contents

ggplot(data=d[d$content == "abby-walk",], aes(x=time, y=f0)) +
  geom_point(na.rm=TRUE,size=.1) + 
  facet_grid(prosody ~ target, scales = "free", space = "fixed") +
  xlab("time (ms)") + 
  ylab("f0 (Hz)")
ggsave("../graphs/raw-f0-abby-walk.pdf",height=14,width=14)

ggplot(data=d[d$content == "maise-whistle",], aes(x=time, y=f0)) +
  geom_point(na.rm=TRUE,size=.1) + 
  facet_grid(prosody ~ target, scales = "free", space = "fixed") +
  xlab("time (ms)") + 
  ylab("f0 (Hz)")
ggsave("../graphs/raw-f0-maise-whistle.pdf",height=14,width=14)

## normalize the f0 values by prosodic condition and target
labels(d)

# find the first and last time of each utterance
minT = aggregate(time ~ File, data=d, FUN=min)
colnames(minT)[ncol(minT)] = 'time.min'
head(minT)

maxT = aggregate(time ~ File, data=d, FUN=max)
colnames(maxT)[ncol(maxT)] = 'time.max'
head(maxT)

# merge the first and last time to one data frame
minmaxT = merge(minT,maxT,by=c('File'))
head(minmaxT)

# add first and last time info to data
d = merge(d,minmaxT,by=c('File'))
head(d)

# normalize each time such that main clause interval + when/before goes from 0 to 1
d$Time <- 0
d$Time <- (d$time - d$time.min) / (d$time.max - d$time.min)

head(d)
sapply(d,function(x) sum(is.na(x))) 
nrow(d) 

# check min and max times 
min(d$Time) #0
max(d$Time) #1

# plots
boxplot(d$time, main='Original time')
boxplot(d$Time, main='Normalized time')

# normalize f0

# calculate talker f0 mean and sd 
d$f0mean = mean(d$f0,na.rm = TRUE)
d$f0mean
d$f0sd = sd(d$f0,na.rm = TRUE)
d$f0sd

head(d)

# z-transform f0: subtract talker f0 mean from each f0 value and divide by talker f0 sd
d$f0z = (d$f0 - d$f0mean) / d$f0sd
head(d)

# plots
par(mfrow=c(1,2))
boxplot(d$f0,main='Original f0')
boxplot(d$f0z,main='Z-transformed f0') 

# # change outliers to NA: f0.z values below -3 and above 3
# nrow(d) #15920
# nrow(d[d$f0.z > 3 & !is.na(d$f0.z),]) #15
# nrow(d[d$f0.z < -3 & !is.na(d$f0.z),]) #83
# # very few data points are changed
# 
# d$f0.z[d$f0.z > 3] <- NA
# d$f0.z[d$f0.z <= -3] <- NA
# nrow(d)
# 
# sapply(d,function(x) sum(is.na(x))) #550
# 
# ggplot(d, aes(x=talker, y=f0.z)) + 
#   geom_boxplot()

# make sure these are factors
str(d$prosody)
table(d$prosody)
d$prosody2 <- ifelse(d$prosody == "A","auxiliary focus",
                     ifelse(d$prosody == "S", "subject focus",
                            ifelse(d$prosody == "T", "target focus", "verb focus")))
d$prosody2 <- as.factor(d$prosody2)
str(d$prosody2)
str(d$content)
str(d$target)

# plot normalized f0 values over time

ggplot(data=d[d$target == "m",], aes(x=time, y=f0,color=prosody2)) +
  geom_point(na.rm=TRUE,size=.1) + 
  #facet_grid(prosody ~ target, scales = "free", space = "fixed") +
  xlab("time (ms)") + 
  ylab("f0 (Hz)")
ggsave("../graphs/f0z-contour-abby-walk.pdf",height=14,width=14)


# transform the data so that each File/utterance is a time series
d <- start_event(d, event=c("File"))
head(d)
summary(d)

## plot normalized f0 values by normalized time ----

# plot manner adverb utterances

# first model to establish residuals
m.tmp <-  bam(f0z ~ s(Time, by = prosody2, bs='ad') + prosody2 + s(Time, content, by=prosody2, bs = "fs", m = 1), 
              data = d[d$target == "m",], discrete = T, nthreads = 2)
macf <- acf_resid(m.tmp)
(rhoval <- macf[2]) #.67

# run model for manner adverbs
m <- bam(f0z ~ s(Time, by = prosody2, bs='ad') + prosody2 + s(Time, content, by=prosody2, bs = "fs", m = 1), 
         data = d[d$target == "m",], discrete = T, nthreads = 2, 
         rho=rhoval, AR.start=d[d$target == "m",]$start.event)
#(smry <- summary(m))


# plot 
pdf("../graphs/GAMM-manner.pdf",width = 5, height = 4)
plot_smooth(m, view='Time',plot_all='prosody2',rm.ranef=T, rug=F, 
            ylab = "z-normalized f0", xlab = "normalized time",
            legend_plot_all = list(x=.7,y=-.2), hide.label = T,
            col = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))
dev.off()

# plot again utterances

# first model to establish residuals
m.tmp <-  bam(f0z ~ s(Time, by = prosody2, bs='ad') + prosody2 + s(Time, content, by=prosody2, bs = "fs", m = 1), 
              data = d[d$target == "a",], discrete = T, nthreads = 2)
macf <- acf_resid(m.tmp)
(rhoval <- macf[2])

# run model 
m <- bam(f0z ~ s(Time, by = prosody2, bs='ad') + prosody2 + s(Time, content, by=prosody2, bs = "fs", m = 1), 
         data = d[d$target == "a",], discrete = T, nthreads = 2, 
         rho=rhoval, AR.start=d[d$target == "a",]$start.event)
#(smry <- summary(m))


# plot 
pdf("../graphs/GAMM-again.pdf",width = 5, height = 4)
plot_smooth(m, view='Time',plot_all='prosody2',rm.ranef=T, rug=F, 
            ylab = "z-normalized f0", xlab = "normalized time",
            legend_plot_all = list(x=.7,y=-.2), hide.label = T,
            col = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))
dev.off()

# plot stop utterances

# first model to establish residuals
m.tmp <-  bam(f0z ~ s(Time, by = prosody2, bs='ad') + prosody2 + s(Time, content, by=prosody2, bs = "fs", m = 1), 
              data = d[d$target == "s",], discrete = T, nthreads = 2)
macf <- acf_resid(m.tmp)
(rhoval <- macf[2])

# run model 
mS <- bam(f0z ~ s(Time, by = prosody2, bs='ad') + prosody2 + s(Time, content, by=prosody2, bs = "fs", m = 1), 
         data = d[d$target == "s",], discrete = T, nthreads = 2, 
         rho=rhoval, AR.start=d[d$target == "s",]$start.event)
#(smry <- summary(m))


# plot 
pdf("../graphs/GAMM-stop.pdf",width = 5, height = 4)
plot_smooth(mS, view='Time',plot_all='prosody2',rm.ranef=T, rug=F, 
            ylab = "z-normalized f0", xlab = "normalized time",
            legend_plot_all = list(x=.7,y=-.2), hide.label = T,
            col = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))
dev.off()

## plot raw f0 values by normalized time ----

# plot manner adverb utterances

# first model to establish residuals
m.tmpM <-  bam(f0 ~ s(Time, by = prosody2, bs='ad') + prosody2 + s(Time, content, by=prosody2, bs = "fs", m = 1), 
              data = d[d$target == "m",], discrete = T, nthreads = 2)
macf <- acf_resid(m.tmpM)
(rhoval <- macf[2]) #.67

# run model for manner adverbs
mM2 <- bam(f0 ~ s(Time, by = prosody2, bs='ad') + prosody2 + s(Time, content, by=prosody2, bs = "fs", m = 1), 
         data = d[d$target == "m",], discrete = T, nthreads = 2, 
         rho=rhoval, AR.start=d[d$target == "m",]$start.event)
#(smry <- summary(m))


# plot 
pdf("../graphs/GAMM-manner-raw.pdf",width = 5, height = 4)
plot_smooth(mM2, view='Time',plot_all='prosody2',rm.ranef=T, rug=F, 
            ylab = "f0 (Hz)", xlab = "normalized time", ylim = c(120,270),
            legend_plot_all = "bottomright", hide.label = T,
            col = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))
dev.off()

# plot again utterances

# first model to establish residuals
m.tmpA <-  bam(f0 ~ s(Time, by = prosody2, bs='ad') + prosody2 + s(Time, content, by=prosody2, bs = "fs", m = 1), 
              data = d[d$target == "a",], discrete = T, nthreads = 2)
macf <- acf_resid(m.tmpA)
(rhoval <- macf[2])

# run model
mA2 <- bam(f0 ~ s(Time, by = prosody2, bs='ad') + prosody2 + s(Time, content, by=prosody2, bs = "fs", m = 1), 
         data = d[d$target == "a",], discrete = T, nthreads = 2, 
         rho=rhoval, AR.start=d[d$target == "a",]$start.event)
#(smry <- summary(m))


# plot 
pdf("../graphs/GAMM-again-raw.pdf",width = 5, height = 4)
plot_smooth(mA2, view='Time',plot_all='prosody2',rm.ranef=F, rug=F, 
            ylab = "f0 (Hz)", legend_plot_all = "bottomright", xlab = "normalized time", ylim = c(120,270), hide.label = T,
            col = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))
dev.off()

# plot stop utterances

# first model to establish residuals
m.tmpS <-  bam(f0 ~ s(Time, by = prosody2, bs='ad') + prosody2 + s(Time, content, by=prosody2, bs = "fs", m = 1), 
              data = d[d$target == "s",], discrete = T, nthreads = 2)
macf <- acf_resid(m.tmpS)
(rhoval <- macf[2])

# run model
mS2 <- bam(f0 ~ s(Time, by = prosody2, bs='ad') + prosody2 + s(Time, content, by=prosody2, bs = "fs", m = 1), 
         data = d[d$target == "s",], discrete = T, nthreads = 2, 
         rho=rhoval, AR.start=d[d$target == "s",]$start.event)
#(smry <- summary(m))


# plot 
pdf("../graphs/GAMM-stop-raw.pdf",width = 5, height = 4)
plot_smooth(mS2, view='Time',plot_all='prosody2',rm.ranef=F, rug=F, 
            ylab = "f0 (Hz)", xlab = "normalized time", ylim = c(120,270),
            legend_plot_all = "bottomright", hide.label = T,
            col = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))
dev.off()
