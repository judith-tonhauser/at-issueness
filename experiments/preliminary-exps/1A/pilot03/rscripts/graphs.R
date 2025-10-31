# at-issueness exp 1A
# graphs

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# load required packages
library(tidyverse)
library(ggrepel)
library(dichromat)
library(forcats)
library(ggrepel)

# color-blind-friendly palette
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

theme_set(theme_bw())

# load helper functions
source('../../../helpers.R')

# load cleaned data
d = read_tsv("../data/d.tsv")
length(unique(d$participantID)) #xx participants

# reduce to target data
t = d %>%
  filter(target == "target")
table(t$predicate)
table(t$ai)

# Fig 1: plot mean naturalness rating by condition and predicate ----

# calculate mean naturalness rating by condition and predicate
nat.means = t %>%
  group_by(predicate,ai) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  #mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, predicate = fct_reorder(as.factor(predicate),Mean))
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh)
nat.means

# order predicates by mean ai in mc condition
tmp = t %>%
  filter(ai == "mc") %>%
  group_by(predicate) %>%
  summarize(Mean = mean(response))
tmp

nat.means$predicate = factor(t$predicate, levels = tmp$predicate[order(tmp$Mean)], ordered = TRUE)
levels(nat.means$predicate)
t$predicate = factor(t$predicate, levels = tmp$predicate[order(tmp$Mean)], ordered = TRUE)
levels(t$predicate)

# plot of naturalness means, with participants' individual responses
ggplot(nat.means, aes(x=ai, y=Mean)) +
  geom_point(shape=21,stroke=.5,size=3, color="black", fill = "black") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0.1,color="black") +
  geom_violin(data=t,aes(x=ai, y=response),scale="width",color="gray80", fill = "gray80", alpha = .3) +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels = c("0",".2",".4",".6",".8","1")) +
  guides(fill=FALSE) +
  theme(legend.position="top") +
  ylab("Mean naturalness rating") +
  xlab("At-issueness") +  
  facet_wrap(. ~ predicate)
ggsave("../graphs/naturalness-by-condition-and-predicate.pdf",height=3,width=7)

# Fig 2: comparison to asking-whether ratings ----

# import asking-whether ratings from this repo, where the ai means were calculated over Exps 1 and 2
# for the Glossa Psycholinguistics paper
tmp <- read_csv("https://raw.githubusercontent.com/judith-tonhauser/attitude_preds_projection/master/results/expsDT12/data/ai.means.csv")
summary(tmp)

# calculate ai means by predicate (short_trigger) and reduce to the six predicates
# the subscript _AW identifies these data as the "asking whether" data
ai.means = tmp %>%
  filter(short_trigger == "be right" | short_trigger == "think" | short_trigger == "know" |
           short_trigger == "confess" | short_trigger =="say" | short_trigger == "discover") %>%
  group_by(short_trigger) %>%
  summarize(Mean_AW = mean(Mean_ai), CILow = ci.low(Mean_ai), CIHigh = ci.high(Mean_ai)) %>%
  mutate(YMin_AW = Mean_AW - CILow, YMax_AW = Mean_AW + CIHigh) %>%
  select(-c(CILow, CIHigh)) %>%
  rename("predicate" = "short_trigger")
ai.means

# calculate mean responses from target data, reduce to ai = cc data, and bind with "asking whether" data
t.means = t %>% 
  group_by(predicate,ai) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh) %>%
  select(-c(CILow, CIHigh)) %>%
  left_join(., ai.means, by = "predicate")
t.means
  
# plot with CC at-issue
ggplot(t.means[t.means$ai == "cc",], aes(x = Mean, y = Mean_AW),label = predicate) +
  geom_point(stroke=.5,size=2.5,color="black") +
  geom_errorbar(aes(ymin=YMin_AW,ymax=YMax_AW),width=.01,color="black") +
  geom_errorbarh(aes(xmin=YMin,xmax=YMax),height=.01,color="black") +
  geom_text_repel(aes(label = predicate),
                  #box.padding   = 0.35, 
                  point.padding = 0.5,
                  segment.color = 'grey50') +
  scale_x_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels= c("0",".2",".4",".6",".8","1")) +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels= c("0",".2",".4",".6",".8","1")) +
  geom_abline(intercept=1,slope=-1,color="gray70",linetype="dashed") +
  xlab("Mean naturalness rating (when CC is at-issue)") +
  ylab("Mean asking-whether rating") +
  theme(panel.spacing.x = unit(4, "mm")) +
  coord_fixed(ratio = 1) 
ggsave("../graphs/comparison-1A-CC-at-issue-asking-whether.pdf",height=4,width=4)


# compare using means and violin plots
ggplot(t.means, aes(x=predicate, y=Mean, group = ai)) +
  geom_point(shape=21,stroke=.5,size=3, aes(color = ai, fill=ai)) +
  geom_point(data=t.means, aes(x=predicate, y=Mean_AW), color = "green", size=3) +
  #geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0.1,color="black") +
  #geom_errorbar(data=t.means,aes(ymin=YMin_AW,ymax=YMax_AW),width=0.1,color="red") +
  #geom_violin(data=t,aes(x=predicateAI, y=response),scale="width",color="gray80", fill = "gray80", alpha = .3) +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels = c("0",".2",".4",".6",".8","1")) +
  guides(fill=FALSE) +
  theme(legend.position="top") +
  ylab("Mean naturalness rating") +
  xlab("Predicate") 
ggsave("../graphs/means-by-predicate-and-ai.pdf",height=3,width=7)



