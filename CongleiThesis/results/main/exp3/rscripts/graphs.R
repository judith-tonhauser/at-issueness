# 13_explicitIgnorance
# graphs

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)
getwd()
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
source('helpers.R')

# load cleaned data
cd = read_csv("../data/cd.csv")

names(cd)


length(unique(cd$participantID)) #3 participants

# Fig 1: plot of mean certainty ratings from Exp 1a of Degen & Tonhauser 2022 -----
# import data from repo
#cd <- read_csv("https://raw.githubusercontent.com/judith-tonhauser/projective-probability/master/results/5-projectivity-no-fact/data/cd.csv")
#summary(cd)

# mean projectivity by predicate, including the main clause controls
means = cd %>%
  group_by(expression) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, expression = fct_reorder(as.factor(expression),Mean))
means

# define colors for the predicates
cols = data.frame(V=levels(means$expression))

cols$Colors = ifelse(cols$V %in% c("know", "discover", "confirm", "confess", "be right", "mnrrc", "fnrrc"), "black", 
                     ifelse(cols$V %in% c("MC"),"black","#009E73"))


cols$V <- factor(cols$V, levels = cols[order(as.character(means$expression)),]$V, ordered = TRUE)

levels(cols$V)


cols$V <- factor(cols$V, levels = cols[order(as.character(means$expression)),]$V, ordered = TRUE)
levels(cols$V)



subjmeans = cd %>%
  group_by(expression,participantID) %>%
  summarize(Mean = mean(response)) 
subjmeans$expression <- factor(subjmeans$expression, levels = unique(levels(means$expression)))


levels(subjmeans$expression)
#view(subjmeans)

# version of Figure 2 Degen & Tonhauser 2022 Language paper
# plot of means, 95% CIs and participants' ratings 
ggplot(means, aes(x=expression, y=Mean)) +
  geom_violin(data=subjmeans, aes(fill = NA), scale="width",color = "grey", linewidth = 0, alpha = .3) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0.1,color="black") +
  geom_point(size=2.5,color="black") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  scale_alpha(range = c(.3,1)) +
  # scale_shape_manual(values=rev(c(23, 24, 25, 22, 21)),labels=rev(c("factive","nonfactive","main clause\ncontrols")),name="Predicate type") +
  # scale_fill_manual(values=rev(c("#D55E00","#009E73","black")),labels=rev(c("factive","nonfactive","main clause\ncontrols")),name="Predicate type") +
  # guides(fill=FALSE, shape=F) +
  theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 45, hjust = 1, 
                                                                 color=cols$Colors)) +
  theme(legend.position="none") +
  theme(panel.grid.major.x = element_blank()) +
  ylab("Mean 'direct dissent' (at-issueness) ratings") +
  xlab(" ") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1)) 
ggsave("../graphs/mean-DD-Ratings.pdf",height=4.5,width=7)



