# at-issueness: pilot 1 (unembedded sentences)
# preprocessing.R

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# load relevant packages, set background color and load helpers
library(tidyverse)
theme_set(theme_bw())
source('helpers.R')

# color blind palette
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# load data
d = read.csv("../data/cd.csv")
summary(d)
nrow(d) #6840 / 40 judgments = 171 AmE Turkers (5 non-native speakers already excluded)
names(d)
head(d)

# box plot by item, including controls, with raw ratings
mean = aggregate(response~item, data=dd, FUN="mean")
mean$YMin = mean$response - aggregate(response~item, data=d, FUN="ci.low")$response
mean$YMax = mean$response + aggregate(response~item, data=d, FUN="ci.high")$response
mean

# order the items by mean response
mean$item <- factor(mean$item, levels=mean[order(mean$response), "item"])
d$item <- factor(d$item, levels=mean[order(mean$response), "item"])

dodge = position_dodge(.9)

ggplot(d, aes(x=item, y=response)) + 
  geom_boxplot(lwd=1,width=0.2,position=dodge) +
  stat_summary(fun.y=mean, geom="point", color="black",fill="black", shape=21, size=3,position=position_dodge(.9)) +
  #geom_jitter(alpha = 1/15, width=0.25) +
  #scale_x_discrete(labels=c("control"="control","manner"="manner adverbs","stop" = "stop","again"="again")) +
  theme(text = element_text(size=12)) +
  ylab("Ratings")+
  xlab("Expression")
ggsave(f="../graphs/boxplot1.pdf",height=3,width=6.5)

