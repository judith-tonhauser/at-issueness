# Exp4 Yes but test
# graphs

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# load required packages
library(tidyverse)

# color-blind-friendly palette
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

theme_set(theme_bw())

# load helper functions
source('../../../helpers.R')

# load cleaned data
d = read_csv("../data/cd.csv")

length(unique(d$participantID)) #72 participants

# exclude controls
t = d %>%
  filter(!(expression == "AI MC" | expression == "NAI MC"))
table(t$expression)
  
# mean rating by expression
means = t %>%
  group_by(expression) %>%
  summarize(Mean = mean(nResponse), CILow = ci.low(nResponse), CIHigh = ci.high(nResponse)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, expression = fct_reorder(as.factor(expression),Mean))
means

# range
min(means$Mean)
max(means$Mean)
max(means$Mean) - min(means$Mean)

# plot 
ggplot(data=means, aes(x=expression, y=Mean)) +
  geom_jitter(data=t, aes(x=expression,y=nResponse), alpha = .2, width=.2, height=.2) +
  geom_point(size=3,color="black") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0.1,color="black") +
  scale_y_continuous(limits = c(-0.2,1.2),breaks = c(0,0.2,0.4,0.6,0.8,1)) +
  theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 45, hjust = 1)) +
  theme(legend.position="none") +
  theme(panel.grid.major.x = element_blank()) +
  ylab("Proportion of \"no\" choice \n (higher response = more not-at-issue") +
  xlab("Expression") 
ggsave("../graphs/mean-ratings.pdf",height=4.5,width=7)
