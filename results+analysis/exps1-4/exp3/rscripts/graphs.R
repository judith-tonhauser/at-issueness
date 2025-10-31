# Exp3 Direct Dissent Diagnostic
# graph

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# load required packages
library(tidyverse)

# color-blind-friendly palette
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

theme_set(theme_bw())

# load helper functions
source('../../helpers.R')

# load cleaned data
d = read_csv("../data/cd.csv")
length(unique(d$participantID)) #71 participants

# exclude controls
t = d %>%
  filter(!(expression == "controlBad" | expression == "controlGood"))
table(t$expression)

# code response such that 1 = at-issue and 0 = not-at-issue
t$response = 1-t$response
table(t$response)

# mean rating by expression
means = t %>%
  group_by(expression) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, expression = fct_reorder(as.factor(expression),Mean))
means

# range
min(means$Mean)
max(means$Mean)
max(means$Mean) - min(means$Mean)

# plot 
ggplot(data=means, aes(x=expression, y=Mean)) +
  geom_violin(data=t, aes(x=expression, y=response), scale="width", fill = "grey", linewidth=0, alpha=.4) +
  geom_point(size=2.5,color="black") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0.1,color="black") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 45, hjust = 1)) +
  theme(legend.position="none") +
  theme(panel.grid.major.x = element_blank()) +
  ylab("Mean naturalness rating \n (higher rating = more at-issue)") +
  xlab("Expression") 
ggsave("../graphs/mean-ratings.pdf",height=4.5,width=7)




