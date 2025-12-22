# comparisons between Exps 5 and 6
## range of by-content means
## spearman rank correlations

# set directory
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source('../../helpers.R')

# load required packages
library(tidyverse)
library(dplyr)
library(dichromat)
library(forcats)
library(ggrepel)

theme_set(theme_bw())

# Exps 5-6 ----

# load clean data
exp5 = read.csv("../exp5/data/data_preprocessed.csv")
nrow(exp5) #12584
exp6 = read.csv("../exp6/data/data_preprocessed.csv")
nrow(exp6) #11440

# code response such that 1 = at-issue and 0 = not-at-issue
exp5$response = 1-exp5$response
table(exp5$response)
exp6$response = 1-exp6$response
table(exp6$response)

# calculate means for target contents for each experiment

means.exp5 = exp5 %>%
  filter(!(trigger == "MC")) %>%
  filter(!(question_type == "projective")) %>%
  group_by(trigger) %>%
  summarize(Mean.exp5 = mean(response))
means.exp5

means.exp6 = exp6 %>%
  filter(!(trigger == "MC")) %>%
  filter(!(question_type == "projective")) %>%
  group_by(trigger) %>%
  summarize(Mean.exp6 = mean(response))
means.exp6

# ranges
min(means.exp5$Mean.exp5) # min 0.07115702
max(means.exp5$Mean.exp5) # max 0.823595
max(means.exp5$Mean.exp5) - min(means.exp5$Mean.exp5) # range 0.752438

min(means.exp6$Mean.exp6) # min 0.09418182
max(means.exp6$Mean.exp6) # max 0.8219091
max(means.exp6$Mean.exp6) - min(means.exp6$Mean.exp6) # range 0.7277273


# bind the data
means = left_join(means.exp5,means.exp6)
means

# Spearman rank correlations

cor.test(x=means$Mean.exp5, y=means$Mean.exp6, method = 'spearman') # 0.9308271 


