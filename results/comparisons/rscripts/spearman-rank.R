# spearman rank correlations between Exps 1-4 and Exps 5-6

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

# Exps 1-4 ----

# load clean data
exp1 = read.csv("../../exp1/data/cd.csv")
nrow(exp1)
exp2 = read.csv("../../exp2/data/cd.csv")
nrow(exp2)
exp3 = read.csv("../../exp3/data/cd.csv")
nrow(exp3)
exp4 = read.csv("../../exp4/data/cd.csv")
nrow(exp4)

# calculate means for target contents for each experiment

means.exp1 = exp1 %>%
  filter(!(expression == "AI MC" | expression == "NAI MC")) %>%
  group_by(expression) %>%
  summarize(Mean.exp1 = mean(response))
means.exp1
  
means.exp2 = exp2 %>%
  filter(!(expression == "AI MC" | expression == "NAI MC")) %>%
  group_by(expression) %>%
  summarize(Mean.exp2 = mean(response))
means.exp2

means.exp3 = exp3 %>%
  filter(!(expression == "controlGood" | expression == "controlBad")) %>%
  group_by(expression) %>%
  summarize(Mean.exp3 = mean(response))
means.exp3

means.exp4 = exp4 %>%
  filter(!(expression == "AI MC" | expression == "NAI MC")) %>%
  group_by(expression) %>%
  summarize(Mean.exp4 = mean(nResponse))
means.exp4

# bind the data
means = left_join(means.exp1,means.exp2) %>%
  left_join(.,means.exp3) %>%
  left_join(.,means.exp4)
means

# Spearman rank correlations

cor.test(x=means$Mean.exp1, y=means$Mean.exp2, method = 'spearman') # 0.1071429
cor.test(x=means$Mean.exp1, y=means$Mean.exp3, method = 'spearman') # -0.2857143 
cor.test(x=means$Mean.exp1, y=means$Mean.exp4, method = 'spearman') # -0.1785714 

cor.test(x=means$Mean.exp2, y=means$Mean.exp3, method = 'spearman') # 0.6428571
cor.test(x=means$Mean.exp2, y=means$Mean.exp4, method = 'spearman') # 0.7857143

cor.test(x=means$Mean.exp3, y=means$Mean.exp4, method = 'spearman') # 0.7857143

# exclude "be right"
means2 = means %>%
  filter(expression != "be right")
means2

# Spearman rank correlations

cor.test(x=means2$Mean.exp1, y=means2$Mean.exp2, method = 'spearman') # 0.7714286
cor.test(x=means2$Mean.exp1, y=means2$Mean.exp3, method = 'spearman') # 0.08571429 
cor.test(x=means2$Mean.exp1, y=means2$Mean.exp4, method = 'spearman') # 0.3142857 

cor.test(x=means2$Mean.exp2, y=means2$Mean.exp3, method = 'spearman') # 0.6571429
cor.test(x=means2$Mean.exp2, y=means2$Mean.exp4, method = 'spearman') # 0.6571429

cor.test(x=means2$Mean.exp3, y=means2$Mean.exp4, method = 'spearman') # 0.7714286

# Exps 5-6 ----

# load clean data
exp5 = read.csv("../../exp5/data/data_preprocessed.csv")
nrow(exp5) #12584
exp6 = read.csv("../../exp6/data/data_preprocessed.csv")
nrow(exp6) #11440

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

# bind the data
means = left_join(means.exp5,means.exp6)
means

# Spearman rank correlations

cor.test(x=means$Mean.exp5, y=means$Mean.exp6, method = 'spearman') # 0.9308271 


