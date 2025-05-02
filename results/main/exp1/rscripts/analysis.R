# Exp 1 
# analysis

# load required packages
require(tidyverse)
library(emmeans)
library(lme4)
library(lmerTest)

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# load helper functions
source('../../../helpers.R')

# load cleaned data
d = read_csv("../data/cd.csv")

length(unique(d$participantID)) #69 participants

# exclude controls
t = d %>%
  filter(!(expression == "AI MC" | expression == "NAI MC"))
table(t$expression)

# pairwise comparisons on expression using tukey 
# model with expression as fixed effect so you can do multiple comparisons 
m = lmer(response ~ expression + (1|participantID), data=t, REML=F)
m

pc = emmeans(m, pairwise ~ expression)
pc

# contrast                 estimate    SE  df t.ratio p.value
# be right - confess       -0.16638 0.037 414  -4.493  0.0002
# be right - confirm       -0.26580 0.037 414  -7.178  <.0001
# be right - discover      -0.19768 0.037 414  -5.338  <.0001
# be right - final NRRC    -0.11058 0.037 414  -2.986  0.0468
# be right - know          -0.10971 0.037 414  -2.963  0.0500
# be right - medial NRRC   -0.13029 0.037 414  -3.519  0.0087
# confess - confirm        -0.09942 0.037 414  -2.685  0.1047
# confess - discover       -0.03130 0.037 414  -0.845  0.9799
# confess - final NRRC      0.05580 0.037 414   1.507  0.7407
# confess - know            0.05667 0.037 414   1.530  0.7264
# confess - medial NRRC     0.03609 0.037 414   0.975  0.9592
# confirm - discover        0.06812 0.037 414   1.840  0.5221
# confirm - final NRRC      0.15522 0.037 414   4.192  0.0007
# confirm - know            0.15609 0.037 414   4.215  0.0006
# confirm - medial NRRC     0.13551 0.037 414   3.659  0.0053
# discover - final NRRC     0.08710 0.037 414   2.352  0.2220
# discover - know           0.08797 0.037 414   2.376  0.2116
# discover - medial NRRC    0.06739 0.037 414   1.820  0.5353
# final NRRC - know         0.00087 0.037 414   0.023  1.0000
# final NRRC - medial NRRC -0.01971 0.037 414  -0.532  0.9984
# know - medial NRRC       -0.02058 0.037 414  -0.556  0.9979