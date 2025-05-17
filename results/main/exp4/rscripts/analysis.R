# Exp 4
# analysis

# load required packages
library(tidyverse)
library(tidybayes)
library(brms)
library(emmeans)
library(lme4)
library(lmerTest)
library(xtable)

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)
getwd()

# load helper functions
source('../../../helpers.R')

# load cleaned data
d = read_csv("../data/cd.csv")

length(unique(d$participantID)) #72 participants

# exclude controls
t = d %>%
  filter(!(expression == "AI MC" | expression == "NAI MC"))
table(t$expression)

# set reference level
t = t %>%
  mutate(expression = fct_relevel(expression, "be right"))
levels(t$expression)

# fit the model
prior = get_prior(nResponse ~ expression + (1|participantID) + (1|cc),family = Beta(),data=t)
prior

# # set some priors
# priors = set_prior("normal(.5,.5)", class = "b") 
# 
# # for prior predictive check
# prior.check = brm(nResponse ~ expression + (1|participantID) + (1|cc),
#           family=bernoulli(),
#           data=t, 
#           prior = priors,
#           sample_prior = "only")
#           
# pp_check(prior.check)

m.b = brm(nResponse ~ expression + (1|participantID) + (1|cc),
          family=bernoulli(),
          data=t, 
          #prior = priors,
          cores = 4, iter = 4000, warmup = 500,
          control = list(adapt_delta = .97,max_treedepth=15))

# model summary
summary(m.b)

# save the model
saveRDS(m.b,file="../models/bayesian-model.rds")

# read the model
m.b <- readRDS(file="../models/bayesian-model.rds")
m.b

# run posterior predictive checks
p1 <- pp_check(m.b, type = "dens_overlay_grouped", group = "expression", ndraws = 100) +
  scale_x_continuous(breaks = seq(0,1,by=.25)) 
p1

# draws of posterior distributions of estimated marginal means of pairwise differences
pairwise <- m.b %>%
  emmeans(~ expression) %>%
  contrast(method = "pairwise") %>%
  gather_emmeans_draws() %>%
  mean_hdi() %>%
  # create new column of "first" and "second" element in contrast
  mutate(first = gsub(" -.*", "", contrast)) %>%
  mutate(second = gsub(".* -", "", contrast)) %>%
  # sort by mean value
  mutate(contrast = fct_reorder(as.factor(contrast),.value))
pairwise

# save the pairwise comparison
write_csv(pairwise,file="../models/pairwise1.csv")

# load the pairwise comparison
pairwise = read_csv(file="../models/pairwise1.csv")
pairwise

# select relevant columns for printing
pairwise_reduced = pairwise %>%
  select(c(contrast, .value, .lower, .upper))
pairwise_reduced

#### full model output for online supplement ----

tableApp1 = print(xtable(pairwise_reduced),
                  #only.contents = T,
                  include.rownames=FALSE,
                  include.colnames=TRUE,
                  tabular.environment="longtable",
                  floating=FALSE,
                  hline.after = NULL,
                  latex.environments=NULL,
                  booktabs=TRUE,
                  sanitize.text.function = function(x){x},
                  comment = F
)

# write the table, print in latex document in supplement
write(tableApp1, "../models/fullModelOutput.tex")

#### create latex input for Table 1 in paper ----

# select needed columns from the pairwise comparison for the table input
tableInput = pairwise %>%
  select(c(contrast, .value, .lower, .upper, first, second)) %>%
  select(-c(contrast))
tableInput$second = trimws(tableInput$second)
tableInput

# make tableInput a dataframe
tableInput <- as.data.frame(tableInput)
tableInput

# create separate dataframes for each expression
expressions = unique(as.character(t$expression))
expressions

# create a separate dataframe for each predicate
for (p in expressions) {
  assign(paste("data.", p, sep=""), subset(tableInput, tableInput$first == p | tableInput$second == p))
  assign(paste("data.", p, sep=""), get(paste("data.", p, sep="")) %>% mutate(expression = c(p)))
  write(paste("data.",p,sep=""),file=paste("../models/data.",p,sep=""))
}

# change dataframes such that value, lower and upper is consistent by expression in first position

# create a tableData dataframe
tableData = data.frame(expression = character(), comparisonExpression = character(), value = numeric(), lower = numeric(), upper = numeric())
tableData

# fill tableData with the relevant information from the individual predicates' dataframes
for (p in expressions) {
  for (i in 1:nrow(get(paste("data.",p,sep="")))) {
    print(p)
    # define some expressions
    valueOld = get(paste("data.",p,sep=""))[i,]$.value
    lowerOld = get(paste("data.",p,sep=""))[i,]$.lower
    upperOld = get(paste("data.",p,sep=""))[i,]$.upper
    first = get(paste("data.",p,sep=""))[i,]$first
    second = get(paste("data.",p,sep=""))[i,]$second
    expression = get(paste("data.",p,sep=""))[i,]$expression
    # now fill the dataframe
    comparisonExpression = ifelse(expression == first, second, first)
    value = ifelse(expression == first, valueOld, -valueOld)
    lower = ifelse(expression == first, lowerOld, -upperOld)
    upper = ifelse(expression == first, upperOld, -lowerOld)
    tableData = tableData %>%
      add_row(expression = p, comparisonExpression = comparisonExpression, value = value, lower = lower, upper = upper)
  }
}

tableData

# sort expressions in dataframe by mean rating in Exp 2 (asking whether)

exp2 = read.csv("../../exp2/data/cd.csv")
nrow(exp2)

means.exp2 = exp2 %>%
  filter(!(expression == "AI MC" | expression == "NAI MC")) %>%
  group_by(expression) %>%
  summarize(Mean.exp2 = mean(response))
means.exp2

# join the tmp dataframe with tableData
tableData = left_join(tableData, means.exp2)
tableData

tableData$expression = factor(tableData$expression, levels=means.exp2$expression[order(means.exp2$Mean.exp2)], ordered=TRUE)
tableData
levels(tableData$expression)

# also sort the other header row by Exp 2 means
tableData$comparisonExpression = factor(tableData$comparisonExpression, levels=means.exp2$expression[order(means.exp2$Mean.exp2)], ordered=TRUE)
levels(tableData$comparisonExpression)

# sort by mean (first column) and comparisonExpression (second column)
tableData <- tableData %>% arrange(Mean.exp2, comparisonExpression)
tableData

# colorcode the cells (just white = HDI contains 0, red = HDI doesn't contain 0)
tableData$cellColor = ifelse(tableData$lower <= 0 & tableData$upper >= 0, "\\cellcolor{white}",
                             ifelse(tableData$lower < 0 & tableData$upper < 0 & tableData$value <= -1.5, "\\cellcolor{blue}",
                                    ifelse(tableData$lower < 0 & tableData$upper < 0 & -1.5 < tableData$value & tableData$value <= -0.5, "\\cellcolor{blue}",
                                           ifelse(tableData$lower < 0 & tableData$upper < 0 & -.5 < tableData$value & tableData$value <= 0, "\\cellcolor{blue}",
                                                  ifelse(tableData$lower > 0 & tableData$upper > 0 & tableData$value >= 1.5, "\\cellcolor{red}",
                                                         ifelse(tableData$lower > 0 & tableData$upper > 0 & 1.5 > tableData$value & tableData$value > 0.5, "\\cellcolor{red}",
                                                                ifelse(tableData$lower > 0 & tableData$upper > 0 & .5 > tableData$value & tableData$value >= 0, "\\cellcolor{red}", "error")))))))
tableData$cellColor
#view(tableData)

# select relevant columns to make the latex table
tableData = tableData %>%
  select(c(expression,comparisonExpression,cellColor))
tableData

# spread the data wide
tableData = tableData %>%
  spread(comparisonExpression,cellColor)
tableData

# replace NA with gray cells and expressions with color coded versions
tableData = tableData %>% mutate(across(everything(), ~replace_na(.x, "\\cellcolor{black}")))
# tableData = tableData %>%
#   mutate(expression = recode(expression,
#                              "know" = "\\color{orange}{\\bf know}\\color{black}",
#                              "confess" = "\\color{black}{\\bf confess}\\color{black}",
#                              "discover" = "\\color{orange}{\\bf discover}\\color{black}",
#                              "be.right" = "\\color{black}{\\bf be right}\\color{black}",
#                              "confirm" = "\\color{black}{\\bf confirm}\\color{black}",
#                              "medial NRRC" = "\\color{black}{\\bf medial NRRC}\\color{black}",
#                              "final NRRC" = "\\color{black}{\\bf final NRRC}\\color{black}"                             
#   ))

#view(tableData)
tableData
names(tableData)

# turn all lower triangle cells black, by each column
tmp = tableData %>% 
  mutate('be right' = case_when('be right' = TRUE ~ "\\cellcolor{black}"))
tmp = tmp %>% 
  mutate(confirm = case_when(confirm = TRUE & expression != 'be right' ~ "\\cellcolor{black}",
         TRUE ~ confirm))
tmp = tmp %>% 
  mutate(discover = case_when(discover = TRUE & (expression != 'be right' & expression != "confirm") ~ "\\cellcolor{black}",
                             TRUE ~ discover))
tmp = tmp %>% 
  mutate(confess = case_when(confess = TRUE & (expression != 'be right' & expression != "confirm" & expression != "discover") ~ "\\cellcolor{black}",
                              TRUE ~ confess))
tmp = tmp %>% 
  mutate(know = case_when(know = TRUE & (expression != 'be right' & expression != "confirm" 
                                         & expression != "discover" & expression != "confess") ~ "\\cellcolor{black}",
                             TRUE ~ know))
tmp = tmp %>% 
  mutate(`final NRRC` = case_when(`final NRRC` = TRUE & (expression != 'be right' & expression != "confirm" 
                                         & expression != "discover" & expression != "confess"
                                         & expression != "know") ~ "\\cellcolor{black}",
                          TRUE ~ `final NRRC`))

tmp
tableData = tmp

# now create the table to include in the paper
table1 = print(xtable(tableData),
               only.contents = T,
               include.rownames=FALSE,
               include.colnames=FALSE,
               floating=FALSE,
               hline.after = NULL,
               latex.environments=NULL,
               booktabs=TRUE,
               sanitize.text.function = function(x){x},
               comment = F
)

write(table1, "../models/table1.tex")

# non-Bayesian pairwise comparison ----

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