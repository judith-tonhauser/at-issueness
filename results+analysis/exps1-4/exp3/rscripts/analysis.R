# Exp 3
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

# load helper functions
source('../../../helpers.R')

# load cleaned data
d = read_csv("../data/cd.csv")
nrow(d)

length(unique(d$participantID)) #71 participants

# exclude controls
t = d %>%
  filter(!(expression == "controlBad" | expression == "controlGood"))
table(t$expression)

# set reference level
t = t %>%
  mutate(expression = fct_relevel(expression, "be right"))
levels(t$expression)

# response distribution before transformation
summary(t$response)

# first, because response assumes values of 0 and 1, which beta regression cannot handle, transform: (Smithson & Verkuilen 2006)
# y_new = (y_old * (n−1) + 0.5) / n (where n is the sample size)
# note: first rescaling of y'=(y-a)/(b-a) not necessary because highest and lowest value are 0 and 1 already
t$betaresponse = (t$response*(nrow(t)-1) + .5)/nrow(t)
summary(t$betaresponse)

# # fit the model
# prior = get_prior(betaresponse ~ expression + (1|participantID) + (1|cc),family = Beta(),data=t)
# prior
# 
# # set priors
# #priors = c(set_prior("cauchy(0, .001)", class = "b"))
# 
# # for prior predictive check
# # prior.check = brm(betaresponse ~ expression + (1|participantID) + (1|cc),
# #           data=t,
# #           prior = priors,
# #           iter = 7000,
# #           control = list(adapt_delta = .97, max_treedepth=20),
# #           sample_prior = "only")
# # 
# # pp_check(prior.check)
# 
# #betamodel = bf(betaresponse ~ expression + (1|participantID) + (1|cc),
# betamodel = bf(betaresponse ~ expression + (1|participantID),
#                phi ~ expression , # beta distribution's precision
#                family = Beta())
# 
# m.b = brm(formula = betamodel,
#           family=Beta(),
#           data=t, 
#           cores = 1, iter = 3000, warmup = 500,
#           control = list(adapt_delta = .95, max_treedepth=15))
# 
# # run posterior predictive checks
# p1 <- pp_check(m.b, type = "dens_overlay_grouped", group = "expression", ndraws = 100) +
#   scale_x_continuous(breaks = seq(0,1,by=.25)) 
# p1
# 
# # save the model
# saveRDS(m.b,file="../models/bayesian-model.rds")

# read the model
m.b <- readRDS(file="../models/bayesian-model.rds")
m.b

# model summary
summary(m.b)

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

# load the pairwise comparison ----
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
tableApp1

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
  obj_name <- paste0("data.", p)
  assign(obj_name, tableInput %>%
           filter(first == p | second == p) %>%
           mutate(expression = p) %>%
           mutate(across(where(is.list), as.character))
  )
  write.table(
    get(obj_name),
    file = paste0("../models/data.", p, ".tsv"),
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
  )
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
                             ifelse(tableData$lower < 0 & tableData$upper < 0 & tableData$value <= -1.5, "\\cellcolor{red}",
                                    ifelse(tableData$lower < 0 & tableData$upper < 0 & -1.5 < tableData$value & tableData$value <= -0.5, "\\cellcolor{red}",
                                           ifelse(tableData$lower < 0 & tableData$upper < 0 & -.5 < tableData$value & tableData$value <= 0, "\\cellcolor{red}",
                                                  ifelse(tableData$lower > 0 & tableData$upper > 0 & tableData$value >= 1.5, "\\cellcolor{blue}",
                                                         ifelse(tableData$lower > 0 & tableData$upper > 0 & 1.5 > tableData$value & tableData$value > 0.5, "\\cellcolor{blue}",
                                                                ifelse(tableData$lower > 0 & tableData$upper > 0 & .5 > tableData$value & tableData$value >= 0, "\\cellcolor{blue}", "error")))))))
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

order <- c(
  "medial NRRC",
  "final NRRC",
  "know",
  "confess",
  "discover",
  "confirm",
  "be right"
)

tmp <- tableData

for (i in seq_along(order)) {
  col <- order[i]
  
  tmp[[col]] <- ifelse(
    match(tmp$expression, order) >= i,
    "\\cellcolor{black}",
    tmp[[col]]
  )
}

tmp
tableData = tmp


# now create the table to include in the paper
table1 = print(xtable(tableData),
               only.contents = T,
               include.rownames=FALSE,
               include.colnames=FALSE,
               floating=FALSE,
               hline.after = c(nrow(tableData)),
               latex.environments=NULL,
               booktabs=FALSE,
               sanitize.text.function = function(x){x},
               comment = F
)
table1 <- paste(table1, collapse = "\n")
table1 <- sub("[[:space:]]+$", "", table1)
cat(table1, file = "../models/table1.tex", sep = "")

