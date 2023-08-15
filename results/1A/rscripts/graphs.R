# 13_explicitIgnorance
# graphs

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# load required packages
require(tidyverse)
library(ggrepel)
library(dichromat)
library(forcats)
library(ggrepel)

# color-blind-friendly palette
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

theme_set(theme_bw())

# load helper functions
source('../../helpers.R')

# load cleaned data
d = read_tsv("../data/cd.tsv")

names(d)
table(d$context)

length(unique(d$participantID)) #370 participants


# Fig 1: plot of mean naturalness ratings in explicit ignorance context ----

# target data: explicit ignorance context
# merge the two controls into one, but exclude them
table(d$expression)
t = d %>%
  filter(context == "explicitIgnorance") %>%
  mutate(expression = recode(expression, "controlGood1" = "controls", "controlGood2" = "controls")) %>%
  filter(expression != "controls")

# calculate mean naturalness rating by expression, including the fillers, in explicit ignorance context
nat.meansEIC = t %>%
  group_by(expression) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, expression = fct_reorder(as.factor(expression),Mean))
nat.meansEIC
levels(nat.meansEIC$expression)

# color code the expressions
factives <- c("know", "discover", "be annoyed", "reveal", "see")
hard.triggers <- c("too", "also","cleft","again")
soft.triggers <- c("stop", "continue")

nat.meansEIC$ps = ifelse(nat.meansEIC$expression %in% soft.triggers, "softTrigger", 
               ifelse(nat.meansEIC$expression %in% hard.triggers, "hardTrigger",
                      ifelse(nat.meansEIC$expression %in% factives, "factive", "other")))

t$ps = ifelse(t$expression %in% soft.triggers, "softTrigger", 
              ifelse(t$expression %in% hard.triggers, "hardTrigger",
                     ifelse(t$expression %in% factives, "factive", "other")))


table(nat.meansEIC$ps, nat.meansEIC$expression)

text.color <- ifelse(nat.meansEIC$expression[order(nat.meansEIC$Mean)] %in% factives, '#D55E00',
                     ifelse(nat.meansEIC$expression[order(nat.meansEIC$Mean)] %in% hard.triggers, "black",
                            ifelse(nat.meansEIC$expression[order(nat.meansEIC$Mean)] %in% soft.triggers, "#009E73",
                            "gray80")))
text.color

t$expression = factor(t$expression, levels = nat.meansEIC$expression[order(nat.meansEIC$Mean)], ordered = TRUE)

# plot of naturalness means, with participants' individual responses
ggplot(nat.meansEIC, aes(x=expression, y=Mean)) +
  geom_violin(data=t[t$context == "explicitIgnorance",],aes(x=expression, y=response),
               scale="width",color="gray80", fill = "gray80") +
  geom_point(aes(group = ps, fill = ps), shape=21,stroke=.5,size=3, color="black") +
  scale_fill_manual(values=c('#D55E00','black','gray80','#009E73')) + 
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0.1,color="black") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels = c("0",".2",".4",".6",".8","1")) +
  guides(fill=FALSE) +
  theme(legend.position="top") +
  ylab("Mean naturalness rating \n in explicit ignorance context") +
  xlab("Expression") +  
  theme_dark() +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1, color = text.color)) 
ggsave("../graphs/explicit-ignorance-naturalness-by-predicate.pdf",height=3,width=7)

# Fig 2: plot of mean naturalness ratings in by context ----
# for 20 clause-embedding predicates only

# calculate mean naturalness rating by predicate and context
table(d$expression)
table(d$context)  #explicit ignorance / factL / factH

nat.means = d %>%
  filter(expression != "practice" & expression != "controlGood1" & expression != "controlGood2" & expression != "controlGood3" & expression != "controlGood4") %>%
  filter(expression != "also" & expression != "too" & expression != "again" & expression != "cleft" &
         expression != "stop" & expression != "continue") %>%
  group_by(expression,context) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh) %>%
  ungroup %>%
  select(-c(CILow,CIHigh)) %>%
  mutate(context = as.factor(context))
nat.means
table(nat.means$context)
nat.means$expression <- as.factor(nat.means$expression)
levels(nat.means$expression)

t = d %>%
  filter(expression != "practice" & expression != "controlGood1" & expression != "controlGood2" & expression != "controlGood3" & expression != "controlGood4") %>%
  filter(expression != "also" & expression != "too" & expression != "again" & expression != "cleft" &
           expression != "stop" & expression != "continue") %>%
  mutate(context = as.factor(context))
levels(t$context)

# order the predicate by Language paper certainty means
# https://github.com/judith-tonhauser/projective-probability/tree/master/results/5-projectivity-no-fact
# tmp <- read_csv("https://raw.githubusercontent.com/judith-tonhauser/projective-probability/master/results/5-projectivity-no-fact/data/cd.csv")
# summary(tmp)
# 
# # target data
# tmp2 <- tmp %>%
#   filter(verb != "MC") %>%
#   group_by(verb) %>%
#   summarize(MeanCertain = mean(response)) %>% 
#   mutate(expression = fct_reorder(as.factor(verb),MeanCertain))
# tmp2
# 
# # order predicates by mean projection in Language paper Exp 1
# nat.means$expression = factor(nat.means$expression, levels=tmp2$expression[order(tmp2$expression)], ordered=TRUE)
# t$expression = factor(t$expression, levels=tmp2$expression[order(tmp2$expression)], ordered=TRUE)

# order predicates by mean naturalness rating in EIC
tmp <- d %>%
  filter(context == "explicitIgnorance") %>%
  filter(expression != "practice" & expression != "controlGood1" & expression != "controlGood2" & expression != "controlGood3" & expression != "controlGood4") %>%
  filter(expression != "also" & expression != "too" & expression != "again" & expression != "cleft" &
           expression != "stop" & expression != "continue") %>%
  group_by(expression) %>%
  summarize(Mean = mean(response)) %>%
  mutate(expression = fct_reorder(as.factor(expression),Mean))
  
nat.means$expression = factor(nat.means$expression, levels=tmp$expression[order(tmp$expression)], ordered=TRUE)
t$expression = factor(t$expression, levels=tmp$expression[order(tmp$expression)], ordered=TRUE)
levels(nat.means$expression)
levels(t$expression)

# order the contexts: EI, low, high
levels(nat.means$context)
nat.means$context = factor(nat.means$context, levels = c("explicitIgnorance", "factL", "factH"))
levels(t$context)
t$context = factor(t$context, levels = c("explicitIgnorance", "factL", "factH"))

fill.color <- ifelse(levels(nat.means$expression) %in% factives, '#D55E00', "gray80")
fill.color

# to color the facets differently
library(ggh4x)

strip <- strip_themed(background_x = elem_list_rect(fill = fill.color))

# violinplot
ggplot(nat.means, aes(x=context, y=Mean)) +
  geom_violin(data=t, aes(x=context, y=response, fill = context), scale="width", linewidth = 0) +
  geom_point(aes(fill = context), shape=21,stroke=.5,size=2, color="black") +
  scale_fill_manual(values=c('gray80',"#56B4E9",'#E69F00'), 
                    name = "Context", 
                    labels=c('explicit ignorance', 'low prior probability','high prior probability')) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0.1,color="black") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels = c("0",".2",".4",".6",".8","1")) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank()) +
  theme(legend.position="top") +
  ylab("Mean naturalness rating") +
  xlab("Context") +
  facet_wrap2(. ~ expression, nrow = 2, strip = strip) +
  theme(strip.background = element_rect(fill="white")) +
  theme(strip.text = element_text(color = "black")) 
ggsave("../graphs/ORDER-by-LANGUAGE-naturalness-by-context-and-predicate.pdf",height=4,width=9)

# plot of mean naturalness ratings against mean certainty ratings

# get data from Degen & Tonhauser, 2022 (Language)
# https://github.com/judith-tonhauser/projective-probability/tree/master/results/5-projectivity-no-fact
dt <- read_csv("https://raw.githubusercontent.com/judith-tonhauser/projective-probability/master/results/5-projectivity-no-fact/data/cd.csv")
summary(dt)
table(dt$verb)

# target data
dt <- dt %>%
  filter(verb != "MC")
table(dt$verb)

dt.means = dt %>%
  group_by(verb) %>%
  summarize(MeanCertain = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMinCertain = MeanCertain - CILow, YMaxCertain = MeanCertain + CIHigh, expression = fct_reorder(as.factor(verb),MeanCertain))
dt.means
levels(dt.means$expression)

# reduce the acceptability data to the 20 predicates in the explicit ignorance context
t = d %>%
  filter(context == "explicitIgnorance") %>%
  filter(expression != "controlGood1" & expression != "controlGood2" & expression != "again"
         & expression != "also" & expression != "too" & expression != "continue"
         & expression != "stop" & expression != "cleft")

# calculate mean naturalness rating by expression
nat.means = t %>%
  group_by(expression) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, expression = fct_reorder(as.factor(expression),Mean))
nat.means

# bind the data
data = left_join(nat.means, dt.means, by = "expression")
data
summary(data)

# plot of naturalness means against certainty means
ggplot(data, aes(x=Mean, y=MeanCertain),label = expression) +
  geom_point(shape=21,stroke=.5,size=2,color="black") +
  #geom_smooth(method="lm") +
  geom_text_repel(aes(label = expression),
                   #box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  geom_errorbarh(aes(xmin=YMin,xmax=YMax),height=.01,color="black") +
  geom_errorbar(aes(ymin=YMinCertain,ymax=YMaxCertain),width=.01,color="black") +
  #geom_point(data = nt[nt$context != "explicitIgnorance",], aes(x=expression, y=response), shape=21,fill="gray60", alpha=.5, color="blue") +
  scale_x_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  guides(fill=FALSE) +
  geom_abline(intercept = 1,slope = -1, col="red", lty = "dashed") + 
  #theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 45, hjust = 1)) +
  #theme(legend.position="top") +
  coord_fixed() +
  xlab("Mean naturalness rating \n in explicit ignorance context") +
  ylab("Mean certainty rating \n (from Degen & Tonhauser 2022)") 
  #theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1)) 
ggsave("../graphs/mean-acceptability-against-mean-certainty.pdf",height=5,width=5)

# calculate Spearman rank correlation
corr <- cor.test(x=data$Mean, y=data$MeanCertain, method = 'spearman', exact = FALSE)
corr

# Fig X: plot of mean naturalness rating by expression and context ----

# calculate mean naturalness rating by predicate and context
table(d$expression)
table(d$context)  #explicit ignorance / factL / factH

nat.means = d %>%
  filter(expression != "practice" & expression != "controlGood1" & expression != "controlGood2" & expression != "controlGood3" & expression != "controlGood4") %>%
  #filter(expression != "also" & expression != "too" & expression != "again" & expression != "cleft" &
  #         expression != "stop" & expression != "continue") %>%
  group_by(expression,context) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh) %>%
  ungroup %>%
  select(-c(CILow,CIHigh)) %>%
  mutate(context = as.factor(context))
nat.means
table(nat.means$context)
nat.means$expression <- as.factor(nat.means$expression)
levels(nat.means$expression)

t = d %>%
  filter(expression != "practice" & expression != "controlGood1" & expression != "controlGood2" & expression != "controlGood3" & expression != "controlGood4") %>%
  #filter(expression != "also" & expression != "too" & expression != "again" & expression != "cleft" &
  #         expression != "stop" & expression != "continue") %>%
  mutate(context = as.factor(context))
levels(t$context)

# order the predicate by Language paper certainty means
# https://github.com/judith-tonhauser/projective-probability/tree/master/results/5-projectivity-no-fact
# tmp <- read_csv("https://raw.githubusercontent.com/judith-tonhauser/projective-probability/master/results/5-projectivity-no-fact/data/cd.csv")
# summary(tmp)
# 
# # target data
# tmp2 <- tmp %>%
#   filter(verb != "MC") %>%
#   group_by(verb) %>%
#   summarize(MeanCertain = mean(response)) %>% 
#   mutate(expression = fct_reorder(as.factor(verb),MeanCertain))
# tmp2
# 
# # order predicates by mean projection in Language paper Exp 1
# nat.means$expression = factor(nat.means$expression, levels=tmp2$expression[order(tmp2$expression)], ordered=TRUE)
# t$expression = factor(t$expression, levels=tmp2$expression[order(tmp2$expression)], ordered=TRUE)

# order predicates by mean naturalness rating in EIC
tmp <- d %>%
  filter(context == "explicitIgnorance") %>%
  filter(expression != "practice" & expression != "controlGood1" & expression != "controlGood2" & expression != "controlGood3" & expression != "controlGood4") %>%
  #filter(expression != "also" & expression != "too" & expression != "again" & expression != "cleft" &
  #         expression != "stop" & expression != "continue") %>%
  group_by(expression) %>%
  summarize(Mean = mean(response)) %>%
  mutate(expression = fct_reorder(as.factor(expression),Mean))

nat.means$expression = factor(nat.means$expression, levels=tmp$expression[order(tmp$expression)], ordered=TRUE)
t$expression = factor(t$expression, levels=tmp$expression[order(tmp$expression)], ordered=TRUE)
levels(nat.means$expression)
levels(t$expression)

# order the contexts: EI, low, high
levels(nat.means$context)
nat.means$context = factor(nat.means$context, levels = c("factH","factL","explicitIgnorance"))
levels(t$context)
t$context = factor(t$context, levels = c("factH","factL","explicitIgnorance"))

# color code the expressions
factives <- c("know", "discover", "be annoyed", "reveal", "see")
hard.triggers <- c("too", "also","cleft","again")
soft.triggers <- c("stop", "continue")

nat.means$ps = ifelse(nat.means$expression %in% soft.triggers, "softTrigger", 
                      ifelse(nat.means$expression %in% hard.triggers, "hardTrigger",
                             ifelse(nat.means$expression %in% factives, "factive", "other")))

t$ps = ifelse(t$expression %in% soft.triggers, "softTrigger", 
              ifelse(t$expression %in% hard.triggers, "hardTrigger",
                     ifelse(t$expression %in% factives, "factive", "other")))
table(t$ps)

table(nat.means$ps, nat.means$expression)

# text.color <- ifelse(nat.means$expression[order(nat.means$Mean)] %in% factives, '#D55E00', 
#                      ifelse(nat.means$expression[order(nat.means$Mean)] %in% hard.triggers, "black", 
#                             ifelse(nat.means$expression[order(nat.means$Mean)] %in% soft.triggers, "#56B4E9",
#                                    "gray80")))

text.color <- ifelse(levels(nat.means$expression) %in% factives, '#D55E00',  
                     ifelse(levels(nat.means$expression) %in% hard.triggers, "black", 
                            ifelse(levels(nat.means$expression) %in% soft.triggers, "#56B4E9",
                                   "gray80")))
fill.color

text.color

# violinplot
ggplot(nat.means, aes(x=expression, y=Mean)) +
  facet_grid(context ~ .) +
  geom_violin(data=t, aes(x=expression, y=response, fill = ps), scale="width") +
  scale_fill_manual(values=c('#D55E00','black','gray80','#56B4E9')) +
  geom_point(aes(group = ps, fill = ps), shape=21,stroke=.5,size=3, color="black") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0.1,color="black") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels = c("0",".2",".4",".6",".8","1")) +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1, color = text.color)) +
  ylab("Mean naturalness rating") +
  xlab("Expression") +
  theme(strip.background = element_rect(fill="white")) +
  theme(strip.text = element_text(color = "black")) 
ggsave("../graphs/naturalness-by-context-and-predicate.pdf",height=5,width=9)



# plot of mean naturalness ratings in explicit ignorance and neutral context ----
# for 20 clause-embedding predicates only

# calculate mean naturalness rating by expression and context for the target data
table(d$expression)
table(d$context)  #explicit ignorance / factL / factH

# create context variable that only distinguishes explicit ignorance and neutral
d = d %>%
  mutate(context2 = recode(context, "factL" = "neutral", "factH" = "neutral"))
table(d$context2)

nat.means = d %>%
  filter(expression != "practice" & expression != "controlGood1" & expression != "controlGood2" & expression != "controlGood3" & expression != "controlGood4") %>%
  filter(expression != "also" & expression != "too" & expression != "again" & expression != "cleft" &
           expression != "stop" & expression != "continue") %>%
  group_by(expression,context2) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh) %>%
  select(-c(CILow,CIHigh))
nat.means
table(nat.means$context2)
nat.means$expression <- as.factor(nat.means$expression)
str(nat.means$expression)

# # sort expressions by difference between ratings in the two contexts
tmp = nat.means %>%
  select(expression, context2, Mean) %>%
  pivot_wider(names_from = context2, values_from = Mean) %>%
  mutate(diff = explicitIgnorance - neutral) %>%
  mutate(diff = ifelse(is.na(diff), 0, diff)) %>% # remove this with full data
  ungroup() %>%
  mutate(expression = fct_reorder(expression, diff))
tmp
table(tmp$expression)
str(tmp$expression)
levels(tmp$expression)

nat.means$expression = factor(nat.means$expression, levels=tmp$expression[order(tmp$diff)], ordered=TRUE)
levels(nat.means$expression)

# plot
ggplot(nat.means, aes(x=expression, y=Mean, group = context2, fill = context2)) +
  geom_bar(stat="identity", color = "black", position=position_dodge(.9)) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=0.1,color="black", position=position_dodge(.9)) +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  scale_fill_manual(values=c('gray40',"pink"), name = "Context") + 
  #guides(fill=FALSE) +
  theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 45, hjust = 1)) +
  theme(legend.position="top") +
  ylab("Mean naturalness rating") +
  xlab("Expression") +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1)) 
ggsave("../graphs/naturalness-by-context2-and-predicate.pdf",height=4,width=7)
  