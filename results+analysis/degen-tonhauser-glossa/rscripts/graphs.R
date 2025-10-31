# script to create Fig X from Degen & Tonhauser Glossa paper
# Exp1: interaction of prior beliefs, at-issueness and projection
# for 20 clause-embedding predicates
# graphs.R

# load required packages
library(tidyverse)
library(ggrepel)
library(dichromat)
library(forcats)
library(RColorBrewer)
library(curl) # to read data from github repo
library(gridExtra)

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

theme_set(theme_bw()) 

source('../../helpers.R')

# color-blind-friendly palette
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# load the data

d <- read_csv("https://raw.githubusercontent.com/judith-tonhauser/projection-interactions/refs/heads/main/results/exp1/data/d.csv")
nrow(d) #10100

# code response such that 1 = at-issue and 0 = not-at-issue
d$ai = 1-d$ai
table(d$ai)

# Fig. X: by-predicate not-at-issueness ----

# sort predicates by not-at-issueness mean
nai.means = d %>%
  filter(short_trigger != "MC") %>%
  group_by(short_trigger) %>%
  summarize(Mean_nai = mean(ai), CILow=ci.low(ai),CIHigh=ci.high(ai)) %>%
  mutate(YMin=Mean_nai-CILow,YMax=Mean_nai+CIHigh) %>%
  select(!c(CILow,CIHigh)) %>%
  mutate(short_trigger = fct_reorder(as.factor(short_trigger),Mean_nai))
nai.means

d = d %>%
  filter(short_trigger != "MC") %>%
  mutate(short_trigger = fct_relevel(short_trigger,levels(nai.means$short_trigger)))
levels(d$short_trigger)

# ggplot(means, aes(x=verb, y=Mean, fill=VeridicalityGroup)) +
#   geom_violin(data=subjmeans,scale="width",linewidth = 0, alpha = .4)

# plot
ggplot() +
  geom_violin(data=d,aes(x=short_trigger, y=ai), scale="width",linewidth = .1, fill = "black", alpha = .2) +
  geom_point(data=nai.means, aes(x=short_trigger, y=Mean_nai), stroke=.5,size=2.5,color="black") +
  geom_errorbar(data=nai.means, aes(x=short_trigger, ymin=YMin,ymax=YMax), width=0.1,color="black") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  theme(text = element_text(size=12), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(legend.position="bottom") +
  theme(panel.grid.major.x = element_blank()) +
  ylab("Mean 'asking whether' rating \n (higher rating indicates more at-issue)") +
  xlab("Predicate")
ggsave("../graphs/mean-asking-whether-ratings.pdf",height=4.5,width=7)


ggplot(d, aes(x=short_trigger, y=ai)) +
  geom_point(size=1, alpha=.1) +
  geom_point(data=nai.means, aes(x=short_trigger,y=Mean_nai), size=3) +
  #scale_color_manual(values=c("#E69F00","#999999")) +
  #scale_fill_manual(values=c("#E69F00","#999999")) +
  #guides(color = "none", fill = "none") +
  ylab("Asking-whether ratings (not-at-issueness) \n (higher rating indicates more not-at-issue)") +
  #ylab("Certainty ratings (projection) \n (higher rating indicates more projection)") +
  #scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  theme(panel.spacing.x = unit(4, "mm"))
ggsave(f="../graphs/projection-by-ai.pdf",height=5,width=5)



# Supplemental figures ----

subjmeans = d %>%
  group_by(eventItem,workerid,prior_type) %>%
  summarize(Mean = mean(prior)) %>%
  ungroup() %>% 
  mutate(prior_type = fct_relevel(as.factor(as.character(prior_type)),"low_prior"))
subjmeans$eventItem <- factor(subjmeans$eventItem, levels = unique(levels(means$eventItem)))
levels(subjmeans$eventItem)
names(subjmeans)

ggplot(means, aes(x=eventItem, y=Mean, color=prior_type, shape=prior_type, fill=prior_type)) + 
  geom_point(data=subjmeans,aes(fill=prior_type,color=prior_type),shape=21,alpha=.05) +
  geom_point(stroke=.5,size=3,color="black") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  scale_shape_manual(values=c(25, 24),labels=c("lower probability","higher probability"),name="Fact") +
  scale_fill_manual(values=c("#56B4E9","#E69F00"),labels=c("lower probability","higher probability"),name="Fact") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels= c("0",".2",".4",".6",".8","1")) +
  scale_color_manual(name="Fact", breaks=c("lower probability","higher probability"),labels=c("lower probability","higher probability"), 
                     values=c("#56B4E9","#E69F00")) +
  theme(legend.position = "top", legend.text=element_text(size=12)) +
  coord_flip() +
  ylab("Mean prior probability rating") +
  xlab("Content") 
ggsave(f="../graphs/prior-ratings.pdf",height=5,width=8)

## Supplement D: By-block by-predicate correlations ----

# load the data
d_projai <- read_csv("../data/d_projai.csv")
nrow(d_projai) #5120
d_aiproj <- read_csv("../data/d_aiproj.csv")
nrow(d_aiproj) #4980

# sort predicates by projection mean
proj.means = d %>%
  group_by(short_trigger) %>%
  summarize(Mean_proj = mean(projective)) %>%
  mutate(short_trigger = fct_rev(fct_reorder(as.factor(short_trigger),Mean_proj)))
proj.means

d_projai = d_projai %>%
  mutate(short_trigger = fct_relevel(short_trigger,levels(proj.means$short_trigger)))
levels(d_projai$short_trigger)

d_aiproj = d_aiproj %>%
  mutate(short_trigger = fct_relevel(short_trigger,levels(proj.means$short_trigger)))
levels(d_aiproj$short_trigger)

# color-code the predicates
d_projai = d_projai %>%
  mutate(predicateType = case_when(short_trigger == "discover" ~ "factive",
                                   short_trigger == "know" ~ "factive",
                                   short_trigger == "be annoyed" ~ "factive",
                                   short_trigger == "reveal" ~ "factive",
                                   short_trigger == "see" ~ "factive",
                                   TRUE ~ "nonfactive"))

d_aiproj = d_aiproj %>%
  mutate(predicateType = case_when(short_trigger == "discover" ~ "factive",
                                   short_trigger == "know" ~ "factive",
                                   short_trigger == "be annoyed" ~ "factive",
                                   short_trigger == "reveal" ~ "factive",
                                   short_trigger == "see" ~ "factive",
                                   TRUE ~ "nonfactive"))

# proj/ai

# projection by prior
ggplot(d_projai, aes(x=prior, y=projective)) +
  geom_smooth(aes(color=predicateType,fill=predicateType),method="lm") +
  geom_point(aes(color=predicateType), shape=20, size=1, alpha=.2) +
  scale_color_manual(values=c("#E69F00","#999999")) +
  scale_fill_manual(values=c("#E69F00","#999999")) +
  guides(color = "none", fill = "none") +
  xlab("Prior probability ratings \n (higher rating indicates higher prior probability)") +
  ylab("Certainty ratings (projection) \n (higher rating indicates more projection)") +
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  theme(panel.spacing.x = unit(4, "mm")) +
  coord_fixed(ratio = 1) +
  facet_wrap(. ~ short_trigger) +
  theme(strip.background=element_rect(fill="white")) 
ggsave(f="../graphs/SUP-projai-projection-by-prior.pdf",height=5,width=5)

# projection by at-issueness
ggplot(d_projai, aes(x=ai, y=projective)) +
  geom_smooth(aes(color=predicateType,fill = predicateType),method="lm") +
  geom_point(aes(color=predicateType), shape=20, size=1, alpha=.2) +
  scale_color_manual(values=c("#E69F00","#999999")) +
  scale_fill_manual(values=c("#E69F00","#999999")) +
  guides(color = "none", fill = "none") +
  xlab("Asking-whether ratings (not-at-issueness) \n (higher rating indicates more not-at-issue)") +
  ylab("Certainty ratings (projection) \n (higher rating indicates more projection)") +
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  theme(panel.spacing.x = unit(4, "mm")) +
  coord_fixed(ratio = 1) +
  facet_wrap(. ~ short_trigger) +
  theme(strip.background=element_rect(fill="white"))
ggsave(f="../graphs/SUP-projai-projection-by-ai.pdf",height=5,width=5)

# projection by at-issueness and prior
ggplot(d_projai, aes(x=ai, y=projective)) +
  geom_smooth(aes(color=predicateType,fill=predicateType,linetype = prior_type),method="lm") +
  scale_linetype_manual(name = "Prior probability", 
                        labels = c("high", "low"),
                        values=c("solid", "dotted")) +
  geom_point(aes(color=predicateType), shape=20, size=1, alpha=.2) +
  scale_color_manual(values=c("#E69F00","#999999")) +
  scale_fill_manual(values=c("#E69F00","#999999")) +
  guides(color = "none", fill = "none") +
  xlab("Asking-whether ratings (not-at-issueness) \n (higher rating indicates more not-at-issue)") +
  ylab("Certainty ratings (projection) \n (higher rating indicates more projection)") +
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  theme(panel.spacing.x = unit(4, "mm")) +
  guides(color = "none", linetype = "none") +
  #guides(linetype = guide_legend(override.aes = list(fill = NA))) +
  theme(legend.key = element_rect(fill = "white"), legend.position = "right") +
  theme(legend.background = element_rect(fill ="white")) +
  coord_fixed(ratio = 1) +
  facet_wrap(. ~ short_trigger) +
  theme(strip.background=element_rect(fill="white")) 
ggsave(f="../graphs/SUP-projai-projection-by-ai-and-prior.pdf",height=5,width=5)

# # at-issueness by prior
ggplot(d_projai, aes(x=prior, y=ai)) +
  geom_smooth(aes(color=predicateType,fill=predicateType),method="lm") +
  geom_point(aes(color=predicateType), shape=20, size=1, alpha=.2) +
  scale_color_manual(values=c("#E69F00","#999999")) +
  scale_fill_manual(values=c("#E69F00","#999999")) +
  guides(color = "none", fill = "none") +
  xlab("Prior probability ratings \n (higher rating indicates higher prior probability)") +
  ylab("Asking-whether ratings (not-at-issueness) \n (higher rating indicates more not-at-issue)") +
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  theme(panel.spacing.x = unit(4, "mm")) +
  coord_fixed(ratio = 1) +
  facet_wrap(. ~ short_trigger) +
  theme(strip.background=element_rect(fill="white"))
ggsave(f="../graphs/SUP-projai-ai-by-prior.pdf",height=5,width=5)

# ai/proj

# projection by prior
ggplot(d_aiproj, aes(x=prior, y=projective)) +
  geom_smooth(aes(color=predicateType,fill=predicateType),method="lm") +
  geom_point(aes(color=predicateType), shape=20, size=1, alpha=.2) +
  scale_color_manual(values=c("#E69F00","#999999")) +
  scale_fill_manual(values=c("#E69F00","#999999")) +
  guides(color = "none", fill = "none") +
  xlab("Prior probability ratings \n (higher rating indicates higher prior probability)") +
  ylab("Certainty ratings (projection) \n (higher rating indicates more projection)") +
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  theme(panel.spacing.x = unit(4, "mm")) +
  coord_fixed(ratio = 1) +
  facet_wrap(. ~ short_trigger) +
  theme(strip.background=element_rect(fill="white")) 
ggsave(f="../graphs/SUP-aiproj-projection-by-prior.pdf",height=5,width=5)

# projection by at-issueness
ggplot(d_aiproj, aes(x=ai, y=projective)) +
  geom_smooth(aes(color=predicateType,fill = predicateType),method="lm") +
  geom_point(aes(color=predicateType), shape=20, size=1, alpha=.2) +
  scale_color_manual(values=c("#E69F00","#999999")) +
  scale_fill_manual(values=c("#E69F00","#999999")) +
  guides(color = "none", fill = "none") +
  xlab("Asking-whether ratings (not-at-issueness) \n (higher rating indicates more not-at-issue)") +
  ylab("Certainty ratings (projection) \n (higher rating indicates more projection)") +
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  theme(panel.spacing.x = unit(4, "mm")) +
  coord_fixed(ratio = 1) +
  facet_wrap(. ~ short_trigger) +
  theme(strip.background=element_rect(fill="white"))
ggsave(f="../graphs/SUP-aiproj-projection-by-ai.pdf",height=5,width=5)

# projection by at-issueness and prior
ggplot(d_aiproj, aes(x=ai, y=projective)) +
  geom_smooth(aes(color=predicateType,fill=predicateType,linetype = prior_type),method="lm") +
  scale_linetype_manual(name = "Prior probability", 
                        labels = c("high", "low"),
                        values=c("solid", "dotted")) +
  geom_point(aes(color=predicateType), shape=20, size=1, alpha=.2) +
  scale_color_manual(values=c("#E69F00","#999999")) +
  scale_fill_manual(values=c("#E69F00","#999999")) +
  guides(color = "none", fill = "none") +
  xlab("Asking-whether ratings (not-at-issueness) \n (higher rating indicates more not-at-issue)") +
  ylab("Certainty ratings (projection) \n (higher rating indicates more projection)") +
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  theme(panel.spacing.x = unit(4, "mm")) +
  guides(color = "none", linetype = "none") +
  #guides(linetype = guide_legend(override.aes = list(fill = NA))) +
  theme(legend.key = element_rect(fill = "white"), legend.position = "right") +
  theme(legend.background = element_rect(fill ="white")) +
  coord_fixed(ratio = 1) +
  facet_wrap(. ~ short_trigger) +
  theme(strip.background=element_rect(fill="white")) 
ggsave(f="../graphs/SUP-aiproj-projection-by-ai-and-prior.pdf",height=5,width=5)

# # at-issueness by prior
ggplot(d_aiproj, aes(x=prior, y=ai)) +
  geom_smooth(aes(color=predicateType,fill=predicateType),method="lm") +
  geom_point(aes(color=predicateType), shape=20, size=1, alpha=.2) +
  scale_color_manual(values=c("#E69F00","#999999")) +
  scale_fill_manual(values=c("#E69F00","#999999")) +
  guides(color = "none", fill = "none") +
  xlab("Prior probability ratings \n (higher rating indicates higher prior probability)") +
  ylab("Asking-whether ratings (not-at-issueness) \n (higher rating indicates more not-at-issue)") +
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  theme(panel.spacing.x = unit(4, "mm")) +
  coord_fixed(ratio = 1) +
  facet_wrap(. ~ short_trigger) +
  theme(strip.background=element_rect(fill="white"))
ggsave(f="../graphs/SUP-aiproj-ai-by-prior.pdf",height=5,width=5)

## Supplement F: Cross-experiment comparison of ratings ----

#### prior (Exp 1 vs Open Mind Exps 1 and 2) ----

# load the relevant data
exp1 <- read_csv("../data/d.csv")
nrow(exp1) #10100

# exp1 of Open Mind paper (Degen & Tonhauser 2021)
om_exp1 <- read_csv("https://raw.githubusercontent.com/judith-tonhauser/projective-probability/master/results/9-prior-projection/data/cd.csv")
nrow(om_exp1) #7436

# exp2a of Open Mind paper
om_exp2a <- read_csv("https://raw.githubusercontent.com/judith-tonhauser/projective-probability/master/results/1-prior/data/cd.csv")
nrow(om_exp2a) #1650

# remove controls from Open Mind paper exp1 and exp2a
om_exp1 = om_exp1 %>%
  filter(short_trigger != "MC")
nrow(om_exp1) #5720

om_exp2a = om_exp2a %>%
  filter(item != "F1" & item != "F2")
nrow(om_exp2a) #1500

# create contentFact column for Open Mind paper exp1 and exp2a
table(exp1$contentFact) # Zoe calculated the tip-factL

table(om_exp1$eventItem) # these are the contents
table(om_exp1$prior_type) # high_prior low_prior
om_exp1 = om_exp1 %>%
  mutate(factNew = case_when(prior_type == "high_prior" ~ "factH",
                             prior_type == "low_prior" ~ "factL",
                             TRUE ~ "ERROR")) %>%
  mutate(contentFact = paste(eventItem, factNew, sep = "-"))

# create contentFact
om_exp2a$prior_typeNew <- om_exp2a$itemType
om_exp2a$prior_typeNew <- gsub("H", "factH",om_exp2a$prior_typeNew)
om_exp2a$prior_typeNew <- gsub("L", "factL",om_exp2a$prior_typeNew)
table(om_exp2a$prior_typeNew)

# create content column
om_exp2a$content <- om_exp2a$item
om_exp2a$content <- gsub("H", "", om_exp2a$content)
om_exp2a$content <- gsub("L", "", om_exp2a$content)
om_exp2a$content <- gsub("10", "Zoe calculated the tip", om_exp2a$content) 
om_exp2a$content <- gsub("11", "Danny ate the last cupcake", om_exp2a$content)
om_exp2a$content <- gsub("12", "Frank got a cat", om_exp2a$content)
om_exp2a$content <- gsub("13", "Jackson ran ten miles",  om_exp2a$content)
om_exp2a$content <- gsub("14", "Jayden rented a car", om_exp2a$content)
om_exp2a$content <- gsub("15", "Tony had a drink last night", om_exp2a$content)
om_exp2a$content <- gsub("16", "Josh learned to ride a bike yesterday", om_exp2a$content)        
om_exp2a$content <- gsub("17", "Owen shoveled snow last winter", om_exp2a$content)       
om_exp2a$content <- gsub("18",  "Julian dances salsa", om_exp2a$content)
om_exp2a$content <- gsub("19", "Jon walks to work", om_exp2a$content)                    
om_exp2a$content <- gsub("20", "Charley speaks Spanish", om_exp2a$content)
om_exp2a$content <- gsub("2",  "Josie went on vacation to France", om_exp2a$content)
om_exp2a$content <- gsub("6", "Mia drank 2 cocktails last night", om_exp2a$content)
om_exp2a$content <- gsub("1", "Mary is pregnant", om_exp2a$content)
om_exp2a$content <- gsub("3", "Emma studied on Saturday morning", om_exp2a$content)  
om_exp2a$content <- gsub("4", "Olivia sleeps until noon", om_exp2a$content)
om_exp2a$content <- gsub("5", "Sophia got a tattoo", om_exp2a$content)
om_exp2a$content <- gsub("7", "Isabella ate a steak on Sunday", om_exp2a$content)
om_exp2a$content <- gsub("8", "Emily bought a car yesterday", om_exp2a$content)
om_exp2a$content <- gsub("9",  "Grace visited her sister", om_exp2a$content)
om_exp2a$content <- gsub("Jackson ran ten miles", "Jackson ran 10 miles",  om_exp2a$content)
table(om_exp2a$content)

om_exp2a$contentFact <- paste(om_exp2a$content, om_exp2a$prior_typeNew, sep = "-")
table(om_exp2a$contentFact)

# select relevant columns from each of the three data sets
exp1 <- exp1 %>%
  select(prior,contentFact)
nrow(exp1) #10100
om_exp1 <- om_exp1 %>%
  select(prior,contentFact)
nrow(om_exp1) #5720
om_exp2a <- om_exp2a %>%
  select(response,contentFact) %>%
  rename("prior" = "response")
nrow(om_exp2a) #1500

# calculate by-contentFact prior indicates in each dataset, label indicates differently

exp1.means <- exp1 %>%
  group_by(contentFact) %>%
  summarize(Mean_prior_Exp1 = mean(prior), CILow=ci.low(prior),CIHigh=ci.high(prior)) %>%
  ungroup() %>%
  mutate(YMin_prior_Exp1=Mean_prior_Exp1-CILow,YMax_prior_Exp1=Mean_prior_Exp1+CIHigh) %>%
  select(!c(CILow,CIHigh))
summary(exp1.means)
nrow(exp1.means) #40

om_exp1.means <- om_exp1 %>%
  group_by(contentFact) %>%
  summarize(Mean_prior_OMExp1 = mean(prior), CILow=ci.low(prior),CIHigh=ci.high(prior)) %>%
  ungroup() %>%
  mutate(YMin_prior_OMExp1=Mean_prior_OMExp1-CILow,YMax_prior_OMExp1=Mean_prior_OMExp1+CIHigh) %>%
  select(!c(CILow,CIHigh))
summary(om_exp1.means)
nrow(om_exp1.means) #40

om_exp2a.means <- om_exp2a %>%
  group_by(contentFact) %>%
  summarize(Mean_prior_OMExp2a = mean(prior), CILow=ci.low(prior),CIHigh=ci.high(prior)) %>%
  ungroup() %>%
  mutate(YMin_prior_OMExp2a=Mean_prior_OMExp2a-CILow,YMax_prior_OMExp2a=Mean_prior_OMExp2a+CIHigh) %>%
  select(!c(CILow,CIHigh))
summary(om_exp2a.means)
nrow(om_exp2a.means) #40

# combine the data
prior.means <- left_join(exp1.means, om_exp1.means, by = c("contentFact")) %>%
  left_join(., om_exp2a.means, by = c("contentFact"))
nrow(prior.means) #40
summary(prior.means)

# Spearman rank correlations

cor.test(prior.means$Mean_prior_Exp1, prior.means$Mean_prior_OMExp1, method=c("spearman"))
# .99
cor.test(prior.means$Mean_prior_Exp1, prior.means$Mean_prior_OMExp2a, method=c("spearman"))
# .99
cor.test(prior.means$Mean_prior_OMExp1, prior.means$Mean_prior_OMExp2a, method=c("spearman"))
# .98

# how many judgments? 
count = exp1 %>%
  group_by(contentFact) %>%
  tally(sort=T)
summary(count$n)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#224.0   247.8   252.5   252.5   257.2   281.0

count = om_exp1 %>%
  group_by(contentFact) %>%
  tally(sort=T)
summary(count$n)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#128     136     143     143     150     158

count = om_exp2a %>%
  group_by(contentFact) %>%
  tally(sort=T)
summary(count$n)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#36.0    36.0    37.5    37.5    39.0    39.0 

# plots 

# exp1 vs om exp1
ggplot(prior.means, aes(x=Mean_prior_Exp1, y=Mean_prior_OMExp1)) +
  geom_abline(intercept=0, slope=1, linetype="dashed", color = "gray", size=1) +
  geom_point(shape=16, size=2, alpha=1) +
  xlab("Prior probability rating Exp. 1") +
  ylab("Prior probability rating OM Exp. 1") +
  scale_x_continuous(expand = expansion(mult = c(0, 0)), breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0)), breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  theme(panel.spacing.x = unit(4, "mm")) +
  coord_fixed(ratio = 1) +
  ggtitle("Spearman's rho = .99")
ggsave(f="../graphs/SUP-priorExp1-by-priorOMExp1.pdf",height=3,width=3)

# exp1 vs om exp2a
ggplot(prior.means, aes(x=Mean_prior_Exp1, y=Mean_prior_OMExp2a)) +
  geom_abline(intercept=0, slope=1, linetype="dashed", color = "gray", size=1) +
  geom_point(shape=16, size=2, alpha=1) +
  xlab("Prior probability rating Exp. 1") +
  ylab("Prior probability rating OM Exp. 2a") +
  scale_x_continuous(expand = expansion(mult = c(0, 0)), breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0)), breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  theme(panel.spacing.x = unit(4, "mm")) +
  coord_fixed(ratio = 1) +
  ggtitle("Spearman's rho = .99")
ggsave(f="../graphs/SUP-priorExp1-by-priorOMExp2a.pdf",height=3,width=3)

# exp om 1 vs om exp2a
ggplot(prior.means, aes(x=Mean_prior_OMExp1, y=Mean_prior_OMExp2a)) +
  geom_abline(intercept=0, slope=1, linetype="dashed", color = "gray", size=1) +
  geom_point(shape=16, size=2, alpha=1) +
  xlab("Prior probability rating OM Exp. 1") +
  ylab("Prior probability rating OM Exp. 2a") +
  scale_x_continuous(expand = expansion(mult = c(0, 0)), breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0)), breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  theme(panel.spacing.x = unit(4, "mm")) +
  coord_fixed(ratio = 1) +
  ggtitle("Spearman's rho = .98")
ggsave(f="../graphs/SUP-priorOMExp1-by-priorOMExp2a.pdf",height=3,width=3)


#### certainty (Exp 1 vs Exp 2) ----

# load the data
exp1 <- read_csv("../data/d.csv")
nrow(exp1) #10100

exp2 <- read_csv("../../exp2/data/d.csv")
nrow(exp2) #10000

# calculate certainty means: by-predicate, by-predicate/item, by-predicate/item/fact

exp1.Pred.means <- exp1 %>%
  group_by(short_trigger) %>%
  summarize(Mean_certainty_Pred_Exp1 = mean(projective), CILow=ci.low(projective),CIHigh=ci.high(projective)) %>%
  ungroup() %>%
  mutate(YMin_certainty_Pred_Exp1=Mean_certainty_Pred_Exp1-CILow,YMax_certainty_Pred_Exp1=Mean_certainty_Pred_Exp1+CIHigh) %>%
  select(!c(CILow,CIHigh))
summary(exp1.Pred.means)
nrow(exp1.Pred.means) #20

exp2.Pred.means <- exp2 %>%
  group_by(short_trigger) %>%
  summarize(Mean_certainty_Pred_Exp2 = mean(projective), CILow=ci.low(projective),CIHigh=ci.high(projective)) %>%
  ungroup() %>%
  mutate(YMin_certainty_Pred_Exp2=Mean_certainty_Pred_Exp2-CILow,YMax_certainty_Pred_Exp2=Mean_certainty_Pred_Exp2+CIHigh) %>%
  select(!c(CILow,CIHigh))
summary(exp2.Pred.means)
nrow(exp2.Pred.means) #20

exp1.PredItem.means <- exp1 %>%
  group_by(short_trigger,content) %>%
  summarize(Mean_certainty_PredItem_Exp1 = mean(projective), CILow=ci.low(projective),CIHigh=ci.high(projective)) %>%
  ungroup() %>%
  mutate(YMin_certainty_PredItem_Exp1=Mean_certainty_PredItem_Exp1-CILow,YMax_certainty_PredItem_Exp1=Mean_certainty_PredItem_Exp1+CIHigh) %>%
  select(!c(CILow,CIHigh))
summary(exp1.PredItem.means)
nrow(exp1.PredItem.means) #400

exp2.PredItem.means <- exp2 %>%
  group_by(short_trigger,content) %>%
  summarize(Mean_certainty_PredItem_Exp2 = mean(projective), CILow=ci.low(projective),CIHigh=ci.high(projective)) %>%
  ungroup() %>%
  mutate(YMin_certainty_PredItem_Exp2=Mean_certainty_PredItem_Exp2-CILow,YMax_certainty_PredItem_Exp2=Mean_certainty_PredItem_Exp2+CIHigh) %>%
  select(!c(CILow,CIHigh))
summary(exp2.PredItem.means)
nrow(exp2.PredItem.means) #400

exp1.PredItemFact.means <- exp1 %>%
  group_by(short_trigger,contentFact,) %>%
  summarize(Mean_certainty_PredItemFact_Exp1 = mean(projective), CILow=ci.low(projective),CIHigh=ci.high(projective)) %>%
  ungroup() %>%
  mutate(YMin_certainty_PredItemFact_Exp1=Mean_certainty_PredItemFact_Exp1-CILow,YMax_certainty_PredItemFact_Exp1=Mean_certainty_PredItemFact_Exp1+CIHigh) %>%
  select(!c(CILow,CIHigh))
summary(exp1.PredItemFact.means)
nrow(exp1.PredItemFact.means) #800

exp2.PredItemFact.means <- exp2 %>%
  group_by(short_trigger,contentFact,) %>%
  summarize(Mean_certainty_PredItemFact_Exp2 = mean(projective), CILow=ci.low(projective),CIHigh=ci.high(projective)) %>%
  ungroup() %>%
  mutate(YMin_certainty_PredItemFact_Exp2=Mean_certainty_PredItemFact_Exp2-CILow,YMax_certainty_PredItemFact_Exp2=Mean_certainty_PredItemFact_Exp2+CIHigh) %>%
  select(!c(CILow,CIHigh))
summary(exp2.PredItemFact.means)
nrow(exp2.PredItemFact.means) #800

# combine the data
pred.means <- left_join(exp1.Pred.means, exp2.Pred.means, by = c("short_trigger"))
nrow(pred.means) #20
summary(pred.means)

predItem.means <- left_join(exp1.PredItem.means, exp2.PredItem.means, by = c("short_trigger","content"))
nrow(predItem.means) #400
summary(predItem.means)

predItemFact.means <- left_join(exp1.PredItemFact.means, exp2.PredItemFact.means, by = c("short_trigger","contentFact"))
nrow(predItemFact.means) #400
summary(predItemFact.means)

# Spearman rank correlations

cor.test(pred.means$Mean_certainty_Pred_Exp1, pred.means$Mean_certainty_Pred_Exp2, method=c("spearman"))
# .98
cor.test(predItem.means$Mean_certainty_PredItem_Exp1, predItem.means$Mean_certainty_PredItem_Exp2, method=c("spearman"), exact=FALSE)
# .9
cor.test(predItemFact.means$Mean_certainty_PredItemFact_Exp1, predItemFact.means$Mean_certainty_PredItemFact_Exp2, method=c("spearman"), exact=FALSE)
# .85

# how many ratings
count = exp1 %>%
  group_by(short_trigger,content) %>%
  tally(sort=T)
summary(count$n)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#14.00   22.00   25.00   25.25   28.00   43.00 

count = exp2 %>%
  group_by(short_trigger,content) %>%
  tally(sort=T)
summary(count$n)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#14      22      25      25      28      41

count = exp1 %>%
  group_by(short_trigger,contentFact) %>%
  tally(sort=T)
summary(count$n)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#4.00   10.00   13.00   12.62   15.00   25.00 

count = exp2 %>%
  group_by(short_trigger,contentFact) %>%
  tally(sort=T)
summary(count$n)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#4.0    10.0    12.0    12.5    15.0    27.0

# plots 

# exp1 vs exp2 by predicate
ggplot(pred.means, aes(x=Mean_certainty_Pred_Exp1, y=Mean_certainty_Pred_Exp2)) +
  geom_abline(intercept=0, slope=1, linetype="dashed", color = "gray", size=1) +
  geom_point(shape=16, size=2, alpha=1) +
  xlab("Certainty rating Exp. 1") +
  ylab("Certainty rating Exp. 2") +
  scale_x_continuous(expand = expansion(mult = c(0, 0)), breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0)), breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  theme(panel.spacing.x = unit(4, "mm")) +
  coord_fixed(ratio = 1) +
  ggtitle("Spearman's rho = .98")
ggsave(f="../graphs/SUP-certainty-Exp1-by-Exp2.pdf",height=4,width=4)

# exp1 vs exp2 by predicate/item
ggplot(predItem.means, aes(x=Mean_certainty_PredItem_Exp1, y=Mean_certainty_PredItem_Exp2)) +
  geom_abline(intercept=0, slope=1, linetype="dashed", color = "gray", size=1) +
  geom_point(shape=16, size=2, alpha=1) +
  xlab("Certainty rating Exp. 1") +
  ylab("Certainty rating Exp. 2") +
  scale_x_continuous(expand = expansion(mult = c(0, 0)), breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0)), breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  theme(panel.spacing.x = unit(4, "mm")) +
  coord_fixed(ratio = 1) +
  ggtitle("Spearman's rho = .9")
ggsave(f="../graphs/SUP-certainty-PredItem-Exp1-by-Exp2.pdf",height=4,width=4)

# exp1 vs exp2 by predicate/item/fact
ggplot(predItemFact.means, aes(x=Mean_certainty_PredItemFact_Exp1, y=Mean_certainty_PredItemFact_Exp2)) +
  geom_abline(intercept=0, slope=1, linetype="dashed", color = "gray", size=1) +
  geom_point(shape=16, size=2, alpha=1) +
  xlab("Certainty rating Exp. 1") +
  ylab("Certainty rating Exp. 2") +
  scale_x_continuous(expand = expansion(mult = c(0, 0)), breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0)), breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  theme(panel.spacing.x = unit(4, "mm")) +
  coord_fixed(ratio = 1) +
  ggtitle("Spearman's rho = .85")
ggsave(f="../graphs/SUP-certainty-PredItemFact-Exp1-by-Exp2.pdf",height=4,width=4)

#### at-issueness (Exp 1 vs Exp 2) ----

# load the data
exp1 <- read_csv("../data/d.csv")
nrow(exp1) #10100

exp2 <- read_csv("../../exp2/data/d.csv")
nrow(exp2) #10000

# calculate ai means: by-predicate, by-item/fact by-predicate/item, by-predicate/item/fact

# by-predicate
exp1.Pred.means <- exp1 %>%
  group_by(short_trigger) %>%
  summarize(Mean_ai_Pred_Exp1 = mean(ai), CILow=ci.low(ai),CIHigh=ci.high(ai)) %>%
  ungroup() %>%
  mutate(YMin_ai_Pred_Exp1=Mean_ai_Pred_Exp1-CILow,YMax_ai_Pred_Exp1=Mean_ai_Pred_Exp1+CIHigh) %>%
  select(!c(CILow,CIHigh))
summary(exp1.Pred.means)
nrow(exp1.Pred.means) #20

exp2.Pred.means <- exp2 %>%
  group_by(short_trigger) %>%
  summarize(Mean_ai_Pred_Exp2 = mean(ai), CILow=ci.low(ai),CIHigh=ci.high(ai)) %>%
  ungroup() %>%
  mutate(YMin_ai_Pred_Exp2=Mean_ai_Pred_Exp2-CILow,YMax_ai_Pred_Exp2=Mean_ai_Pred_Exp2+CIHigh) %>%
  select(!c(CILow,CIHigh))
summary(exp2.Pred.means)
nrow(exp2.Pred.means) #20

# by-contentFact
exp1.ItemFact.means <- exp1 %>%
  group_by(contentFact) %>%
  summarize(Mean_ai_ItemFact_Exp1 = mean(ai), CILow=ci.low(ai),CIHigh=ci.high(ai)) %>%
  ungroup() %>%
  mutate(YMin_ai_ItemFact_Exp1=Mean_ai_ItemFact_Exp1-CILow,YMax_ai_ItemFact_Exp1=Mean_ai_ItemFact_Exp1+CIHigh) %>%
  select(!c(CILow,CIHigh))
summary(exp1.ItemFact.means)
nrow(exp1.ItemFact.means) #40

exp2.ItemFact.means <- exp2 %>%
  group_by(contentFact) %>%
  summarize(Mean_ai_ItemFact_Exp2 = mean(ai), CILow=ci.low(ai),CIHigh=ci.high(ai)) %>%
  ungroup() %>%
  mutate(YMin_ai_ItemFact_Exp2=Mean_ai_ItemFact_Exp2-CILow,YMax_ai_ItemFact_Exp2=Mean_ai_ItemFact_Exp2+CIHigh) %>%
  select(!c(CILow,CIHigh))
summary(exp2.ItemFact.means)
nrow(exp2.ItemFact.means) #40

# by-predicate/item
exp1.PredItem.means <- exp1 %>%
  group_by(short_trigger,content) %>%
  summarize(Mean_ai_PredItem_Exp1 = mean(ai), CILow=ci.low(ai),CIHigh=ci.high(ai)) %>%
  ungroup() %>%
  mutate(YMin_ai_PredItem_Exp1=Mean_ai_PredItem_Exp1-CILow,YMax_ai_PredItem_Exp1=Mean_ai_PredItem_Exp1+CIHigh) %>%
  select(!c(CILow,CIHigh))
summary(exp1.PredItem.means)
nrow(exp1.PredItem.means) #400

exp2.PredItem.means <- exp2 %>%
  group_by(short_trigger,content) %>%
  summarize(Mean_ai_PredItem_Exp2 = mean(ai), CILow=ci.low(ai),CIHigh=ci.high(ai)) %>%
  ungroup() %>%
  mutate(YMin_ai_PredItem_Exp2=Mean_ai_PredItem_Exp2-CILow,YMax_ai_PredItem_Exp2=Mean_ai_PredItem_Exp2+CIHigh) %>%
  select(!c(CILow,CIHigh))
summary(exp2.PredItem.means)
nrow(exp2.PredItem.means) #400

# by-predicate/item/fact
exp1.PredItemFact.means <- exp1 %>%
  group_by(short_trigger,contentFact) %>%
  summarize(Mean_ai_PredItemFact_Exp1 = mean(ai), CILow=ci.low(ai),CIHigh=ci.high(ai)) %>%
  ungroup() %>%
  mutate(YMin_ai_PredItemFact_Exp1=Mean_ai_PredItemFact_Exp1-CILow,YMax_ai_PredItemFact_Exp1=Mean_ai_PredItemFact_Exp1+CIHigh) %>%
  select(!c(CILow,CIHigh))
summary(exp1.PredItemFact.means)
nrow(exp1.PredItemFact.means) #800

exp2.PredItemFact.means <- exp2 %>%
  group_by(short_trigger,contentFact) %>%
  summarize(Mean_ai_PredItemFact_Exp2 = mean(ai), CILow=ci.low(ai),CIHigh=ci.high(ai)) %>%
  ungroup() %>%
  mutate(YMin_ai_PredItemFact_Exp2=Mean_ai_PredItemFact_Exp2-CILow,YMax_ai_PredItemFact_Exp2=Mean_ai_PredItemFact_Exp2+CIHigh) %>%
  select(!c(CILow,CIHigh))
summary(exp2.PredItemFact.means)
nrow(exp2.PredItemFact.means) #800

# combine the data
pred.means <- left_join(exp1.Pred.means, exp2.Pred.means, by = c("short_trigger"))
nrow(pred.means) #20
summary(pred.means)

itemFact.means <- left_join(exp1.ItemFact.means, exp2.ItemFact.means, by = c("contentFact"))
nrow(itemFact.means) #40
summary(itemFact.means)

predItem.means <- left_join(exp1.PredItem.means, exp2.PredItem.means, by = c("short_trigger","content"))
nrow(predItem.means) #400
summary(predItem.means)

predItemFact.means <- left_join(exp1.PredItemFact.means, exp2.PredItemFact.means, by = c("short_trigger","contentFact"))
nrow(predItemFact.means) #400
summary(predItemFact.means)

# Spearman rank correlations

cor.test(pred.means$Mean_ai_Pred_Exp1, pred.means$Mean_ai_Pred_Exp2, method=c("spearman"))
# .99
cor.test(itemFact.means$Mean_ai_ItemFact_Exp1, itemFact.means$Mean_ai_ItemFact_Exp2, method=c("spearman"))
# .19
cor.test(predItem.means$Mean_ai_PredItem_Exp1, predItem.means$Mean_ai_PredItem_Exp2, method=c("spearman"), exact=FALSE)
# .84
cor.test(predItemFact.means$Mean_ai_PredItemFact_Exp1, predItemFact.means$Mean_ai_PredItemFact_Exp2, method=c("spearman"), exact=FALSE)
# .72

# count (as above with projection)

# plots 

# exp1 vs exp2 by predicate
ggplot(pred.means, aes(x=Mean_ai_Pred_Exp1, y=Mean_ai_Pred_Exp2)) +
  geom_abline(intercept=0, slope=1, linetype="dashed", color = "gray", size=1) +
  geom_point(shape=16, size=2, alpha=1) +
  xlab("Asking-whether rating Exp. 1") +
  ylab("Asking-whether rating Exp. 2") +
  scale_x_continuous(expand = expansion(mult = c(0, 0)), breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0)), breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  theme(panel.spacing.x = unit(4, "mm")) +
  coord_fixed(ratio = 1) +
  ggtitle("Spearman's rho = .99")
ggsave(f="../graphs/SUP-ai-Exp1-by-Exp2.pdf",height=4,width=4)

# exp1 vs exp2 by itemFact
ggplot(itemFact.means, aes(x=Mean_ai_ItemFact_Exp1, y=Mean_ai_ItemFact_Exp2)) +
  geom_abline(intercept=0, slope=1, linetype="dashed", color = "gray", size=1) +
  geom_point(shape=16, size=2, alpha=1) +
  xlab("Asking-whether rating Exp. 1") +
  ylab("Asking-whether rating Exp. 2") +
  scale_x_continuous(expand = expansion(mult = c(0, 0)), breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0)), breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  theme(panel.spacing.x = unit(4, "mm")) +
  coord_fixed(ratio = 1) +
  ggtitle("Spearman's rho = .19")
ggsave(f="../graphs/SUP-ai-ItemFact-Exp1-by-Exp2.pdf",height=4,width=4)

# exp1 vs exp2 by predicate/item
ggplot(predItem.means, aes(x=Mean_ai_PredItem_Exp1, y=Mean_ai_PredItem_Exp2)) +
  geom_abline(intercept=0, slope=1, linetype="dashed", color = "gray", size=1) +
  geom_point(shape=16, size=2, alpha=1) +
  xlab("Asking-whether rating Exp. 1") +
  ylab("Asking-whether rating Exp. 2") +
  scale_x_continuous(expand = expansion(mult = c(0, 0)), breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0)), breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  theme(panel.spacing.x = unit(4, "mm")) +
  coord_fixed(ratio = 1) +
  ggtitle("Spearman's rho = .84")
ggsave(f="../graphs/SUP-ai-PredItem-Exp1-by-Exp2.pdf",height=4,width=4)

# exp1 vs exp2 by predicate/item/fact
ggplot(predItemFact.means, aes(x=Mean_ai_PredItemFact_Exp1, y=Mean_ai_PredItemFact_Exp2)) +
  geom_abline(intercept=0, slope=1, linetype="dashed", color = "gray", size=1) +
  geom_point(shape=16, size=2, alpha=1) +
  xlab("Asking-whether rating Exp. 1") +
  ylab("Asking-whether rating Exp. 2") +
  scale_x_continuous(expand = expansion(mult = c(0, 0)), breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0)), breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  theme(panel.spacing.x = unit(4, "mm")) +
  coord_fixed(ratio = 1) +
  ggtitle("Spearman's rho = .72")
ggsave(f="../graphs/SUP-ai-PredItemFact-Exp1-by-Exp2.pdf",height=4,width=4)

## Supplement G: by-predicate correlations ----

# load the data
d <- read_csv("../data/d.csv")
nrow(d) #10100

mean.proj = d %>%
  group_by(short_trigger) %>%
  summarize(Mean.Proj = mean(projective), CILow = ci.low(projective), CIHigh = ci.high(projective)) %>%
  mutate(YMin.Proj = Mean.Proj - CILow, YMax.Proj = Mean.Proj + CIHigh, short_trigger = fct_reorder(as.factor(short_trigger),Mean.Proj)) %>%
  select(-c(CILow,CIHigh))
mean.proj

mean.ai = d %>%
  group_by(short_trigger) %>%
  summarize(Mean.AI = mean(ai), CILow = ci.low(ai), CIHigh = ci.high(ai)) %>%
  mutate(YMin.AI = Mean.AI - CILow, YMax.AI = Mean.AI + CIHigh, short_trigger = fct_reorder(as.factor(short_trigger),mean.proj$Mean.Proj)) %>%
  select(-c(CILow,CIHigh))
mean.ai

means = left_join(mean.proj, mean.ai, by = c("short_trigger"))
means

# to color-code the predicates
means = means %>%
  mutate(predicateType = case_when(short_trigger == "discover" ~ "factive",
                                   short_trigger == "know" ~ "factive",
                                   short_trigger == "be annoyed" ~ "factive",
                                   short_trigger == "reveal" ~ "factive",
                                   short_trigger == "see" ~ "factive",
                                   TRUE ~ "nonfactive"))
table(means$short_trigger,means$predicateType)

ggplot(means, aes(x=Mean.AI, y=Mean.Proj)) +
  geom_abline(intercept=0, slope=1, linetype="dashed", color = "black", size=.5) +
  geom_point(aes(color=predicateType), shape=20, size=5, alpha=1) +
  geom_errorbar(aes(ymin=YMin.Proj,ymax=YMax.Proj,color=predicateType),width=.01) +
  geom_errorbarh(aes(xmin=YMin.AI,xmax=YMax.AI, color=predicateType),height=.01) +
  geom_text_repel(aes(label = short_trigger, color=predicateType), hjust = 0.5,  vjust = -1) +
  scale_color_manual(values=c("#E69F00","#999999")) +
  scale_fill_manual(values=c("#E69F00","#999999")) +
  guides(color = "none", fill = "none") +
  xlab("Mean asking-whether rating") +
  ylab("Mean certainty rating") + 
  scale_x_continuous(expand = expansion(mult = c(0, 0)), breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0)), breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  theme(panel.spacing.x = unit(4, "mm")) +
  coord_fixed(ratio = 1) 
ggsave(f="../graphs/SUP-mean-projection-by-mean-ai.pdf",height=4,width=4)

## Supplement H: distribution of ratings ----
# (to justify fitting beta-models rather than ZOIB models)

# load the data
d <- read_csv("../data/d.csv")
nrow(d) #10100

names(d)
# prior, projective, ai

# order the predicates by projection 
# sort predicates by projection mean
proj.means = d %>%
  group_by(short_trigger) %>%
  summarize(Mean_proj = mean(projective)) %>%
  mutate(short_trigger = fct_rev(fct_reorder(as.factor(short_trigger),Mean_proj)))
proj.means

d = d %>%
  mutate(short_trigger = fct_relevel(short_trigger,levels(proj.means$short_trigger)))
levels(d$short_trigger)

# color-code the predicates
d = d %>%
  mutate(predicateType = case_when(short_trigger == "discover" ~ "factive",
                                   short_trigger == "know" ~ "factive",
                                   short_trigger == "be annoyed" ~ "factive",
                                   short_trigger == "reveal" ~ "factive",
                                   short_trigger == "see" ~ "factive",
                                   TRUE ~ "nonfactive"))
table(d$short_trigger,d$predicateType)

# plots

ggplot(d, aes(prior)) +
  geom_histogram(aes(color=predicateType),bins = 100) +
  facet_wrap(. ~ short_trigger,nrow = 5) +
  scale_color_manual(values=c("#E69F00","#999999")) +
  scale_fill_manual(values=c("#E69F00","#999999")) +
  guides(color = "none", fill = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0)), breaks=c(0,1),labels=c("0","1"), limits = c(-.05,1.05)) +
  theme(strip.background=element_rect(fill="white")) +
  xlab("Prior probability ratings")
ggsave(f="../graphs/SUP-rating-distributions-prior.pdf",height=5,width=5)

ggplot(d, aes(projective)) +
  geom_histogram(aes(color=predicateType),bins = 100) +
  facet_wrap(. ~ short_trigger,nrow = 5) +
  scale_color_manual(values=c("#E69F00","#999999")) +
  scale_fill_manual(values=c("#E69F00","#999999")) +
  guides(color = "none", fill = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0)), breaks=c(0,1),labels=c("0","1"), limits = c(-.05,1.05)) +
  theme(strip.background=element_rect(fill="white")) +
  xlab("Certain that ratings")
ggsave(f="../graphs/SUP-rating-distributions-projective.pdf",height=5,width=5)

ggplot(d, aes(ai)) +
  geom_histogram(aes(color=predicateType),bins = 100) +
  facet_wrap(. ~ short_trigger,nrow = 5) +
  scale_color_manual(values=c("#E69F00","#999999")) +
  scale_fill_manual(values=c("#E69F00","#999999")) +
  guides(color = "none", fill = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0)), breaks=c(0,1),labels=c("0","1"), limits = c(-.05,1.05)) +
  theme(strip.background=element_rect(fill="white")) +
  xlab("Asking-whether ratings")
ggsave(f="../graphs/SUP-rating-distributions-ai.pdf",height=5,width=5)





