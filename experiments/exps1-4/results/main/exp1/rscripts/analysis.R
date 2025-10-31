this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source('helpers.R')

# load required packages
library(tidyverse)
library(dplyr)
library(dichromat)
library(forcats)
library(ggrepel)
library(cowplot)
theme_set(theme_bw())

# load clean data for analysis ----

# load clean projectivity data for analysis
dd = read.csv("../../exp3/data/cd.csv")
ak = read.csv("../../exp2/data/cd.csv") 

# projection: binary vs continuous ----

# for projectivity data, plot proportions against mean slider ratings
dd_means = dd %>%
  group_by(expression) %>%
  summarize(Mean_dd = mean(response), CILow_dd = ci.low(response), CIHigh_dd = ci.high(response)) %>%
  mutate(YMinM = Mean_dd - CILow_dd, YMaxM = Mean_dd + CIHigh_dd, expression = fct_reorder(as.factor(expression), Mean_dd)) %>%
  select(-CILow_dd, -CIHigh_dd)

ak_means = ak %>%
  group_by(expression) %>%
  summarize(Mean_ak = mean(response), CILow_ak = ci.low(response), CIHigh_ak = ci.high(response)) %>%
  mutate(YMinP = Mean_ak - CILow_ak, YMaxP = Mean_ak + CIHigh_ak, expression = fct_reorder(as.factor(expression), Mean_ak)) %>%
  select(-CILow_ak, -CIHigh_ak)

ak_dd = ak_means %>%
  left_join(dd_means, by = "expression")

# Define a vector of shapes for 7 expressions (using valid shape values)
shapes <- c(21, 22, 23, 24, 25, 0, 1)
names(shapes) <- levels(ak_dd$expression)

# Define a vector of colors for 7 expressions
colors <- c("darkorchid", "black", "gray60", "tomato1", "dodgerblue", "forestgreen", "gold")
names(colors) <- levels(ak_dd$expression)

# shape-predicate mapping
# 21: mc
# 22: non-veridical non-factive NF
# 23: factive F
# 24: optionally factive V
# 25: veridical non-factive VNF
# 0, 1: Add appropriate labels if needed

pp <- ggplot(ak_dd, aes(x = Mean_ak, y = Mean_dd, fill = expression, shape = expression)) +
  geom_errorbar(aes(ymin = YMinM, ymax = YMaxM), width = 0) +
  geom_errorbarh(aes(xmin = YMinP, xmax = YMaxP), width = 0) +
  geom_point(stroke = .5, size = 2.5, color = "black") +
  geom_abline(intercept = 0, slope = 1, color = "gray70", linetype = "dashed") +
  ylab("Mean 'Direct Dissent' Ratings") +
  xlab("Mean 'Asking Whether' Ratings") +
  theme(legend.position = "bottom") +
  coord_fixed(ratio = 1) +
  xlim(c(0, 1)) +
  ylim(c(0, 1)) +
  scale_shape_manual(values = shapes) +
  scale_fill_manual(values = colors)

pp
ggsave("../graphs/AKvsDD.pdf", height = 5, width = 5)




