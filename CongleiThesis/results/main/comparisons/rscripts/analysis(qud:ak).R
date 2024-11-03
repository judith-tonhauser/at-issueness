#QUD VS. AK
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
ak = read.csv("../../exp2/data/cd.csv")
qud = read.csv("../../exp1/data/cd.csv") 

# projection: binary vs continuous ----

# for projectivity data, plot proportions against mean slider ratings
ak_means = ak %>%
  group_by(expression) %>%
  summarize(Mean_ak = mean(response), CILow_ak = ci.low(response), CIHigh_ak = ci.high(response)) %>%
  mutate(YMinM = Mean_ak - CILow_ak, YMaxM = Mean_ak + CIHigh_ak, expression = fct_reorder(as.factor(expression), Mean_ak)) %>%
  select(-CILow_ak, -CIHigh_ak)

qud_means = qud %>%
  group_by(expression) %>%
  summarize(Mean_qud = mean(response), CILow_qud = ci.low(response), CIHigh_qud = ci.high(response)) %>%
  mutate(YMinP = Mean_qud - CILow_qud, YMaxP = Mean_qud + CIHigh_qud, expression = fct_reorder(as.factor(expression), Mean_qud)) %>%
  select(-CILow_qud, -CIHigh_qud)

qud_ak = qud_means %>%
  left_join(ak_means, by = "expression")

# Define a vector of shapes for 7 expressions (using valid shape values)
shapes <- c(21, 22, 23, 24, 25, 0, 1)
names(shapes) <- levels(qud_ak$expression)

# Define a vector of colors for 7 expressions
colors <- c("darkorchid", "black", "gray60", "tomato1", "dodgerblue", "forestgreen", "gold")
names(colors) <- levels(qud_ak$expression)

# shape-predicate mapping
# 21: mc
# 22: non-veridical non-factive NF
# 23: factive F
# 24: optionally factive V
# 25: veridical non-factive VNF
# 0, 1: Add appropriate labels if needed

pp <- ggplot(qud_ak, aes(x = Mean_qud, y = Mean_ak, fill = expression, shape = expression)) +
  geom_errorbar(aes(ymin = YMinM, ymax = YMaxM), width = 0) +
  geom_errorbar(aes(xmin = YMinP, xmax = YMaxP), width = 0) +
  geom_point(stroke = .5, size = 2.5, color = "black") +
  geom_abline(intercept = 0, slope = 1, color = "gray70", linetype = "dashed") +
  geom_text_repel(aes(label = expression), size = 3, max.overlaps = 10) +
  ylab("Mean `asking whether' (at-issueness) ratings") +
  xlab("Mean QUD (at-issueness) ratings") +
  theme(legend.position = "none") +
  coord_fixed(ratio = 1) +
  scale_x_continuous(limits = c(0.0, 0.9), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0.0, 0.9), expand = c(0, 0)) +
  scale_shape_manual(values = shapes) +
  scale_fill_manual(values = colors)

pp
ggsave("../graphs/AKvsQUD.pdf", height = 5, width = 5)




