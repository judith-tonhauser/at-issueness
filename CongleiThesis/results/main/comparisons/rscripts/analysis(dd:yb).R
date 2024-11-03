#YB VS. DD
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
yb = read.csv("../../exp4/data/cd.csv")
dd = read.csv("../../exp3/data/cd.csv") 

# projection: binary vs continuous ----

# for projectivity data, plot proportions against mean slider ratings
yb_means = yb %>%
  group_by(expression) %>%
  summarize(Mean_yb = mean(nResponse), CILow_yb = ci.low(nResponse), CIHigh_yb = ci.high(nResponse)) %>%
  mutate(YMinM = Mean_yb - CILow_yb, YMaxM = Mean_yb + CIHigh_yb, expression = fct_reorder(as.factor(expression), Mean_yb)) %>%
  select(-CILow_yb, -CIHigh_yb)

dd_means = dd %>%
  group_by(expression) %>%
  summarize(Mean_dd = mean(response), CILow_dd = ci.low(response), CIHigh_dd = ci.high(response)) %>%
  mutate(YMinP = Mean_dd - CILow_dd, YMaxP = Mean_dd + CIHigh_dd, expression = fct_reorder(as.factor(expression), Mean_dd)) %>%
  select(-CILow_dd, -CIHigh_dd)

dd_yb = dd_means %>%
  left_join(yb_means, by = "expression")

# Define a vector of shapes for 7 expressions (using valid shape values)
shapes <- c(21, 22, 23, 24, 25, 0, 1)
names(shapes) <- levels(dd_yb$expression)

# Define a vector of colors for 7 expressions
colors <- c("darkorchid", "black", "gray60", "tomato1", "dodgerblue", "forestgreen", "gold")
names(colors) <- levels(dd_yb$expression)

# shape-predicate mapping
# 21: mc
# 22: non-veridical non-factive NF
# 23: factive F
# 24: optionally factive V
# 25: veridical non-factive VNF
# 0, 1: Add appropriate labels if needed

pp <- ggplot(dd_yb, aes(x = Mean_dd, y = Mean_yb, fill = expression, shape = expression)) +
  geom_errorbar(aes(ymin = YMinM, ymax = YMaxM), width = 0) +
  geom_errorbar(aes(xmin = YMinP, xmax = YMaxP), width = 0) +
  geom_point(stroke = .5, size = 2.5, color = "black") +
  geom_abline(intercept = 0, slope = 1, color = "gray70", linetype = "dashed") +
  geom_text_repel(aes(label = expression), size = 3, max.overlaps = 10) +
  ylab("Proportion of 'no' (at-issueness) selections (`yes, but' test)") +
  xlab("Mean direct dissent (at-issueness) ratings") +
  theme(legend.position = "none") +
  coord_fixed(ratio = 1) +
  scale_x_continuous(limits = c(0, 1.05), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1.05), expand = c(0, 0)) +
  scale_shape_manual(values = shapes) +
  scale_fill_manual(values = colors)

pp
ggsave("../graphs/YBvsDD.pdf", height = 6, width = 6)




