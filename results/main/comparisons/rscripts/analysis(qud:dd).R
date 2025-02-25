#QUD VS. DD
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

dd = read.csv("../../exp3/data/cd.csv")
qud = read.csv("../../exp1/data/cd.csv") 

dd_means = dd %>%
  group_by(expression) %>%
  summarize(Mean_dd = mean(response), CILow_dd = ci.low(response), CIHigh_dd = ci.high(response)) %>%
  mutate(YMinM = Mean_dd - CILow_dd, YMaxM = Mean_dd + CIHigh_dd, expression = fct_reorder(as.factor(expression), Mean_dd)) %>%
  select(-CILow_dd, -CIHigh_dd)

qud_means = qud %>%
  group_by(expression) %>%
  summarize(Mean_qud = mean(response), CILow_qud = ci.low(response), CIHigh_qud = ci.high(response)) %>%
  mutate(YMinP = Mean_qud - CILow_qud, YMaxP = Mean_qud + CIHigh_qud, expression = fct_reorder(as.factor(expression), Mean_qud)) %>%
  select(-CILow_qud, -CIHigh_qud)

qud_dd = qud_means %>%
  left_join(dd_means, by = "expression")

# Define a vector of shapes for 7 expressions (using valid shape values)
shapes <- c(21, 22, 23, 24, 25, 0, 1)
names(shapes) <- levels(qud_dd$expression)

# Define a vector of colors for 7 expressions
colors <- c("darkorchid", "black", "gray60", "tomato1", "dodgerblue", "forestgreen", "gold")
names(colors) <- levels(qud_dd$expression)


pp <- ggplot(qud_dd, aes(x = Mean_qud, y = Mean_dd, fill = expression, shape = expression)) +
  geom_errorbar(aes(ymin = YMinM, ymax = YMaxM), width = 0) +
  geom_errorbarh(aes(xmin = YMinP, xmax = YMaxP), width = 0) +
  geom_point(stroke = .5, size = 2.5, color = "black") +
  geom_abline(intercept = 0, slope = 1, color = "gray70", linetype = "dashed") +
  geom_text_repel(aes(label = expression), size = 3, max.overlaps = 10) +
  ylab("Mean direct dissent (at-issueness) ratings") +
  xlab("Mean QUD (at-issueness) ratings") +
  theme(legend.position = "none") +
  coord_fixed(ratio = 1) +
  scale_x_continuous(limits = c(0.4, 0.9), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0.4, 0.9), expand = c(0, 0)) +
  scale_shape_manual(values = shapes) +
  scale_fill_manual(values = colors)

pp
ggsave("../graphs/DDvsQUD.pdf", height = 5, width = 5)




