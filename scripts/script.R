# packages needed
library(dplyr)
library(tidyverse)
library(ds4ling)
library(here)
library(phonR)
library(splancs)
library(phonTools)
library(broom)
library(knitr)

# colors used
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442",
               "#0072B2", "#D55E00", "#CC79A7", "#E23000", "#A48032")
cbPalette10 <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442",
                 "#0072B2", "#D55E00", "#CC79A7", "#E23000", "#A48032")

# data loading
data <- read.csv(here("data", "fourvowels.csv"))
head(data)

# remapping tags for vowels
remapping <- c(a = "a", ae = "æ", i = "i", u = "u")
data$vowel <- remapping[as.character(data$vowels)]
levels(data$vowels) = c("a", "æ", "i", "u")
#levels(data$vowels)
levels(data$vowel) = c("a", "æ", "i", "u")
#levels(data$vowel)
tablevowel <- table(data$vowel)
tablelang <- as.data.frame(
  table(data$lang_ability, data$vowel))

#This code measures how large or small each participant’s vowel space is,
#which can reflect articulation precision, speech clarity, or vowel system structure.
poly.area <- with(
  data, vowelMeansPolygonArea(
    mean_f1, mean_f2, vowels, poly.order = c("i", "a", "ae", "u"), 
    group = participant)
  )
hull.area <- with(data, convexHullArea(
  mean_f1, mean_f2,
  group = participant)
  )
rbind(poly.area, hull.area)


## vowel plot 1: vowel altogether, with dots
vp1 <- data |>
  ggplot() +
  aes(x = mean_f2, y = mean_f1, color = vowel) +
  geom_point(aes(shape = vowels))+
  stat_ellipse() +
  theme_minimal() + 
  scale_color_manual(values = cbPalette[c(1:10, 1, 2)]) +
  scale_x_reverse() +
  scale_y_reverse() +
  scale_shape_manual(values = 1:12) +
  geom_text(data=data |>
              group_by(vowel) |>
              summarise_at(vars(mean_f1:mean_f2), mean, na.rm = TRUE),
            aes(label = vowel), size = 8)

## vowel plot 2: vowel altogether, without dots
vp2 <- data |>
  ggplot() +
  aes(x = mean_f2, y = mean_f1, color = vowel) +
  stat_ellipse() +
  theme_minimal() +
  scale_color_manual(values = c(cbPalette, cbPalette10)) +
  scale_x_reverse() +
  scale_y_reverse() +
  geom_text(data=data |>
              group_by(vowel) |>
              summarise_at(vars(mean_f1:mean_f2), mean, na.rm = TRUE),
            aes(label = vowel), size = 8)

## vowel plot 3: vowel by participants, without dots
vp3 <- data |>
  ggplot() +
  aes(x = mean_f2, y = mean_f1, color = vowel) +
  stat_ellipse() +
  theme_minimal() +
  scale_color_manual(values = c(cbPalette, cbPalette10)) +
  scale_x_reverse() +
  scale_y_reverse() +
  geom_text(data=data |>
              group_by(participant, vowel) |> 
              summarise_at(vars(mean_f1:mean_f2), mean, na.rm = TRUE),
            aes(label = vowel),
            size = 6,
            show.legend = FALSE) + 
  facet_wrap(~participant, ncol = 3) +
  labs(
    x = "F2 (Hz)",
    y = "F1 (Hz)"
  ) +
  theme(legend.position = "none")

#vowel plot 4: vowel by language ability
#par(mfrow = c(1, 1))
#vp4 <- with(data, plotVowels(
#  mean_f1, mean_f2, vowels, group = lang_ability,
#  plot.tokens = FALSE, plot.means = TRUE, 
#  pch.means = vowels, cex.means = 2, var.col.by = vowels,
#  var.sty.by = participant, ellipse.line = TRUE, 
#  ellipse.fill = TRUE, fill.opacity = 0.1, pretty = TRUE)
#  )

#filter data for vowel a
data_a <- data |>
  filter(vowel=="a")
#f1 model for vowel a
mf1a <- lm(mean_f1 ~ lang_ability + generation + gender, data=data_a)
sf1a <- summary(mf1a)
#f2 model for vowel a
mf2a <- lm(mean_f2 ~ lang_ability + generation + gender, data=data_a)
sf2a <- summary(mf2a)

#filter data for vowel ae
data_ae <- data |>
  filter(vowel=="æ")
#f1 model for vowel ae
mf1ae <- lm(mean_f1 ~ lang_ability + generation + gender, data=data_ae)
sf1ae <- summary(mf1ae)
#f2 model for vowel ae
mf2ae <- lm(mean_f2 ~ lang_ability + generation + gender, data=data_ae)
sf2ae <- summary(mf2ae)

#filter data for vowel i
data_i <- data |>
  filter(vowel=="i")
#f1 model for vowel i
mf1i <- lm(mean_f1 ~ lang_ability + generation + gender, data=data_i)
sf1i <- summary(mf1i)
#f2 model for vowel i
mf2i <- lm(mean_f2 ~ lang_ability + generation + gender, data=data_i)
sf2i <- summary(mf2i)

#filter data for vowel u
data_u <- data |>
  filter(vowel=="u")
#f1 model for vowel u
mf1u <- lm(mean_f1 ~ lang_ability + generation + gender, data=data_u)
sf1u <- summary(mf1u)
#f2 model for vowel u
mf2u <- lm(mean_f2 ~ lang_ability + generation + gender, data=data_u)
sf2u <- summary(mf2u)

# plot 1: vowel altogether
#p1 <- with(data, plotVowels(
#  mean_f1, mean_f2, var.sty.by = vowels,
#  var.col.by = vowels, pretty = TRUE))
# plot 2: vowel by language ability
#p2 <- with(data, plotVowels(
#  mean_f1, mean_f2, var.sty.by = vowels,
#  var.col.by = lang_ability, pretty = TRUE))

# vowel plot 1: vowel altogether
#vp1 <- with(data, plotVowels(
#  mean_f1, mean_f2, vowels, plot.tokens = TRUE,
#  pch.tokens = vowels, cex.tokens = 1.2, alpha.tokens = 0.2,
#  plot.means = TRUE, pch.means = vowels, cex.means = 2,
#  var.col.by = vowels, ellipse.line = TRUE, pretty = TRUE))

