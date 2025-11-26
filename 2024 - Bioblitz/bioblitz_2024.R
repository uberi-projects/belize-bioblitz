# bioblitz_2024.R

## Attach Packages ---------------------------------
library(tidyverse)
library(knitr)
library(ggpubr)
library(iNEXT)

## Define Palette ---------------------------------
palette <- c(
    "#E69F00", "#56B4E9", "#9933FF", "#FB9A99", "#009E73", "#F0E442", "#9999FF",
    "#33A02C", "#990066", "#999933", "#FFCCFF", "#336600", "#999999", "#990000"
)

## Define ggplot theme ------------------------
custom_theme <- theme(
    axis.title = element_text(size = 26),
    axis.text = element_text(size = 18),
    axis.ticks = element_line(color = "black", size = 1.2),
    axis.ticks.length = unit(0.4, "cm"),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 16),
    panel.grid.major.y = element_line(color = "black", linewidth = 0.5, linetype = "dashed"),
    panel.grid.minor.y = element_line(color = "grey85", linewidth = 0.3)
)

## Load Data ---------------------------------
df_survey <- read.csv("2024 - Bioblitz/Data_Survey_2024.csv", na = "")
df_inat <- read.csv("2024 - Bioblitz/Data_Observations_2024.csv", na = "")

## Clean Survey Responses Data ---------------------------------
clean_df_survey <- df_survey %>%
    mutate(
        "Photographs?" = Did.you.take.photographs.of.plants.and.or.animals.during.the.Bioblitz..,
        "Upload?" = Did.you.upload.your.observations.to.iNaturalist.,
        "Enjoyment?" = How.much.did.you.enjoy.the.event.,
        "Safety?" = How.safe.did.you.feel.during.the.event.,
        "Educational?" = How.educational.was.the.event.for.you.,
        "Participate Next Year?" = Based.on.your.experiences..how.likely.are.you.to.participate.in.a.2025.Bioblitz.next.year.
    )

## Summarize Survey Responses ---------------------------------
activity_counts <- df_survey %>%
    separate_rows(Which.Bioblitz.activities.did.you.participate.in., sep = ";") %>%
    mutate(Activities = factor(tolower(Which.Bioblitz.activities.did.you.participate.in.),
        levels = c("friday bioblitz", "night bioblitz", "bird bioblitz", "saturday bioblitz"),
        labels = c("Fri Bioblitz", "Night Bioblitz", "Bird Bioblitz", "Sat Bioblitz")
    )) %>%
    count(Activities) %>%
    mutate("count" = n)

## Visualize Survey Responses ---------------------------------
A <- ggplot(activity_counts, aes(x = Activities, y = count)) +
    geom_col(color = palette[1], fill = alpha(palette[1], 0.2)) +
    geom_text(aes(label = count), vjust = -0.5) +
    theme_pubclean() +
    custom_theme +
    ylim(0, 20)
B <- ggplot(clean_df_survey, aes(x = `Photographs?`)) +
    geom_bar(color = palette[2], fill = alpha(palette[2], 0.2)) +
    geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
    theme_pubclean() +
    ylim(0, 20) +
    custom_theme
C <- ggplot(clean_df_survey %>% filter(!is.na(`Upload?`)), aes(x = `Upload?`)) +
    geom_bar(color = palette[3], fill = alpha(palette[3], 0.2)) +
    geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
    theme_pubclean() +
    ylim(0, 20) +
    custom_theme
D <- ggplot(clean_df_survey %>% mutate(`Enjoyment?` = factor(`Enjoyment?`, levels = 1:5)), aes(x = `Enjoyment?`)) +
    geom_bar(color = palette[4], fill = alpha(palette[4], 0.2)) +
    geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
    scale_x_discrete(drop = FALSE) +
    theme_pubclean() +
    ylim(0, 20) +
    custom_theme
E <- ggplot(clean_df_survey %>% mutate(`Safety?` = factor(`Safety?`, levels = 1:5)), aes(x = `Safety?`)) +
    geom_bar(color = palette[5], fill = alpha(palette[5], 0.2)) +
    geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
    scale_x_discrete(drop = FALSE) +
    theme_pubclean() +
    ylim(0, 20) +
    custom_theme
F <- ggplot(clean_df_survey %>% mutate(`Educational?` = factor(`Educational?`, levels = 1:5)), aes(x = `Educational?`)) +
    geom_bar(color = palette[6], fill = alpha(palette[6], 0.2)) +
    geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
    scale_x_discrete(drop = FALSE) +
    theme_pubclean() +
    ylim(0, 20) +
    custom_theme
G <- ggplot(clean_df_survey %>% mutate(`Participate Next Year?` = factor(`Participate Next Year?`, levels = 1:5)), aes(x = `Participate Next Year?`)) +
    geom_bar(color = palette[7], fill = alpha(palette[7], 0.2)) +
    geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
    scale_x_discrete(drop = FALSE) +
    theme_pubclean() +
    ylim(0, 20) +
    custom_theme
BC <- ggarrange(B, C, ncol = 2, nrow = 1, labels = c("B", "C"))
DE <- ggarrange(D, E, ncol = 2, nrow = 1, labels = c("D", "E"))
FG <- ggarrange(F, G, ncol = 2, nrow = 1, labels = c("F", "G"))
fig_surveys <- ggarrange(A, BC, DE, FG, ncol = 1, nrow = 4, labels = c("A", "", "", ""))
ggsave("2024 - Bioblitz/fig_surveys.png", fig_surveys, height = 12, width = 10)
