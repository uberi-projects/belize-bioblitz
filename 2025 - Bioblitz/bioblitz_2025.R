# bioblitz_2025.R

## Attach Packages ---------------------------------
library(tidyverse)
library(iNEXT)
library(readxl)

## Define Palette ---------------------------------
palette <- c(
  "#E69F00", "#56B4E9", "#9933FF", "#FB9A99", "#009E73", "#F0E442", "#9999FF", "#33A02C",
  "#990066", "#999933", "#FFCCFF", "#336600", "#999999", "#990000"
)

## Load Data ---------------------------------
df <- read.csv("2025 - Bioblitz/Data_Observations_2025.csv", na.strings = "")
df_ebird <- read_excel("2025 - Bioblitz/Data_eBird_2025.xlsx")

## Summarize Data ---------------------------------
df_summary <- df %>%
  group_by(iconic_taxon_name) %>%
  summarize(
    Observations = n(),
    Taxa = length(unique(scientific_name, na.rm = TRUE)),
    Species = n_distinct(
      taxon_species_name[quality_grade == "research"],
      na.rm = TRUE
    )
  ) %>%
  na.omit() %>%
  arrange(-Observations)
df_summary # Observations, taxa, and species by iconic taxon
colSums(select(df_summary, -iconic_taxon_name)) # Total observations, taxa, and species

## Create Rarefaction Curve ---------------------------------
make_abundance_df <- function(df, group_var) {
  df %>%
    filter(!is.na(taxon_species_name)) %>%
    group_by(across(all_of(group_var)), taxon_species_name) %>%
    summarize(count = n())
}
df_inat_clean_class <- make_abundance_df(df, "taxon_class_name")
df_inat_clean_phylum <- make_abundance_df(df, "taxon_phylum_name")
df_inat_clean_kingdom <- make_abundance_df(df, "taxon_kingdom_name")
abundance_list <- list(
  Birds = filter(df_inat_clean_class, taxon_class_name == "Aves")$count,
  Insects = filter(df_inat_clean_class, taxon_class_name == "Insecta")$count,
  Amphibians = filter(df_inat_clean_class, taxon_class_name == "Amphibia")$count,
  Plants = filter(df_inat_clean_kingdom, taxon_kingdom_name == "Plantae")$count,
  Fungi = filter(df_inat_clean_kingdom, taxon_kingdom_name == "Fungi")$count
)
inext_all <- iNEXT(abundance_list, q = 0, datatype = "abundance")
inext_all_gg <- fortify(inext_all, type = 1)
transition_points <- inext_all_gg %>%
  filter(Method == "Extrapolation") %>%
  group_by(Assemblage) %>%
  slice(1)
fig_rarefaction <- ggplot(inext_all_gg, aes(x = x, y = y, color = Assemblage)) +
  geom_line() +
  geom_ribbon(aes(ymin = y.lwr, ymax = y.upr, fill = Assemblage), alpha = 0.2) +
  geom_point(data = transition_points, aes(x = x, y = y), shape = 8, size = 5, color = "black") +
  labs(x = "Number of Individuals", y = "Species Richness") +
  theme_classic() +
  scale_color_manual(values = palette) +
  scale_fill_manual(values = palette)
ggsave("2025 - Bioblitz/fig_rarefaction.png", fig_rarefaction, height = 8, width = 12)

## Summarize eBird Data ---------------------------------
df_summary_ebird <- df_ebird %>%
  group_by(Species) %>%
  summarize(Count = sum(Count)) %>%
  arrange(-Count)
nrow(df_summary_ebird) # Number of species
sum(df_summary_ebird$Count) # Number of observations
head(df_summary_ebird, 6) # Common species
