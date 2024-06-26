library(ggpubr)
library(tidyverse)
library(readxl)
library(vegan)


redlisted <- read_xlsx(r'(data/ohrozeni-ochrana.xlsx)') |>
  select(taxon_pladias = taxon, IUCN) |>
  filter(IUCN != '-')

read_csv(r'(data/species_list_joined.csv)') |>
  left_join(redlisted) -> species_data

species_data1 <- species_data |>
  select(-species_original) |>
  distinct(species_corrected, .keep_all = TRUE)

read_delim(r'(data/seceni-releves230823.tsv)') |>
  #filter(releve.n <= 37) |>
  filter(area == 25) |>
  select(plot_ID, species_original = species, cover) |>
  left_join(species_data) |>
  select(plot_ID, species_corrected, cover) |>
  group_by(plot_ID, species_corrected) |>
  summarise(cover = sum(cover)) |>
  left_join(species_data1) -> meta

meta |>
  select(plot_ID, species_corrected, IUCN, woody_plant, cover) |>
  mutate(endg = as.numeric(ifelse(IUCN %in% c('EN', 'NT', 'VU', 'CR'), 1, 0)),
         woody_cover = woody_plant * cover) |>
  group_by(plot_ID) |>
  summarise_at(c('endg', 'woody_plant', 'woody_cover'), sum, na.rm = T) -> meta1

# count species richness (S) for plots
read_delim(r'(data/seceni-releves230823.tsv)') |>
  filter(area == 25) |>
  select(plot_ID, species) |>
  distinct() |>
  group_by(plot_ID) |>
  summarise(S = n_distinct(species)) -> spe_rich

#prepare species matrix for calculations of diversity indices
read_delim(r'(data/seceni-releves230823.tsv)') |>
  filter(area == 25) |>
  select(releve.n, species, cover) |>
  group_by(releve.n, species) |>
  summarise(cover = sum(cover)) |>
  mutate(cover = as.numeric(cover)) |>
  pivot_wider(names_from = species, values_from = cover, values_fill = 0) |>
  as.data.frame() -> spe_matrix

# preparing table with diversity indices
releve.n <- spe_matrix$releve.n
rownames(spe_matrix) <- spe_matrix[, 1]
spe_matrix <- spe_matrix |> select(-releve.n)
div_shannon <- diversity(spe_matrix, index = "shannon")
div_simpson <- diversity(spe_matrix, index = "simpson")

plots_num <- read_delim(r'(data/seceni-headers230915.tsv)') |>
  filter(area == 25) |>
  select(plot_ID, releve.n) |>
  mutate(releve.n = as.numeric(releve.n))
spe_div <- tibble(releve.n, div_shannon, div_simpson) |>
  left_join(plots_num) |>
  select(-releve.n)

meta |>
  group_by(plot_ID) |>
  summarise_at(c('Light', 'Moisture', 'Nutrients'), mean, na.rm = T) |>
  left_join(meta1) |>
  left_join(spe_div) |>
  left_join(spe_rich) -> stats_to_look

env <- read_delim(r'(data/seceni-headers230915.tsv)') |>
  mutate(mowing2 = factor(paste0(mowing, mosaic),
                          levels = c('00', '11', '10'),
                          labels = c('abandoned', 'mosaic_mowing', 'mowing')))

# plotting
# create new labels for plot facet
new_labels <- c("cover_e1" = "Cover herb", "cover_e0" = "Cover moss", "cover_litter" = "Cover litter", "S" = "Species richness", "div_shannon" = "Shannon index", "div_simpson" = "Simpson index", "woody_plant" = "Woody species (number)", "woody_cover" = "Woody species (cover)", "endg" = "Rare species (IUCN)", "Light" = "Light (EIV)", "Moisture" = "Moisture (EIV)", "Nutrients" = "Nutrients (EIV)")

stats_to_look |>
  left_join(read_delim(r'(data/seceni-headers230915.tsv)')  |>
              mutate(area = as.numeric(area)) |>
              filter(area == 25) |>
              select(plot_ID, cover_litter, cover_e1, cover_e0)) |>
  left_join(env |> select(plot_ID, mowing2)) |>
  pivot_longer(c(Light:cover_e0)) |>
  transform(name = factor(name, levels = c("cover_e1", "cover_e0", "cover_litter", "S", "div_shannon", "div_simpson",
                                           "woody_plant", "woody_cover", "endg", "Light", "Moisture", "Nutrients"))) ->
  data_to_plot

data_to_plot |>
  mutate(mowing2 = factor(mowing2, levels = c('abandoned',
                                              'mosaic_mowing',
                                              'mowing'),
                          labels = c('Abandonment',
                                     'Mosaic mowing',
                                     'Regular mowing'))) |>
  ggplot(aes(mowing2, value)) +
  geom_boxplot(aes(fill = mowing2), show.legend = F) +
  geom_point() +
  stat_compare_means(aes(label = after_stat(p.signif)),
                      method = 't.test',
                     ref.group = "Regular mowing") +
  facet_wrap(~name, scales = 'free_x',
             ncol = 3,
             labeller = labeller(name = new_labels)) +
  scale_fill_manual(values = c('#C8C1AB', '#fba53b', '#54AB3C')) +
  coord_flip() +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_text(hjust = 0, face = 'bold', size = 16),
        axis.title = element_blank(),
  text = element_text(size = 14))

env |>
  summarise(n(), .by = mowing2)

ggsave('boxplots.png', height = 9, width = 10)
