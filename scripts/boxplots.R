Sys.setlocale("LC_ALL", "de_DE.UTF-8")
library(writexl)
library(ggforce)
library(ggpubr)
library(tidyverse)
library(vegan)
library(readxl)

#check nomenclature match - zero values in diagnostic and basal spe are suspecious
# check one plot with very low shannon and evenness


# list.files(r'(C:/Users/krystof/OneDrive - MUNI/2023_Moravia-mowing/data)')

#filter sites
sites_filtr <- read_xlsx(r'(data/headers.xlsx)') |>
  select (plot_ID, habitat) |> 
  filter(habitat %in% c("T34", "T33/T34", "T35/T34"), .keep_all = TRUE) |> 
  select(-habitat) |> 
  mutate (habitat_select = "yes")

head <- read_xlsx(r'(data/headers.xlsx)') |>
  mutate(expert_assessment = factor(expert_assessment,
                                    levels = c('abandoned', 'irregular', 'regular'),
                                    labels = c('abandoned', 'irregular\nmowing', 'regular\nmowing'))) |> 
  left_join(sites_filtr, by = "plot_ID") |> 
  filter(habitat_select == "yes", .keep_all = TRUE) |> 
  select(-habitat_select)

species.richness <- read_xlsx(r'(data/species.xlsx)') |>
  select(plot_ID, subplot_ID, species) |> 
  group_by(plot_ID, subplot_ID, species) |> 
  mutate(num = 1) |> 
  group_by(plot_ID, subplot_ID) |> 
  summarise(spe_rich = sum(num))

write_csv(species.richness, 'species_richness_moravia.csv')

species_data <- read_xlsx(r'(data/species_joined.xlsx)')
species <- read_xlsx(r'(data/species.xlsx)') |>
  left_join(sites_filtr, by = "plot_ID") |> 
  filter(habitat_select == "yes", .keep_all = TRUE) |> 
  select(-habitat_select) |> 
  filter(area == 25) |>
  select(plot_ID, layer, taxon_original = species, cover) |>
  left_join(species_data |> select(taxon_original, taxon_ordination)) |>
  select(plot_ID, taxon_original, taxon_ordination, cover) |>
  group_by(plot_ID, taxon_original, taxon_ordination) |>
  summarise(cover = sum(cover)) |>
  left_join(species_data |> select(-c(taxon, taxon_pladias, taxon_ordination))) |>
  left_join(head |> select(plot_ID, habitat)) |>
  ungroup() |>
  left_join(read_csv('vyznamne.csv'))

species |> write_csv('species_ready.csv')

species$expanzni
species |>
  mutate(expanzni = as.logical(replace_na(expanzni, 0)),
    endg = as.numeric(ifelse(IUCN %in% c('EN', 'NT', 'VU', 'CR'), 1, 0)),
         woody_cover = woody_plant * cover,
  expanzni = (expanzni + invasive)*cover) |>
  group_by(plot_ID) |>
  summarise_at(c('endg', 'woody_plant', 'woody_cover', 'bazalni', 'diagnosticke', 'specificke', 'expanzni'),
               sum, na.rm = T) -> stats1

species |>
  select(plot_ID, name = taxon_original, value = cover) |>
  pivot_wider(values_fill = 0) |>
  column_to_rownames('plot_ID') -> spe

tibble(plot_ID = rownames(spe),
       S = rowSums(spe != 0),
       div_shannon = diversity(spe, index = "shannon"),
       div_simpson = diversity(spe, index = "simpson"),
       evenness = div_shannon / log(S)) -> stats2


species |>
  group_by(plot_ID) |>
  summarise_at(c('Light', 'Moisture', 'Nutrients'), mean, na.rm = T) |>
  left_join(stats1) |>
  left_join(stats2) |>
  left_join(head |>
              select(plot_ID, cover_e1, cover_e0, cover_litter, height_mean_e1, expert_assessment)) |>
  relocate(expert_assessment) -> stats_all
stats_all$evenness

#filter(expert_assessment != 'nekoseno') 

lm(log(S) ~ poly(cover_e0, 1), data = stats_all)
stats_all |> ggplot(aes(S, cover_e0)) +
  geom_point() +
  geom_smooth(method = 'lm')

m_litter <- lm(S ~ cover_litter, data = stats_all)
stats_all |> ggplot(aes(diagnosticke, cover_litter)) +
  geom_point() +
  scale_y_log10() +
  geom_smooth(method = 'lm')

summary(m_litter)

diverzita <- tribble(~level, ~label,
                     "S", "Species number",
                     "endg", "Number of protected species (IUCN)",
                     "div_shannon", "Shannon diversity index",
                     'evenness', "Evenness",
                     'expanzni', 'Cover of invasive and expansive species (%)',
                     'diagnosticke', 'Number of diagnostic species',
                     'bazalni', 'Number of basal species',
                     'specificke', 'Number of specific species')

struktura <- tribble(~level, ~label,
                     'cover_e1', 'Cover of herb layer (%)',
                     'cover_litter', 'Cover of litter (%)',
                     'cover_e0', 'Cover of moss layer (%)',
                     'height_mean_e1', 'Mean height of herb layer (cm)',
                     "woody_plant", "Number of woody species",
                     "woody_cover", "Cover of woody species (%, log)",
                     "Light", "EIV Light",
                     "Nutrients", "EIV Nutrients")

stats_all |>
  pivot_longer(-c(expert_assessment, plot_ID)) |>
  mutate(name = factor(name, levels = diverzita$level, labels = diverzita$label)) |>
  drop_na() |>
  #left_join(head |> select(plot_ID, habitat)) |>
  ggplot(aes(expert_assessment, value)) +
  geom_boxplot(aes(fill = expert_assessment, colour = expert_assessment), show.legend = F, notch = T, outlier.color = NA) +
  scale_fill_manual(values = c('#5697ff', 'gold', '#0dd67f')) +
  scale_colour_manual(values = c('#2a5599', '#ff691d', '#0a8045')) +
  geom_jitter(aes(color = expert_assessment), width = .05, height = 0, size = 3, alpha = 0.4, stroke = 0) +
  stat_compare_means(aes(label = after_stat(p.signif)),
                     colour = '#db1fd7', size = 4, hjust = 1,
                     ref.group = "regular\nmowing") +
  facet_wrap(~name, scales = 'free', ncol = 2) +
  #facet_grid(habitat~name, scales = 'free') +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none") +
  theme(strip.background = element_blank(),
        strip.text = element_text(hjust = 0, face = 'bold', size = 14, color = "#363636"),
        axis.title = element_blank())

ggsave('outputs//boxplots_diversity_240705.png', height = 12, width = 10)

stats_all |>
  mutate(woody_cover = log1p(woody_cover)) |>
  pivot_longer(-c(expert_assessment, plot_ID)) |>
  mutate(name = factor(name, levels = struktura$level, labels = struktura$label)) |>
  drop_na() |>
  ggplot(aes(expert_assessment, value)) +
  geom_boxplot(aes(fill = expert_assessment, colour = expert_assessment), show.legend = F, notch = T, outlier.color = NA) +
  scale_fill_manual(values = c('#5697ff', 'gold', '#0dd67f')) +
  scale_colour_manual(values = c('#2a5599', '#ff691d', '#0a8045')) +
  geom_jitter(aes(color = expert_assessment), width = .05, height = 0, size = 3, alpha = 0.4, stroke = 0) +
  stat_compare_means(aes(label = after_stat(p.signif)),
                     colour = '#db1fd7', size = 4, hjust = 1,
                     ref.group = "regular\nmowing") +
  facet_wrap(~name, scales = 'free', ncol = 2) +
  #facet_grid(habitat~name, scales = 'free') +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none") +
  theme(strip.background = element_blank(),
        strip.text = element_text(hjust = 0, face = 'bold', size = 14, color = "#363636"),
        axis.title = element_blank())

?stat_compare_means

ggsave('outputs//boxplots_structure_240705.png', height = 10, width = 8)

#previous version aestetics
stats_all |>
  mutate(woody_cover = log1p(woody_cover)) |>
  pivot_longer(-c(expert_assessment, plot_ID)) |>
  mutate(name = factor(name, levels = struktura$level, labels = struktura$label)) |>
  drop_na() |>
  ggplot(aes(expert_assessment, value)) +
  geom_boxplot(aes(fill = expert_assessment), show.legend = F, notch = T, outlier.color = NA) +
  scale_fill_manual(values = c('#3879e0', 'gold', '#0dd67f')) +
  geom_jitter(width = .05, height = 0) +
  stat_compare_means(aes(label = after_stat(p.signif)),
                     colour = 'red', 
                     ref.group = "regular\nmowing") +
  facet_wrap(~name, scales = 'free', ncol = 2) +
  #  facet_grid(habitat~name, scales = 'free') +
  coord_flip() +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_text(hjust = 0, face = 'bold', size = 14),
        axis.title = element_blank())

ggsave('outputs//boxplots_structure_v1.png', height = 10, width = 8)

summary(lm(S ~ cover_litter * cover_e1, data = stats_all))

stats_all 

stats_all |>
  ggplot(aes(cover_litter, log1p(endg))) +
  geom_point() +
  geom_smooth(method = 'lm') +
  stat_cor()

stats_all |>
  mutate(expert_assessment = substr(expert_assessment, 1, 5)) |>
  group_by(expert_assessment) |>
  summarise_if(is.numeric, mean, na.rm = T) |> pivot_longer(-1) |>
  pivot_wider(names_from = expert_assessment) |>
  mutate(rat  = 1-(nepra/pravi)) |>
  mutate(rat_nekos  = 1-(nekos/pravi))