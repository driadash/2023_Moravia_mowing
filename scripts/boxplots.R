Sys.setlocale("LC_ALL", "de_DE.UTF-8")
library(writexl)
library(ggforce)
library(ggpubr)
library(tidyverse)
library(vegan)
library(readxl)

list.files(r'(C:/Users/krystof/OneDrive - MUNI/2023_Moravia-mowing/data)')
headau <- read_xlsx(r'(C:/Users/krystof/OneDrive - MUNI/2023_Moravia-mowing/data/headers.xlsx)', 2) |>
  mutate(authorities = factor(paste(mowing_frequency, mowing_type),
                              levels = c('regular mowing',
                                         'regular mosaic',
                                         'irregular mowing',
                                         'irregular mosaic',
                                         'abandoned abandoned'),
                              labels = c('koseno',
                                         'mozaika',
                                         'nepravidelně koseno',
                                         'nepravidelně koseno', 'nekoseno')))

head <- read_xlsx(r'(C:/Users/krystof/OneDrive - MUNI/2023_Moravia-mowing/data/headers.xlsx)') |>
  mutate(expert_assessment = factor(expert_assessment,
                                    levels = c('abandoned', 'irregular', 'regular'),
                                    labels = c('nekoseno', 'nepravidelně\nkoseno', 'pravidelně\nkoseno')))
species_data <- read_xlsx(r'(C:/Users/krystof/OneDrive - MUNI/2023_Moravia-mowing/data/species_joined.xlsx)')
species <- read_xlsx(r'(C:/Users/krystof/OneDrive - MUNI/2023_Moravia-mowing/data/species.xlsx)') |>
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
  relocate(expert_assessment) |> filter(expert_assessment != 'nekoseno') -> stats_all
stats_all$evenness


lm(log(S) ~ poly(cover_e0, 1), data = stats_all)
stats_all |> ggplot(aes(S, cover_e0)) +
  geom_point() +
  geom_smooth(method = 'lm')

diverzita <- tribble(~level, ~label,
                     "S", "Počet druhů",
                     "endg", "Počet vzácných druhů (IUCN)",

                     "div_shannon", "Shannonův index diverzity",
                     'evenness', "Ekvitabilita",

                     'expanzni', 'Pokryvnost invazních a expanzních druhů (%)',
                     'diagnosticke', 'Počet diagnostických druhů',

                     'bazalni', 'Počet bazálních druhů',
                     'specificke', 'Počet specifických druhů')

struktura <- tribble(~level, ~label,
                     'cover_e1', 'Pokryvnost bylinného patra (%)',
                     'cover_litter', 'Pokryvnost stařiny (%)',

                     'cover_e0', 'Pokryvnost mechového patra (%)',
                     'height_mean_e1', 'Průměrná výška bylinného patra (cm)',

                     "woody_plant", "Počet druhů dřevin",
                     "woody_cover", "Pokryvnost dřevin (%, log)",

                     "Light", "EIH světlo",
                     "Nutrients", "EIH živiny")

stats_all |>
  pivot_longer(-c(expert_assessment, plot_ID)) |>
  mutate(name = factor(name, levels = diverzita$level, labels = diverzita$label)) |>
  drop_na() |>
  #left_join(head |> select(plot_ID, habitat)) |>
  ggplot(aes(expert_assessment, value)) +
  geom_boxplot(aes(fill = expert_assessment), show.legend = F, notch = T, outlier.color = NA) +
  scale_fill_manual(values = c('#3879e0', 'gold', '#0dd67f')) +
  geom_jitter(width = .05, height = 0) +
  stat_compare_means(aes(label = after_stat(p.signif)),
                     colour = 'red',
                     ref.group = "pravidelně\nkoseno") +
  facet_wrap(~name, scales = 'free', ncol = 2) +
  #facet_grid(habitat~name, scales = 'free') +
  coord_flip() +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_text(hjust = 0, face = 'bold', size = 16),
        axis.title = element_blank())

ggsave('outputs//boxplots_diverzita.png', height = 12, width = 10)

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
                     ref.group = "pravidelně\nkoseno") +
  facet_wrap(~name, scales = 'free', ncol = 2) +
  facet_grid(habitat~name, scales = 'free') +
  coord_flip() +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_text(hjust = 0, face = 'bold', size = 16),
        axis.title = element_blank())

ggsave('outputs//boxplots_struktura.png', height = 12, width = 10)


summary(lm(S ~ cover_litter * cover_e1, data = stats_all))

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