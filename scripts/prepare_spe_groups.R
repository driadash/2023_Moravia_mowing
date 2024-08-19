library(tidyverse)
library(readxl)

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

species |> 
  mutate(endg = as.numeric(ifelse(IUCN %in% c('EN', 'NT', 'VU', 'CR'), 1, 0)),
         woody = as.integer(woody_plant),
         charact = if_else(pmax(diagnosticke, specificke, bazalni, na.rm = TRUE) == 1, 1, 0),
         charact = replace_na(charact, 0),
         exp = as.integer(as.logical(native_expansive)),
         inv = as.integer(as.logical(invasive)),
         other = as.integer(!(endg == 1 | charact == 1 | exp == 1 | inv == 1 | woody == 1))) -> species2
  

species2 |>
  mutate(endg_cover = endg * cover,
         charact_cover = charact * cover,
         exp_cover = exp*cover,
         inv_cover = inv * cover,
         woody_cover = woody * cover,
         other_cover = other * cover) |>
  group_by(plot_ID) |>
  summarise_at(c('endg', 'endg_cover', 'charact', 'charact_cover', 'exp', 'exp_cover',  'inv', 'inv_cover', 'woody', 'woody_cover', 'other', 'other_cover'),
               sum, na.rm = T) -> stats_spe_groups

stats_spe_groups |> write_csv('stats_spe_groups.csv')

