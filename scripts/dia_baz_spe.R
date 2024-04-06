library(tidyverse)
library(readxl)

read_csv('species_ready.csv') |>
  select(plot_ID, taxon_ordination)

expanzni <- read_xlsx('data/source_expanzni-druhy-cz-biotopy-2023-09-27.xlsx') |>
  select(druh = species) |>
  mutate(expanzni = 1)

read_xlsx(r'(data/species_data_diag.xlsx)', 1) |>
  select(druh, habitat = biotop) |>
  mutate(name = 'bazalni') |>
  bind_rows(read_xlsx(r'(data/species_data_diag.xlsx)', 2) |>
              select(druh, habitat = biotop) |>
              mutate(name = 'specificke')) |>
  bind_rows(read_xlsx(r'(data/species_data_diag.xlsx)', 3) |>
              select(druh, habitat = biotop) |>
              mutate(name = 'diagnosticke')) |>
  mutate(value = 1) |>
  pivot_wider(values_fn = max) |>
  rename(taxon_ordination = druh) |>
  mutate(habitat = gsub('\\.', '', habitat)) -> z

z |>
  bind_rows(z |>
              filter(habitat %in% c('T33', 'T34')) |>
              select(taxon_ordination, bazalni, specificke, diagnosticke) |> 
              group_by(taxon_ordination)|>
              summarise_all(max) |>
              mutate(habitat = 'T33/T34')) |> 
  bind_rows(z |>
              filter(habitat %in% c('T35', 'T34')) |>
              select(taxon_ordination, bazalni, specificke, diagnosticke) |> 
              group_by(taxon_ordination)|>
              summarise_all(max) |>
              mutate(habitat = 'T35/T34')) -> z_step

unique(z_step$habitat)

z_step |>
full_join(
    crossing(taxon_ordination = expanzni$druh, habitat = unique(z$habitat)) |>
      mutate(expanzni = 1)) |>
  write_csv('vyznamne.csv')


