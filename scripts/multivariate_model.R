library(ggrepel)
library(vegan)
library(tidyverse)

read_delim(r'(data/seceni-releves230823.tsv)') |>
  filter(area == 25) |>
  select(plot_ID, species, cover) |>
  #distinct(species) |>
  #filter(grepl('Prunus', species)) |>
  #group_by(species) |>
  #count()
  mutate(species = ifelse(grepl('Rosa sp.', species),
                          'Rosa canina agg.', species),
         species = ifelse(grepl('Crataegus', species),
                          'Crataegus agg.', species),
         species = ifelse(grepl('Achillea', species),
                          'Achillea millefolium agg.', species))|>
  mutate(species = gsub(' cf. ', ' ', species)) |>
  filter(!grepl('sp.$', species)) |>
  group_by(species, plot_ID) |>
  summarise(cover = sum(cover)) |>
  pivot_wider(names_from = 'species', values_from = 'cover', values_fill = 0) -> spe

env <- spe[1] |>
  left_join(read_delim(r'(data/seceni-headers230915.tsv)') |>
  filter(area == 25)) |> write_csv('data//seceni_headers_meta.csv')

env <- read_csv(r'(data/seceni_headers_meta.csv)') |>
  mutate(mowing = mowing/max(mowing))

mod1 <- capscale(sqrt(spe[-1]) ~ mowing + Condition(lat), method = 'bray', sqrt.dist = T, data = env)
anova(mod1)

spe_bin <- spe[-1] != 0
spe_for_ordi <- spe[,c(0, colSums(spe_bin)) > 10]

envfit_m <- envfit(mod1, spe_for_ordi)

rownames_to_column(as.data.frame(scores(envfit_m, 'vectors')), 'name') |>
  bind_cols(r= envfit_m$vectors$r) |>
  arrange(-r) |>
  slice(1:60) |>
  as_tibble() |>
  mutate(abb = paste0(substr(word(name, 1),1,3), '.',
                substr(word(name,2), 1,3))) -> spe_scores

biplot <- rownames_to_column(as.data.frame(mod1$CCA$biplot), 'name')

env_with_scores <- bind_cols(env, as_tibble(scores(mod1)$sites))

#' ===========================================================================
#' plotting
#' ===========================================================================

env_with_scores |>
  ggplot(aes(CAP1, MDS1)) +
  geom_point(aes(fill = management_corrected...17), shape = 21, size = 3) -> plot1

biplot |>
  ggplot(aes(CAP1, MDS1)) +
  geom_segment(aes(xend = CAP1, yend = 0, x = 0, y = 0), arrow = arrow()) +
  geom_text(aes(label = name, y = 0)) +
  geom_point(data = spe_scores) +
  geom_text_repel(data = spe_scores, aes(label = abb, y = MDS1)), fontface = 'italic')

