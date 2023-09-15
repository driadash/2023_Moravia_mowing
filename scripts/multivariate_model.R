library(ggrepel)
library(vegan)
library(tidyverse)

read_delim(r'(data/seceni-headers230915.tsv)') |>
  filter(area == 25) |>
  mutate(site_ID = str_sub(plot_ID, 1, 5)) |>
  group_by(site_ID) |>
  count() |>
  filter(n > 1) |>
  left_join(read_delim(r'(data/seceni-headers230915.tsv)') |>
              filter(area == 25) |>
              select(plot_ID, site_ID)) |>
  ungroup() -> more_plots

read_delim(r'(data/seceni-releves230915.tsv)') |>
  semi_join(more_plots) |>
  #filter(releve.n <= 37) |>
  filter(area == 25) |>
  select(plot_ID, species_original = species, cover) |>
  left_join(species_data) |>
  select(plot_ID, species_corrected, cover) |>
  group_by(plot_ID, species_corrected) |>
  summarise(cover = sum(cover)) |>
  ungroup() |>
  pivot_wider(names_from = 'species_corrected', values_from = 'cover', values_fill = 0) -> spe

spe[1] |>
  left_join(read_delim(r'(data/seceni-headers230915.tsv)') |>
              filter(area == 25)) |>
  mutate(site_ID = str_sub(plot_ID, 1, 5)) |>
  write_csv('data//seceni_headers_meta.csv')

env <- read_csv(r'(data/seceni_headers_meta.csv)') |>
  mutate(mowing2 = factor(paste0(mowing, mosaic),
                          levels = c('00', '11', '10'),
                          labels = c('abandoned', 'mosaic_mowing', 'mowing')))

mod1 <- capscale(sqrt(spe[-1]) ~ mowing2 + Condition(site_ID), method = 'bray', sqrt.dist = T, data = env)
anova(mod1)

spe_bin <- spe[-1] != 0
spe_for_ordi <- spe[, c(0, colSums(spe_bin)) > 5]

envfit_m <- envfit(mod1, spe_for_ordi)

rownames_to_column(as.data.frame(scores(envfit_m, 'vectors')), 'name') |>
  bind_cols(r = envfit_m$vectors$r) |>
  arrange(-r) |>
  #slice(1:150) |>
  as_tibble() |>
  mutate(abb = paste0(substr(word(name, 1), 1, 3), '.',
                      substr(word(name, 2), 1, 3))) |>
  left_join(species_data |> distinct(name = species_corrected, IUCN)) -> spe_scores

biplot <- rownames_to_column(as.data.frame(mod1$CCA$centroids), 'name')

env_with_scores <- bind_cols(env, as_tibble(scores(mod1)$sites))

#' ===========================================================================
#' plotting
#' ===========================================================================

env_with_scores |>
  ggplot(aes(CAP1, CAP2)) +
  geom_point(aes(fill = management_corrected...17), shape = 21, size = 3) -> plot1

biplot |>
  mutate(across(c('CAP1', 'CAP2'), function(x){x*3}),
         name = gsub('mowing2', '', name)) |>
  ggplot(aes(CAP1, CAP2)) +
  geom_segment(aes(xend = CAP1, yend = CAP2, x = 0, y = 0), arrow = arrow()) +
  geom_text(aes(x = CAP1 * 1.2, y = CAP2 *1.2, label = name)) +
  geom_point(data = spe_scores, aes(colour = IUCN), shape = 3, stroke = 1.2) +
  geom_text_repel(data = spe_scores |>
    filter(!is.na(IUCN) | r > .2), aes(label = abb, colour = IUCN), fontface = 'bold.italic', max.overlaps = Inf) +
  theme_bw() +
  theme(legend.position = c(1,0),
        legend.justification = c(1,0),
  legend.background = element_blank())

ggsave('mowing_mosaic-ireg_abandoned.png',
       height = 8, width = 9.5)