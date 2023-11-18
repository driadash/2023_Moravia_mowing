Sys.setlocale("LC_ALL", "de_DE.UTF-8")
library(patchwork)
library(ggnewscale)
library(ggrepel)
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
                                         'nepravidelně koseno', 'nekoseno'))) |>
  mutate(grazing_tf = replace_na(grazing, F))

head <- read_xlsx(r'(C:/Users/krystof/OneDrive - MUNI/2023_Moravia-mowing/data/headers.xlsx)') |>
  mutate(expert_assessment = factor(expert_assessment,
                                    levels = c('abandoned', 'irregular', 'regular'),
                                    labels = c('nekoseno', 'nepravidelně\nkoseno', 'pravidelně\nkoseno'))) |>
  left_join(headau, by = 'plot_ID')
species_data <- read_xlsx(r'(C:/Users/krystof/OneDrive - MUNI/2023_Moravia-mowing/data/species_joined.xlsx)')
species <- read_xlsx(r'(C:/Users/krystof/OneDrive - MUNI/2023_Moravia-mowing/data/species.xlsx)') |>
  filter(area == 25) |>
  select(plot_ID, layer, taxon_original = species, cover) |>
  left_join(species_data |> select(taxon_original, taxon_ordination)) |>
  select(plot_ID, taxon_original, taxon_ordination, cover) |>
  group_by(plot_ID, taxon_ordination) |>
  summarise(cover = sum(cover)) |>
  ungroup()

species |>
  select(plot_ID, name = taxon_ordination, value = cover) |>
  drop_na() |>
  pivot_wider(values_fill = 0) |>
  column_to_rownames('plot_ID') -> spe

tibble(plot_ID = rownames(spe)) |>
  left_join(head) -> head


table(head$authorities, head$expert_assessment)

model <- capscale(sqrt(spe) ~ expert_assessment + Condition(site_ID) + Condition(habitat) + Condition(grazing_tf),
                  data = head, method = 'bray', sqrt.dist = T)
anova(model)
head[head$expert_assessment != 'nekoseno',]
model1 <- capscale(sqrt(spe[head$expert_assessment != 'nekoseno',]) ~ expert_assessment + Condition(site_ID) +
                    Condition(habitat) + Condition(grazing_tf),
                   data = head[head$expert_assessment != 'nekoseno',], method = 'bray', sqrt.dist = T)
anova(model1)

scores(model)$centroids |>
  as.data.frame() |>
  rownames_to_column('name') |>
  mutate(name = gsub('expert_assessment', '', name)) |>
  mutate_if(is.numeric, ~(.x * .2)) -> tb

scores(model, display = 'sp') |>
  as.data.frame() |>
  rownames_to_column('taxon_ordination') |>
  as_tibble() |>
  mutate(r2 = envfit(model, spe, permutations = 0)$vectors$r) |>
  left_join(species_data |>
              select(taxon_ordination, IUCN) |>
              drop_na()) |>
  mutate(taxon_ordination = paste0(word(taxon_ordination, 1) |> substr(1, 3),
                                   '.',
                                   word(taxon_ordination, 2) |> substr(1, 3))) |>
  mutate(IUCN = factor(IUCN, levels = c('CR', 'EN', 'VU', 'NT'))) |>
  slice_max(r2, n = 60) |>
  arrange(desc(IUCN)) |>
  ggplot(aes(CAP1, CAP2)) +
  geom_segment(data = tb, aes(x = 0, y = 0, xend = CAP1, yend = CAP2, colour = name),
               arrow = arrow(length = unit(.3, 'inches')), linewidth = 1.3, show.legend = F) +
  geom_text(data = tb, aes(label = name, colour = name, x = CAP1 * 1.08, y = CAP2 * 1.08), show.legend = F, size = 5,
            fontface = 'bold') +
  scale_colour_manual(values = c('grey30', '#FF007F', '#44BBFF')) +
  ggnewscale::new_scale_colour() +
  geom_segment(aes(x = 0, y = 0, xend = CAP1, yend = CAP2, colour = IUCN),
               arrow = arrow(length = unit(.1, 'inches')), alpha = .4) +
  geom_text_repel(aes(colour = IUCN, label = taxon_ordination), fontface = 'bold.italic', max.overlaps = Inf,
                  segment.colour = NA) + #
  scale_colour_manual(values = c('red', 'blue', '#CE7E00', '#1B6F0E')) +
  theme_bw() +
  theme(legend.position = c(0, 0),
        legend.justification = c(0, 0),
        legend.background = element_blank()) -> p1


scores(model1)$centroids |>
  as.data.frame() |>
  rownames_to_column('name') |>
  mutate(name = gsub('expert_assessment', '', name)) |>
  mutate_if(is.numeric, ~(.x * .8)) -> tb

scores(model1, display = 'sp') |>
  as.data.frame() |>
  rownames_to_column('taxon_ordination') |>
  as_tibble() |>
  mutate(r2 = envfit(model1, spe[head$expert_assessment != 'nekoseno',], permutations = 0)$vectors$r) |>
  left_join(species_data |>
              select(taxon_ordination, IUCN) |>
              drop_na()) |>
  mutate(taxon_ordination = paste0(word(taxon_ordination, 1) |> substr(1, 3),
                                   '.',
                                   word(taxon_ordination, 2) |> substr(1, 3))) |>
  mutate(IUCN = factor(IUCN, levels = c('CR', 'EN', 'VU', 'NT'))) |>
  slice_max(r2, n = 60) |>
  arrange(desc(IUCN)) |>
  ggplot(aes(CAP1, MDS1)) +
  geom_segment(data = tb, aes(x = 0, y = 0, xend = CAP1, yend = MDS1, colour = name),
               arrow = arrow(length = unit(.3, 'inches')), linewidth = 1.3, show.legend = F) +
  geom_text(data = tb, aes(label = name, colour = name, x = CAP1 * 1.08, y = MDS1 * 1.08), show.legend = F, size = 5,
            fontface = 'bold') +
  scale_colour_manual(values = c('#FF007F', '#44BBFF')) +
  ggnewscale::new_scale_colour() +
  geom_segment(aes(x = 0, y = 0, xend = CAP1, yend = MDS1, colour = IUCN),
               arrow = arrow(length = unit(.1, 'inches')), alpha = .4) +
  geom_text_repel(aes(colour = IUCN, label = taxon_ordination), fontface = 'bold.italic', max.overlaps = Inf,
                  segment.colour = NA) + #
  scale_colour_manual(values = c('red', 'blue', '#CE7E00', '#1B6F0E'), drop = F) +
  theme_bw() +
  theme(legend.position = c(0, 0),
        legend.justification = c(0, 0),
        legend.background = element_blank()) -> p2
m <- p1 + p2

ggsave('outputs/ordination_3.png', m, height = 12, width = 20)
