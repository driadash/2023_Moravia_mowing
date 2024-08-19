Sys.setlocale("LC_ALL", "de_DE.UTF-8")
library(writexl)
library(patchwork)
library(ggnewscale)
library(ggrepel)
library(ggforce)
library(ggpubr)
library(tidyverse)
library(vegan)
library(readxl)

read_xlsx(r'(data/headers.xlsx)', 3) |>
  mutate(authorities_assessment = factor(authorities_assessment,
                                         levels = c('abandoned', 'irregular mosaic', 'irregular mowing', 'regular mosaic', 'regular mowing'),
                                         labels = c(
                                           'Abandoned', 
                                           'Mosaic or\nirregular\nmowing',
                                           'Mosaic or\nirregular\nmowing',
                                           'Mosaic or\nirregular\nmowing',
                                           'Regular\nmowing')),
         expert_assessment = factor(expert_assessment,
                                    levels = c('abandoned', 'irregular', 'regular'),
                                    labels = c('Abandoned', 'Irregular mowing', 'Regular mowing'))) |>
  mutate(grazing = replace_na(as.logical(grazing), F))|>
  select(plot_ID, authorities_assessment, expert_assessment, grazing) -> hedau

#filter sites
sites_filtr <- read_xlsx(r'(data/headers.xlsx)') |>
  select (plot_ID, habitat) |> 
  filter(habitat %in% c("T34", "T33/T34", "T35/T34"), .keep_all = TRUE) |> 
  select(-habitat) |> 
  mutate (habitat_select = "yes")

head <- read_xlsx(r'(data/headers.xlsx)') |>
  select(-expert_assessment, -grazing) |>
  left_join(hedau) |> 
  left_join(sites_filtr, by = "plot_ID") |> 
  filter(habitat_select == "yes", .keep_all = TRUE) |> 
  select(-habitat_select)

species_data <- read_xlsx(r'(data/species_joined.xlsx)')
species <- read_xlsx(r'(data/species.xlsx)') |>
  filter(area == 25) |>
  select(plot_ID, layer, taxon_original = species, cover) |>
  left_join(species_data |> select(taxon_original, taxon_ordination)) |>
  select(plot_ID, taxon_original, taxon_ordination, cover) |>
  left_join(sites_filtr, by = "plot_ID") |> 
  filter(habitat_select == "yes", .keep_all = TRUE) |> 
  select(-habitat_select) |> 
  group_by(plot_ID, taxon_ordination) |>
  summarise(cover = sum(cover)) |>
  ungroup()

species |> write_xlsx('outputs/tabulka_online.xlsx')

species |>
  select(plot_ID, name = taxon_ordination, value = cover) |>
  drop_na() |>
  pivot_wider(values_fill = 0) |>
  column_to_rownames('plot_ID') -> spe

tibble(plot_ID = rownames(spe)) |>
  left_join(head) -> head


model <- capscale(sqrt(spe) ~ expert_assessment +
  Condition(site_ID) +
  Condition(habitat) +
  Condition(grazing), data = head, method = 'bray', sqrt.dist = T)
anova(model)

model1 <- capscale(sqrt(spe[head$expert_assessment != 'Abandoned',]) ~ expert_assessment +
  Condition(site_ID) +
  Condition(habitat) +
  Condition(grazing), data = head[head$expert_assessment != 'Abandoned',], method = 'bray', sqrt.dist = T)
anova(model1)

#' this we do not show
model_discard <- capscale(sqrt(spe[!is.na(head$authorities_assessment),]) ~ authorities_assessment +
  Condition(site_ID) +
  Condition(habitat) +
  Condition(grazing), data = head[!is.na(head$authorities_assessment),], method = 'bray', sqrt.dist = T)
anova(model_discard)


scores(model)$centroids |>
  as.data.frame() |>
  rownames_to_column('name') |>
  mutate(name = gsub('expert_assessment', '', name)) |>
  mutate_if(is.numeric, ~(.x * .2)) -> tb1

scores(model, display = 'sp') |>
  as.data.frame() |>
  rownames_to_column('taxon_ordination') |>
  as_tibble() |>
  mutate(r2 = envfit(model, spe, permutations = 0)$vectors$r,
         count = colSums(spe!=0)) |>
  left_join(species_data |>
              distinct(taxon_ordination, IUCN) |>
              drop_na()) |>
  mutate(taxon_ordination = paste0(word(taxon_ordination, 1) |> substr(1, 3),
                                   '.',
                                   word(taxon_ordination, 2) |> substr(1, 3))) |>
  mutate(IUCN = factor(IUCN, levels = c('CR', 'EN', 'VU', 'NT', 'LC'))) |>
  filter(count >= 5) |>
  slice_max(r2, n = 80) |>
  arrange(desc(IUCN)) |>
  ggplot(aes(CAP1, CAP2)) +
  geom_segment(data = tb1, aes(x = 0, y = 0, xend = CAP1*1.2, yend = CAP2*1.2, colour = name),
               arrow = arrow(length = unit(.3, 'inches')), linewidth = 1.3, show.legend = F) +
  geom_text(data = tb1, aes(label = name, colour = name, x = (CAP1*1.2+sign(CAP1)*0.06), y = (CAP2*1.2+sign(CAP2)*0.06)), show.legend = F, size = 5,
            fontface = 'bold') +
  scale_colour_manual(values = c('grey30', '#FF007F', '#44BBFF')) +
  ggnewscale::new_scale_colour() +
  geom_segment(aes(x = 0, y = 0, xend = CAP1, yend = CAP2, colour = IUCN),
               arrow = arrow(length = unit(.1, 'inches')), alpha = .4, show.legend = F) +
  geom_text_repel(aes(colour = IUCN, label = taxon_ordination), fontface = 'bold.italic', max.overlaps = Inf,
                  show.legend = F,
                  segment.colour = NA) + #
  scale_x_continuous(expand = c(.1, .1)) +
  scale_y_continuous(expand = c(.1, .1)) +
  scale_colour_manual(values = c('red', 'blue', '#CE7E00', '#1B6F0E', '#f403fc'), drop = F) +
  theme_bw() +
  theme(legend.position = c(0, 0),
        legend.justification = c(0, 0),
        legend.background = element_blank()) -> p1

p1

ggsave('outputs/ordination_all_240705.png', p1, height = 10, width = 10)

scores(model1)$centroids |>
  as.data.frame() |>
  rownames_to_column('name') |>
  mutate(name = gsub('expert_assessment', '', name)) |>
  mutate_if(is.numeric, ~(.x * .8)) -> tb

scores(model1, display = 'sp') |>
  as.data.frame() |>
  rownames_to_column('taxon_ordination') |>
  as_tibble() |>
  mutate(r2 = envfit(model1, spe[head$expert_assessment != 'Abandoned',], permutations = 0)$vectors$r,
         count = colSums(spe[head$expert_assessment != 'Abandoned',]!=0)) |>
  left_join(species_data |>
              distinct(taxon_ordination, IUCN) |>
              drop_na()) |>
  mutate(taxon_ordination = paste0(word(taxon_ordination, 1) |> substr(1, 3),
                                   '.',
                                   word(taxon_ordination, 2) |> substr(1, 3))) |>
  mutate(IUCN = factor(IUCN, levels = c('CR', 'EN', 'VU', 'NT', 'LC'))) |>
  filter(count >= 5) |>
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
  scale_colour_manual(values = c('red', 'blue', '#CE7E00', '#1B6F0E', '#f403fc'), drop = F) +
  scale_x_continuous(expand = c(.1, .1)) +
  scale_y_continuous(expand = c(.1, .1)) +
  theme_bw() +
  theme(legend.position = c(0, 0),
        legend.justification = c(0, 0),
        legend.background = element_blank()) -> p2

m <- p1 / p2

ggsave('outputs/ordination_240705.png', m, height = 15, width = 10)
