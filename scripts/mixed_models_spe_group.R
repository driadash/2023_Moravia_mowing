
library(lme4)
library(tidyverse)
library(emmeans)


spe_num <- read_csv('spe_group_num.csv')
spe_cov <- read_csv('spe_group_cov.csv')

spe_num$management <- factor(spe_num$management, levels = c("regular_mowing", "abandoned", "irregular_mowing"))
spe_cov$management <- factor(spe_cov$management, levels = c("regular_mowing", "abandoned", "irregular_mowing"))

# Model 1: Abandonment vs. Regular Mowing using species NUMBER
model1_num <- lmer(species_number ~ management * species_group + (1 | site_ID), 
                   data = spe_num, 
                   subset = management %in% c("abandoned", "regular_mowing"))

summary(model1_num)

# Model 2: Irregular Mowing vs. Regular Mowing using species NUMBER
model2_num <- lmer(species_number ~ management * species_group + (1 | site_ID), 
                   data = spe_num, 
                   subset = management %in% c("irregular_mowing", "regular_mowing"))

summary(model2_num)

# Full model using species NUMBER
full_model_num <- lmer(species_number ~ management * species_group + (1 | site_ID), data = spe_num)
summary(full_model_num)


# emmeans - Pairwise comparisons for management types and species groups (using cover)

emmeans(full_model_num, pairwise ~ management | species_group)
emmeans(model1_num, pairwise ~ management | species_group)
emmeans(model2_num, pairwise ~ management | species_group)


# SAME but using cover of species groups

# Model 1: Abandonment vs. Regular Mowing using species COVER
model1_cov <- lmer(species_cover ~ management * species_group + (1 | site_ID), 
               data = spe_cov, 
               subset = management %in% c("abandoned", "regular_mowing"))

summary(model1_cov)

# Model 2: Irregular Mowing vs. Regular Mowing using species cover
model2_cov <- lmer(species_cover ~ management * species_group + (1 | site_ID), 
               data = spe_cov, 
               subset = management %in% c("irregular_mowing", "regular_mowing"))

summary(model2_cov)

# Full model
full_model_cov <- lmer(species_cover ~ management * species_group + (1 | site_ID), data = spe_cov)
summary(full_model_cov)


# emmeans - Pairwise comparisons for management types and species groups (using cover)

emmeans(full_model_cov, pairwise ~ management | species_group)
emmeans(model1_cov, pairwise ~ management | species_group)
emmeans(model2_cov, pairwise ~ management | species_group)



