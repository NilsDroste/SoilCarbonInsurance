# ---------------------------------
# Analysis of Soil Carbon Effects
# on yields in a changing Climate
# script author: nils@droste.io
# ---------------------------------

# load required libraries ----
library(tidyverse)
library(here)
library(lmerTest)
library(sjPlot)
library(cowplot)

# load data ----
df_crops <- read_csv(paste0(here(), "\\data\\SLU_2020_SwedishLTE_SoilFertilityExperiment_R3-9001_WheatBarley.csv")) %>% mutate(site = factor(site, levels=c("M-1", "M-2", "M-3", "M-4", "M-5", "M-6", "R-94", "R-95", "E-10", "E-9", "C-7", "C-8")), crop = crop %>% as.factor(), crop.rotation = crop.rotation %>% as.factor(), PK.treatment = PK.treatment %>% as.factor(), year = year %>% as.integer(), N.treatment = N.treatment %>% as.integer(), variety = variety %>% as.factor())

# crop regressions ----
## wheat
wheat.lme <- lmer(yield ~ crop.rotation + PK.treatment
                  + log(topsoil.C.tot.imp) * N.treatment
                  + log(topsoil.C.tot.imp) * I(N.treatment ^ 2)
                  + topsoil.pH.h2o
                  + log(topsoil.C.tot.imp) * gdd_meantemp_Q1
                  + log(topsoil.C.tot.imp) * gdd_meantemp_Q2
                  + log(topsoil.C.tot.imp) * gdd_meantemp_Q3
                  + log(topsoil.C.tot.imp) * gdd_meantemp_Q4
                  + log(topsoil.C.tot.imp) * gdd_prec_Q1
                  + log(topsoil.C.tot.imp) * gdd_prec_Q2
                  + log(topsoil.C.tot.imp) * gdd_prec_Q3
                  + log(topsoil.C.tot.imp) * gdd_prec_Q4
                  + gdd_meantemp_Q1 * gdd_prec_Q1
                  + gdd_meantemp_Q2 * gdd_prec_Q2
                  + gdd_meantemp_Q3 * gdd_prec_Q3
                  + gdd_meantemp_Q4 * gdd_prec_Q4
                  + variety + year
                  + log(topsoil.C.tot.imp.site_mean)
                  + (1 | site),
                  df_crops %>% filter(crop == "winter wheat"), REML = T)
summary(wheat.lme)

## barley
barley.lme <- lmer(yield ~ crop.rotation + PK.treatment 
                   + log(topsoil.C.tot.imp) * N.treatment 
                   + log(topsoil.C.tot.imp) * I(N.treatment^2) 
                   + topsoil.pH.h2o
                   + log(topsoil.C.tot.imp) * gdd_meantemp_Q1
                   + log(topsoil.C.tot.imp) * gdd_meantemp_Q2
                   + log(topsoil.C.tot.imp) * gdd_meantemp_Q3
                   + log(topsoil.C.tot.imp) * gdd_meantemp_Q4
                   + log(topsoil.C.tot.imp) * gdd_prec_Q1
                   + log(topsoil.C.tot.imp) * gdd_prec_Q2
                   + log(topsoil.C.tot.imp) * gdd_prec_Q3
                   + log(topsoil.C.tot.imp) * gdd_prec_Q4
                   + gdd_meantemp_Q1 * gdd_prec_Q1
                   + gdd_meantemp_Q2 * gdd_prec_Q2
                   + gdd_meantemp_Q3 * gdd_prec_Q3
                   + gdd_meantemp_Q4 * gdd_prec_Q4
                   + variety
                   + year
                   + log(topsoil.C.tot.imp.site_mean)
                   + (1|site), 
                   df_crops %>% filter(crop=="spring barley"), REML=T)
summary(barley.lme)
