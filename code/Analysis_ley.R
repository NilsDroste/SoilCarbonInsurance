# ---------------------------------
# Analysis of ley-livestock cropping
# sysestems on soil carbon
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

# regression ----
## Multilevel random intercept and trend model for crop rotation (i.e. ley-farming) on soil carbon

ley.lme <- lmer(log(topsoil.C.tot.imp)~ PK.treatment
                + N.treatment
                + I(N.treatment ^ 2)
                + topsoil.pH.h2o
                + gdd_meantemp_Q1
                + gdd_meantemp_Q2
                + gdd_meantemp_Q3
                + gdd_meantemp_Q4
                + gdd_prec_Q1
                + gdd_prec_Q2
                + gdd_prec_Q3
                + gdd_prec_Q4
                + gdd_meantemp_Q1 * gdd_prec_Q1
                + gdd_meantemp_Q2 * gdd_prec_Q2
                + gdd_meantemp_Q3 * gdd_prec_Q3
                + gdd_meantemp_Q4 * gdd_prec_Q4
                + year
                + crop.rotation 
                + crop.rotation:year
                + ( 1 + year| site),
                df_crops, REML = T
)
summary(ley.lme)

# this does not converge (troubleshooting below)

# extract random effects
(ley.lme.ranef <- ranef(ley.lme)) 


# troubleshooting non-convergence ----
# https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html

# try scaling

df_crops_subs_scaled <- df_crops_subs %>% mutate(
  topsoil.C.tot.imp_scale = scale(topsoil.C.tot.imp),
  N.treatment_scale = scale(N.treatment),
  topsoil.pH.h2o_scale = scale(topsoil.pH.h2o),
  gdd_meantemp_Q1_scale = scale(gdd_meantemp_Q1),
  gdd_meantemp_Q2_scale = scale(gdd_meantemp_Q2),
  gdd_meantemp_Q3_scale = scale(gdd_meantemp_Q3),
  gdd_meantemp_Q4_scale = scale(gdd_meantemp_Q4),
  gdd_prec_Q1_scale = scale(gdd_prec_Q1),
  gdd_prec_Q2_scale = scale(gdd_prec_Q2),
  gdd_prec_Q3_scale = scale(gdd_prec_Q3),
  gdd_prec_Q4_scale = scale(gdd_prec_Q4)
)


ley.lme_scale <- lmer(log(topsoil.C.tot.imp)~ PK.treatment
                      + N.treatment_scale
                      + I(N.treatment_scale ^ 2)
                      + topsoil.pH.h2o_scale
                      + gdd_meantemp_Q1_scale
                      + gdd_meantemp_Q2_scale
                      + gdd_meantemp_Q3_scale
                      + gdd_meantemp_Q4_scale
                      + gdd_prec_Q1_scale
                      + gdd_prec_Q2_scale
                      + gdd_prec_Q3_scale
                      + gdd_prec_Q4_scale
                      + gdd_meantemp_Q1_scale * gdd_prec_Q1_scale
                      + gdd_meantemp_Q2_scale * gdd_prec_Q2_scale
                      + gdd_meantemp_Q3_scale * gdd_prec_Q3_scale
                      + gdd_meantemp_Q4_scale * gdd_prec_Q4_scale
                      + year
                      + crop.rotation 
                      + crop.rotation:year
                      + ( 1 + year | site),
                      df_crops_subs_scaled, REML = T
) 
summary(ley.lme_scale)
# the issue remains

# Check singularity
tt <- getME(ley.lme,"theta")
ll <- getME(ley.lme,"lower")
min(tt[ll==0])
# Not a problem in this case.

#Double-checking gradient calculations
derivs1 <- ley.lme@optinfo$derivs
sc_grad1 <- with(derivs1,solve(Hessian,gradient))
max(abs(sc_grad1))
max(pmin(abs(sc_grad1),abs(derivs1$gradient)))
# far above tolerance

# trying to restart the optimization with first round estimates
ss <- getME(ley.lme,c("theta","fixef"))
m2 <- update(ley.lme,start=ss,control=lmerControl(optCtrl=list(maxfun=2e5)))
# does not solve the issue either

# try different optimizer
allFit(show.meth.tab=TRUE) 
diff_optims <- allFit(ley.lme, maxfun = 1e5, parallel = 'multicore', ncpus = parallel::detectCores())
is.OK <- sapply(diff_optims, is, "merMod")
diff_optims.OK <- diff_optims[is.OK]
lapply(diff_optims.OK,function(x) x@optinfo$conv$lme4$messages)
# not a single optimizer converges

(lliks <- sort(sapply(diff_optims.OK,logLik)))
# loglikelihoods are comparable


# compare estimates from various optimizers
diff_optims.fixef <- t(sapply(diff_optims.OK,fixef))
diff_optims.fixef.m <- reshape::melt(diff_optims.fixef)
models <- levels(diff_optims.fixef.m$Var1)
ylabs <- substr(models,1,3)
diff_optims.fixef.m <- transform(diff_optims.fixef.m,Var1=factor(X1,levels=names(lliks)))

#plot
(gplot1 <- ggplot(diff_optims.fixef.m,aes(x=value,y=Var1,colour=Var1))+geom_point()+
    facet_wrap(~X2,scale="free")+
    scale_colour_brewer(palette="Dark2")+
    scale_y_discrete(breaks=models,
                     labels=ylabs)+
    labs(x="",y=""))

# Coefficients of variation of fixed-effect parameter estimates:
summary(unlist(plyr::daply(diff_optims.fixef.m,"X2",summarise,sd(value)/abs(mean(value)))))

# There is some variation but it is at a range that is small enough to not overly be worried.
# Last test: compare to simpler models

ley.lme_smpl <- lmer(log(topsoil.C.tot.imp)~ 
                       + year
                     + crop.rotation 
                     + crop.rotation:year
                     + ( 1  | site),
                     df_crops_subs, REML = T
) 
summary(ley.lme_smpl)
# this converges

# predictions
ley.lme.preds_smpl <- ggeffects::ggpredict(ley.lme_smpl, terms = c("year [all]", "crop.rotation"))
ley.lme.preds.rand_smpl <- ggeffects::ggpredict(ley.lme_smpl, terms = c("year [all]", "crop.rotation", "site"), type = "re")

cowplot::plot_grid(plot(ley.lme.preds_smpl) + ggtitle("population level effects of crop rotation on topsoil carbon")  + labs(colour="crop rotation") + ylab("topsoil carbon"),
                   plot(ley.lme.preds.rand_smpl) + ggtitle("site specific effects of crop rotation on topsoil carbon") + labs(colour="crop rotation") + ylab("topsoil carbon"),
                   nrow=1, labels = c("a)","b)"))
# pattern seems to be about the same.

# compare to lm
ley.lm_smpl <- lm(log(topsoil.C.tot.imp)~ 
                    + year* crop.rotation*site
                  + PK.treatment
                  + N.treatment
                  + I(N.treatment ^ 2)
                  + topsoil.pH.h2o
                  + gdd_meantemp_Q1
                  + gdd_meantemp_Q2
                  + gdd_meantemp_Q3
                  + gdd_meantemp_Q4
                  + gdd_prec_Q1
                  + gdd_prec_Q2
                  + gdd_prec_Q3
                  + gdd_prec_Q4
                  + gdd_meantemp_Q1 * gdd_prec_Q1
                  + gdd_meantemp_Q2 * gdd_prec_Q2
                  + gdd_meantemp_Q3 * gdd_prec_Q3
                  + gdd_meantemp_Q4 * gdd_prec_Q4,
                  df_crops_subs
) 
summary(ley.lm_smpl)

# predictions
ley.lm.preds_smpl <- ggeffects::ggpredict(ley.lm_smpl, terms = c("year [all]", "crop.rotation"))
ley.lm.preds.rand_smpl <- ggeffects::ggpredict(ley.lm_smpl, terms = c("year [all]", "crop.rotation", "site"), type = "re")

cowplot::plot_grid(plot(ley.lm.preds_smpl) + ggtitle("population level effects of crop rotation on topsoil carbon")  + labs(colour="crop rotation") + ylab("topsoil carbon"),
                   plot(ley.lm.preds.rand_smpl) + ggtitle("site specific effects of crop rotation on topsoil carbon") + labs(colour="crop rotation") + ylab("topsoil carbon"),
                   nrow=1, labels = c("a)","b)"))
# except for the population level estimates, this is also fairly similar.

# compare with raw data
(soc.plot <- df_crops_subs %>% group_by(site) %>% ggplot(aes(year,topsoil.C.tot.imp)) + geom_point(color="gray51") + geom_point(aes(y=topsoil.C.tot), color = "gray31") +
    geom_smooth(method = lm, formula = y ~ x, aes(color=crop.rotation,fill=crop.rotation)) + facet_grid(~site) + theme(strip.text.x = element_text(size=14, color = "black"),strip.text.y = element_text(size=14, color = "black"), axis.text.x = element_text(angle = 45, hjust = 1.2, vjust = 1.2), axis.ticks.length.x=unit(.15, "cm"), plot.margin=unit(c(0,.8,0,.8),"cm"), legend.position = "none", axis.title.y = element_text(vjust=7)) + labs(y="soil carbon") 
)

# overall the pattern that there is an increase in only about three of the sites, and a slower decrease for crop.rotationII seems fairly robust.
