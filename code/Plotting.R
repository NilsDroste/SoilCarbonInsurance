library(sf)
library(ggplot2)
library(ggmap)
library(rnaturalearth)
library(dplyr)
library(grid)
library(gridGraphics)
library(gtable)
library(cowplot)
library(here)
library(tidyverse)

# Figure 1 ----

## Mapping of long-term environmental stations 

world <- ne_countries(scale = 50, type = "countries", returnclass = "sf") %>%
  select(iso_a3, iso_n3, admin)

temp <- tempfile()
temp2 <- tempfile()
download.file("http://gisco-services.ec.europa.eu/distribution/v2/nuts/download/ref-nuts-2016-01m.gdb.zip",temp)
unzip(zipfile = temp, exdir = temp2)
dsn <- paste0(temp2, "\\ref-nuts-2016-01m.gdb")
europe <- st_read(dsn, layer="NUTS_RG_01M_2016_3035")
unlink(c(temp,temp2))

europe <- st_read(dsn, layer="NUTS_RG_01M_2016_3035")
europe <- europe %>% st_simplify(dTolerance = 1000)
swe <- europe %>% dplyr::filter(ISO3_CODE=="SWE" & LEVL_CODE==1) %>% st_geometry()

ltes <- read.csv(paste0(here(), "\\data\\Coordinates.csv"), sep = ";", fileEncoding = "UTF-8")[,1:4] # from: http://www.ffe.slu.se/FF/Db01.cfm?DB00=P03-9001&DB04=2016&LG=SE 
ltes_sf <- st_as_sf(ltes, coords = c("long_coordinates","lat_coordinates"), crs = 4326)
ltes_sf <- st_transform(ltes_sf, crs=3035)

eumap <- world  %>%
  st_geometry() %>%
  st_transform(crs=3035) %>% 
  ggplot() +
  geom_sf(fill="gray95") +
  geom_sf(data=swe, fill="gray") +
  coord_sf(xlim = c(2750000, 5500000),
           ylim = c(1500000, 5250000)) +
  annotate(geom = "rect", xmin = 4325000, xmax= 5000000,
           ymin = 3500000, ymax = 5200000,
           colour="red", fill=NA) +
  theme_bw() +
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), plot.margin = margin(t = 0, r = 2, b = 2, l = -4, unit = "pt"))

swemap <- europe  %>%
  filter(LEVL_CODE == 0) %>%
  st_geometry() %>%
  ggplot() +
  geom_sf(fill="gray95") +
  geom_sf(data=swe, fill="lightgray") +
  geom_sf(data=ltes_sf, size = 4, shape = 10, col = "red") +
  annotate(geom = "text", x = c(4577500, 4775000, 4672500, 4535000), y = c(3625000, 4140000, 3950000, 3910000), label = c("M", "C", "E", "R"), 
           color = "black", size = 5, fontface = "bold") +
  coord_sf(xlim = c(4376814, 4972478),
           ylim = c(3586351, 5136005)) +
  theme_bw() +
  theme(axis.title = element_blank())

lte_map <- ggdraw() + draw_plot(swemap,0,0,1,1) + draw_plot(eumap, x=.62, y=-0.01, width=.3, height=.3)

### housekeeping
rm(list=c("dsn","eumap","europe", "ltes", "ltes_sf", "swe", "swemap", "world"))


## long term trends -----
### plotting info
sjPlot::set_theme(base=theme_light())

### yields

df_crops <- read_csv(paste0(here(), "\\data\\SLU_2020_SwedishLTE_SoilFertilityExperiment_R3-9001_WheatBarley.csv")) %>% mutate(site = factor(site, levels=c("M-1", "M-2", "M-3", "M-4", "M-5", "M-6", "R-94", "R-95", "E-10", "E-9", "C-7", "C-8")), crop = crop %>% as.factor(), crop.rotation = crop.rotation %>% as.factor(), PK.treatment = PK.treatment %>% as.factor(), year = year %>% as.integer(), N.treatment = N.treatment %>% as.integer(), variety = variety %>% as.factor())


yield.plot <- df_crops %>% filter(crop == "winter wheat" | crop == "spring barley") %>% group_by(site,crop) %>% ggplot(aes(year,yield)) + geom_point(color="gray51") +
  geom_smooth(method = lm, formula = y ~ x, color="red", aes(fill="red")) + facet_grid(crop~site) + theme(strip.text.x = element_text(size=14, color = "black"),strip.text.y = element_text(size=14, color = "black"), axis.text.x = element_text(angle = 45, hjust = 1.2, vjust = 1.2), axis.ticks.length.x=unit(.15, "cm"), legend.position = "none") + scale_x_continuous(breaks=seq(1970, 2010, 20))

(Figure1 <- cowplot::plot_grid(cowplot::plot_grid(NULL,lte_map, nrow=1, rel_widths=c(.05,1)), yield.plot, rel_widths = c(1,3), labels = c("a)","b)"), axis = "tr"))

## Figure 2 ----
df_climate <- read.csv2(paste0(here(),"\\data\\climate_data.csv"), fileEncoding = "UTF-8") %>% as_tibble() %>% mutate(dates= as.Date(dates), season=hydroTSM::time2season(dates, out.fmt = "seasons") %>% as.factor()) %>% mutate(season = fct_relevel(season, "spring","summer","autumm","winter"), season = fct_recode(season, "autumn"="autumm")) %>% filter(!(code=="M-4-1957" & dates > '2010-05-31') & !(code=="M-4-2010" & dates < "2010-09-01")) %>% mutate(site =  substr(`code`,start=1,nchar(as.character(`code`))-5)) %>% mutate(site = factor(site, levels=c("M-1", "M-2", "M-3", "M-4", "M-5", "M-6", "R-94", "R-95", "E-10", "E-9", "C-7", "C-8")))

# temp by season
df_month_values_tmp <- df_climate %>% mutate(year = format(dates, "%Y"), month = format(dates, "%m")) %>% group_by(year, month) %>% mutate(month_tmp_mean=mean(daily_tmp_avg), mean_month_tmp_max = mean(daily_tmp_max), mean_month_tmp_min = mean(daily_tmp_min), monthly_mean_tmp_sd = sd(`daily_tmp_avg`), monthly_max_tmp_sd = sd(`daily_tmp_max`), monthly_min_temp_sd = sd(`daily_tmp_min`))

# calculate difference between first year, last year averages
df_climate %>% mutate(year = format(dates, "%Y")) %>% group_by(site, year) %>% summarise(yearly_mean=mean(daily_tmp_avg)) %>% filter(year %in% c(1962,2015)) %>% summarise(yearly_mean_diff=diff(yearly_mean)) %>% summarise(mean_yearly_mean_diff=mean(yearly_mean_diff))

# calcuate ranges from min to max 
df_climate %>% mutate(year = format(dates, "%Y")) %>% group_by(season) %>% summarise(mean_max=mean(`daily_tmp_max`), mean_min = mean(`daily_tmp_min`)) %>% mutate(mean_range=mean_max-mean_min)

# mean temperature plot
mean_tmp_plot <- df_climate %>% mutate(year = format(dates, "%Y")) %>% group_by(season, year) %>% summarise(yearly_tmp_mean=mean(`daily_tmp_avg`), yearly_tmp_sd = sd(`daily_tmp_avg`)) %>% ggplot(aes(year, `yearly_tmp_mean`, group=1)) + geom_line() + geom_smooth(colour = "blue",size = 1) + geom_smooth(method="lm", color="red") + facet_grid(~season) + scale_x_discrete(breaks=c(1960,1970,1980,1990,2000,2010)) + 
  theme(strip.text.x = element_text(size=14, color = "black"), axis.title.y.left = element_text(size=16), axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) + ylab("mean daily temperatures per season")

#rainfall by season
df_season_values_precip <- df_climate %>%  mutate(month = format(dates, "%m"), year = format(dates, "%Y")) %>% 
  group_by(year, month) %>% mutate(monthly_rainfall=sum(daily_rainfall))  

# mean precipitation plot
mean_prec_plot <- df_climate %>% mutate(year = format(dates, "%Y")) %>% group_by(season, year) %>% summarise(mean_rainfall=mean(`daily_rainfall`), sd_rainfall = sd(`daily_rainfall`)) %>% ggplot(aes(year, `mean_rainfall`, group=1)) + geom_line() + geom_smooth(colour = "blue",size = 1) + geom_smooth(method="lm", color="red") + facet_grid(~season) + scale_x_discrete(breaks=c(1960,1970,1980,1990,2000,2010)) + 
  theme(strip.text.x = element_text(size=14, color = "black"), axis.text.x = element_text(angle = 45, hjust = 1.2, vjust = 1.2), axis.title.y.left = element_text(size=16, margin = margin(0, 9, 0, 0)), axis.title.x.bottom = element_text(size = 16)) + ylab("mean daily rainfall per season")


# plotting both temp and precip per season
(Figure2 <- cowplot::plot_grid(mean_tmp_plot, mean_prec_plot, nrow=2, rel_heights = c(.8,1)))


# Figure 3 & 4 -----

# Wheat (Figure 3)
source(paste0(here(), "\\code\\Analysis_wheat_barley.R"))

wheat.lme.CarbClim.preds <- list()
df_crops_precip_quants <- list()
for (q in 1:4) {
  df_crops_tmp_quants <- df_crops %>% filter(crop=="winter wheat") %>% pull(paste("gdd_meantemp_Q", q, sep = "")) %>% quantile(probs = c(0.05,0.5,0.95), na.rm=T)
  df_crops_precip_quants[[q]] <- df_crops %>% filter(crop=="winter wheat") %>% pull(paste("gdd_prec_Q", q, sep = "")) %>% quantile(probs = c(0.05,0.5,0.95), na.rm=T)
  wheat.lme.CarbClim.preds[[q]] <- list()
  j<-1
  for (i in df_crops_tmp_quants) {
    wheat.lme.CarbClim.preds[[q]][[j]] <-
      ggeffects::ggpredict(
        wheat.lme,
        terms = c(
          "topsoil.C.tot.imp [exp]",
          paste("gdd_prec_Q", q, " [", paste(df_crops_precip_quants[[q]], collapse = ","), "]", sep=""),
          paste("gdd_meantemp_Q", q, " [", i, "]", sep = "")
        ),
        type = "re"
      ) %>% plot () + ggtitle(paste(i, "°C", collapse = "")) + 
      coord_cartesian(ylim = c(-1000, 10500)) + 
      theme(legend.position="none") + xlab("topsoil carbon")
    wheat.lme.CarbClim.preds[[q]][[j]]$coordinates$expand <- FALSE
    j=j+1
  }
}

for (i in 1:4){
  for (j in c(2,3)){wheat.lme.CarbClim.preds[[i]][[j]] <- wheat.lme.CarbClim.preds[[i]][[j]] + ylab("")}
  if (! i==4) {for (j in c(1:3)){wheat.lme.CarbClim.preds[[i]][[j]] <- wheat.lme.CarbClim.preds[[i]][[j]] + xlab("")}}
  else {for (j in c(1,3)){wheat.lme.CarbClim.preds[[i]][[j]] <- wheat.lme.CarbClim.preds[[i]][[j]] + xlab("")}}
}

legend.q <- list()
for (q in 1:4) {
  p <- ggeffects::ggpredict(
    wheat.lme,
    terms = c(
      "topsoil.C.tot.imp [exp]",
      paste("gdd_prec_Q", q, " [", paste(round(df_crops_precip_quants[[q]]), collapse = ","), "]", sep=""),
      paste("gdd_meantemp_Q", q, " [1]", sep = "")
    ),
    #colors = cbPalette,
    type = "re"
  ) %>% plot () + labs(colour="precipitation")
  legend.q[[q]] <- get_legend(p)
}

wheat.ydens <-
  ggplot() + geom_density(
    data = df_crops %>% filter(crop == "winter wheat"),
    aes(x = yield),
    fill = "darkgrey",
    alpha = 0.15,
    size = .2
  ) + coord_flip( xlim = c(-1000, 10500)) + 
  theme(axis.text.y=element_text(size=9, colour = "gray51"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.margin = unit(c(.6,0.5,.8,0),"cm"))


(wheat.plot <- cowplot::plot_grid(
  cowplot::plot_grid(NULL,NULL,NULL,NULL,NULL, nrow = 5, rel_heights = c(.075, .25, .25, .25, .25), labels="a)", label_size = 18, label_y = .7),
  cowplot::plot_grid(NULL,
                     cowplot::plot_grid(plotlist=wheat.lme.CarbClim.preds[[1]], nrow=1, labels="Q1"),
                     cowplot::plot_grid(plotlist=wheat.lme.CarbClim.preds[[2]], nrow=1, labels="Q2"),
                     cowplot::plot_grid(plotlist=wheat.lme.CarbClim.preds[[3]], nrow=1, labels="Q3"),
                     cowplot::plot_grid(plotlist=wheat.lme.CarbClim.preds[[4]], nrow=1, labels="Q4"),
                     nrow=5, rel_heights = c(.075, .25, .25, .25, .25)), 
  cowplot::plot_grid(NULL,
                     cowplot::plot_grid(legend.q[[1]]),
                     cowplot::plot_grid(legend.q[[2]]),
                     cowplot::plot_grid(legend.q[[3]]),
                     cowplot::plot_grid(legend.q[[4]]),
                     nrow = 5, rel_heights = c(.075, .25, .25, .25, .25)),
  cowplot::plot_grid(NULL,NULL,NULL,NULL,NULL, nrow =5, rel_heights = c(.075, .25, .25, .25, .25),  labels="b)", label_size = 18, label_y = .7),
  cowplot::plot_grid(NULL,wheat.ydens,wheat.ydens,wheat.ydens,wheat.ydens, nrow = 5, rel_heights = c(.075, .25, .25, .25, .25)),
  rel_widths = c(.15, 5, .5, .1, .5), nrow = 1,
  align="hv"
)
)

# barley (Figure 4)

barley.lme.CarbClim.preds <- list()
df_crops_precip_quants <- list()
for (q in 1:4) {
  df_crops_quants <- df_crops %>% filter(crop=="spring barley") %>% pull(paste("gdd_meantemp_Q", q, sep = "")) %>% quantile(probs = c(0.05,0.5,0.95), na.rm=T)
  df_crops_precip_quants[[q]] <- df_crops %>% filter(crop=="winter wheat") %>% pull(paste("gdd_prec_Q", q, sep = "")) %>% quantile(probs = c(0.05,0.5,0.95), na.rm=T)
  barley.lme.CarbClim.preds[[q]] <- list()
  j<-1
  for (i in df_crops_quants) {
    barley.lme.CarbClim.preds[[q]][[j]] <-
      ggeffects::ggpredict(
        barley.lme,
        terms = c(
          "topsoil.C.tot.imp [exp]",
          paste("gdd_prec_Q", q, " [", paste(df_crops_precip_quants[[q]], collapse = ","), "]", sep=""),
          paste("gdd_meantemp_Q", q, " [", i, "]", sep = "")
        ),
        #colors = cbPalette,
        type = "re"
      ) %>% plot () + ggtitle(paste(i, "°C", collapse = "")) + 
      coord_cartesian(ylim = c(-2500, 15000)) + 
      theme(legend.position="none") + xlab("topsoil carbon")
    barley.lme.CarbClim.preds[[q]][[j]]$coordinates$expand <- FALSE
    j=j+1
  }
}  

for (i in 1:4){
  for (j in c(2,3)){barley.lme.CarbClim.preds[[i]][[j]] <- barley.lme.CarbClim.preds[[i]][[j]] + ylab("")}
  if (! i==4) {for (j in c(1:3)){barley.lme.CarbClim.preds[[i]][[j]] <- barley.lme.CarbClim.preds[[i]][[j]] + xlab("")}}
  else {for (j in c(1,3)){barley.lme.CarbClim.preds[[i]][[j]] <- barley.lme.CarbClim.preds[[i]][[j]] + xlab("")}}
}


legend.q <- list()
for (q in 1:4) {
  p <- ggeffects::ggpredict(
    barley.lme,
    terms = c(
      "topsoil.C.tot.imp [exp]",
      paste("gdd_prec_Q", q, " [", paste(round(df_crops_precip_quants[[q]]), collapse = ","), "]", sep=""),
      paste("gdd_meantemp_Q", q, " [1]", sep = "")
    ),
    #colors = cbPalette,
    type = "re"
  ) %>% plot () + labs(colour="precipitation")
  legend.q[[q]] <- get_legend(p)
}

barley.ydens <-
  ggplot() + geom_density(
    data = df_crops %>% filter(crop == "spring barley"),
    aes(x = yield),
    fill = "darkgrey",
    alpha = 0.15,
    size = .2
  ) + coord_flip( xlim = c(-2500, 15000))+ 
  theme(axis.text.y=element_text(size=9, colour = "gray51"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.margin = unit(c(.6,0.5,.8,0),"cm"))

(barley.plot <- cowplot::plot_grid(
  cowplot::plot_grid(NULL,NULL,NULL,NULL,NULL, nrow = 5, rel_heights = c(.075, .25, .25, .25, .25), labels="a)", label_size = 18, label_y = .7),
  cowplot::plot_grid(NULL,
                     cowplot::plot_grid(plotlist=barley.lme.CarbClim.preds[[1]], nrow=1, labels="Q1"),
                     cowplot::plot_grid(plotlist=barley.lme.CarbClim.preds[[2]], nrow=1, labels="Q2"),
                     cowplot::plot_grid(plotlist=barley.lme.CarbClim.preds[[3]], nrow=1, labels="Q3"),
                     cowplot::plot_grid(plotlist=barley.lme.CarbClim.preds[[4]], nrow=1, labels="Q4"),
                     nrow=5, rel_heights = c(.075, .25, .25, .25, .25)), 
  cowplot::plot_grid(NULL,
                     cowplot::plot_grid(legend.q[[1]]),
                     cowplot::plot_grid(legend.q[[2]]),
                     cowplot::plot_grid(legend.q[[3]]),
                     cowplot::plot_grid(legend.q[[4]]),
                     nrow = 5, rel_heights = c(.075, .25, .25, .25, .25)),
  cowplot::plot_grid(NULL,NULL,NULL,NULL,NULL, nrow =5, rel_heights = c(.075, .25, .25, .25, .25),  labels="b)", label_size = 18, label_y = .7),
  cowplot::plot_grid(NULL,barley.ydens,barley.ydens,barley.ydens,barley.ydens, nrow = 5, rel_heights = c(.075, .25, .25, .25, .25)),
  rel_widths = c(.15, 5, .5, .1, .5), nrow = 1,
  align="hv"
))

# Figure 5 ----

source(paste0(here(), "\\code\\Analysis_ley.R"))

## predictions
ley.lme.preds <- ggeffects::ggpredict(ley.lme, terms = c("year [all]", "crop.rotation"))
ley.lme.preds.rand <- ggeffects::ggpredict(ley.lme, terms = c("year [all]", "crop.rotation", "site"), type = "re")

## plots
av_eff_plot <- plot(ley.lme.preds) + ggtitle("population level effects of crop rotation on topsoil carbon")  + labs(colour="crop rotation") + ylab("topsoil carbon") + theme(legend.position = "none")
manual_cols  <- c('I' = '#E41A1C', 'II' = '#377EB8')
site_eff_plots <-   ley.lme.preds.rand %>% as_tibble() %>% ggplot(aes(y=predicted, x= x, col = group)) + geom_smooth(method="lm", formula = 'y ~ x', size=.1) + facet_wrap(~facet) + scale_color_manual(values=manual_cols, name="crop rotation", labels = c("ley-livestock", "annual crop")) + scale_x_continuous(breaks=seq(1970, 2010, 20)) + theme_sjplot() + xlab("year") + ylab("topsoil carbon")  + ggtitle("site specific  effects of crop rotation on topsoil carbon")  
legend <- get_legend(site_eff_plots)
site_eff_plots <- site_eff_plots + theme(legend.position = "none")

## altogether
(cowplot::plot_grid(av_eff_plot,
                   site_eff_plots,
                   legend,
                   nrow=1, labels = c("a)","b)",NULL), rel_widths = c(5,5,1))
)

# Supplementary Figures ----

# temp by season
df_climate %>%
  ggplot() +
  geom_ribbon(aes(x = dates, ymax = daily_tmp_max, ymin = daily_tmp_min), alpha = 0.6, fill = "grey") +
  geom_smooth(aes(dates, daily_tmp_avg), colour = "yellow",size = 1) +
  geom_smooth(aes(dates, daily_tmp_max), colour = "red",size = 1) +
  geom_smooth(aes(dates, daily_tmp_min), colour = "blue",size = 1) +
  theme(strip.text.x = element_text(color = "black"),strip.text.y = element_text(color = "black")) +
  facet_grid(season~site, scales = "free") + labs(y = "daily temperature", x = "year")

#rainfall by season
df_climate %>%  mutate(month = format(dates, "%m"), year = format(dates, "%Y")) %>%
  group_by(site, year, month) %>% mutate(monthly_rainfall=sum(daily_rainfall))  %>%
  ggplot(aes(y=monthly_rainfall,x=year)) + geom_boxplot() +
  geom_smooth(aes(group=1),colour = "blue",size = 1) + geom_smooth(aes(group=1),method="lm", colour = "red",size = 1) + scale_x_discrete(breaks=c(1960,1970,1980,1990,2000,2010)) +
  theme(strip.text.x = element_text(color = "black"),strip.text.y = element_text(color = "black"), axis.text.x = element_text(angle = 45, hjust = 1.2, vjust = 1.2)) +
  facet_grid(season~site, scales = "free")  + labs(y = "monthly rainfall", x = "year")
