# This script creates a series of charts. 

# Load packages
library(tidyverse)
library(readr)
library(sf)

cat('\nChart construction begins. Charts are saved to "plots" folder.\n')

# Step 1. Generate charts related to zones of control ----

# Chart area assessed as Russia controlled over time
area_map <- read_csv("output-data/area_assessed_as_controlled_by_Russia.csv")
ggplot(area_map, aes(x=date, y=area_assessed_as_russia_controlled/1000000))+geom_line()+theme_minimal()+ylab('')+
  xlab('\nSource: ISW')+
  ggtitle('Area assessed as Russia-controlled\nkm2')
ggsave('plots/area_assessed_as_russia_controlled_by_day.png', width = 8, height = 5)

# Chart area assessed as Russia controlled over time
ggplot(area_map, aes(x=date, y=change_in_area_assessed_as_russia_controlled_in_past_7_days/1000000))+geom_line()+theme_minimal()+ylab('')+
  xlab('')+
  ggtitle('Change in area assessed as Russia-controlled\nkm2, past 7 days')+
  xlab('\nSource: ISW')
ggsave('plots/change_in_area_assessed_as_russia_controlled_by_day.png', width = 8, height = 5)

# Step 2. Generate charts of areas touched by the fighting ----

# Load analysis data:
dat <- readRDS('output-data/X_analysis_data.RDS')

# Chart share of municipalities and regions touched by war
ukraine <- st_read('source-data/ukraine-detailed-map/ukr_admbnda_adm3_sspe_20230201.shp')
ukraine <- st_transform(ukraine, "WGS84")

# A. Get share of municipalities touched by war
munis <- as.data.frame(ukraine[ukraine$ADM0_PCODE != "" & !is.na(ukraine$ADM0_PCODE), ])
munis <- munis[!duplicated(munis$ADM3_PCODE), ]
munis$war <- munis$ADM3_PCODE %in% dat$ADM3_PCODE[dat$war_fire == 1]

# B. Get share of regions touched by war:
res <- data.frame()
length(unique(munis$ADM1_PCODE)) == length(unique(munis$ADM1_EN))
for(i in unique(munis$ADM1_EN)){
  res <- rbind(res, c(i, mean(munis$war[munis$ADM1_EN == i])))
}
res[, 2] <- round(as.numeric(res[, 2]), 4)*100
colnames(res) <- c('region', 'percent_of_municipalities_with_war_fires')
res <- res[rev(order(res[, 2])), ]
regions <- res

regions <- rbind(regions, c('Ukraine', as.numeric(mean(munis$war)*100)))
regions$percent_of_municipalities_with_war_fires <- as.numeric(regions$percent_of_municipalities_with_war_fires)

regions$region <- factor(regions$region, levels = rev(regions$region))

# Plot of affected regions, by % of municipalities
ggplot(regions[regions$percent_of_municipalities_with_war_fires > 0, ],
       aes(x=percent_of_municipalities_with_war_fires, y=region, yend=region, xend= 0))+geom_segment(size = 2)+xlim(c(0,100))+xlab('')+ylab('')+ggtitle('Regions enflamed by fighting\nBy % of municipalities with war-related fires*, Feb 24 2022-Feb 24 2023')+theme_minimal()+theme(legend.pos = 'none')
ggsave('plots/share_of_regions_affected_by_fighting.png', width = 8, height = 5)

# Step 3. Generate charts of war events by time and location ----

# Load analysis data:
dat <- readRDS('output-data/X_analysis_data.RDS')

# Restrict to war-fire events
dat <- dat[dat$war_fire == 1, ]
dat <- dat[dat$date >= as.Date('2022-02-24'), ]
dat <- dat[!is.na(dat$date), ]

# Load cloud cover data:
clouds <- read_csv('output-data/cloud_cover_in_ukraine_by_day.csv')

# Assess % of fire in Russian-held areas:
dat$in_russia_held_area_by_day <- ave(dat$in_russia_held_area, dat$date, FUN = mean)

# Define average for a given day:
dat$pop_exact_daily_average <- ave(dat$pop_exact, dat$date, FUN = function(x) mean(x, na.rm = T))

# Get total fires on a given day:
dat$total_daily_fires <- ave(dat$fire, dat$date, FUN = function(x) sum(x, na.rm = T))

# Get daily average population density of strikes in non-Russian held areas:
dat$pop_exact_daily_average_in_non_russian_areas <- NA
dat$pop_exact_daily_average_in_non_russian_areas[dat$in_russia_held_area == 0] <- dat$pop_exact[dat$in_russia_held_area == 0]
dat$pop_exact_daily_average_in_non_russian_areas <- ave(dat$pop_exact_daily_average_in_non_russian_areas, dat$date, FUN = function(x) mean(x, na.rm = T))

# In urban area dummy
dat$urban_area_1500 <- dat$pop_exact > 1500

# Get daily average population density of strikes in non-Russian held areas:
dat$urban_area_1500_daily_average_in_non_russian_areas <- NA
dat$urban_area_1500_daily_average_in_non_russian_areas[dat$in_russia_held_area == 0] <- dat$urban_area_1500[dat$in_russia_held_area == 0]
dat$urban_area_1500_daily_average_in_non_russian_areas <- ave(dat$urban_area_1500_daily_average_in_non_russian_areas, dat$date, FUN = function(x) mean(x, na.rm = T))

ggplot(dat[!duplicated(dat$date), ], 
       aes(x=date, y=urban_area_1500_daily_average_in_non_russian_areas, 
           size = total_daily_fires, weight = total_daily_fires))+
  geom_point()+xlab('')+
  ylab('')+theme_minimal()+ggtitle('Strikes in non-Russia held areas hitting urban areas\n% per day')+theme(legend.title = element_blank(), legend.pos = 'bottom')
ggsave('plots/strikes_hitting_urban_areas_not_controlled_by_russia_per_day.png', width = 8, height = 5)

ggplot()+geom_segment(data=dat, aes(x=date, y=in_russia_held_area_by_day*total_daily_fires, col = 'Russia-held territory', xend=date, yend = 0,))+
  geom_segment(data=dat,
               aes(x=date, xend=date, yend = 0,
                   y=-(1-in_russia_held_area_by_day)*total_daily_fires,
                   col = 'Ukraine-held or contested territory'))+
  theme_minimal()+theme(legend.title = element_blank())+ylab('')+xlab('')+xlim(c(as.Date('2022-02-24'),as.Date('2023-02-24')))+
  ggtitle('War related fires\nBy zones of control')+theme(legend.pos = 'bottom')+geom_vline(data=clouds[clouds$cloud_cover_in_country > 0.6 & clouds$date >= as.Date('2022-02-24'), ], aes(xintercept=date, col='Cloud cover over 60%'), alpha = 0.2, show.legend = F)+xlab('\n (vertical lines indicate >60% cloud cover)')

cat('\nChart construction completed.\n')