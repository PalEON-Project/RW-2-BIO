# comparing species range boundaries to site climate
library(tidyverse)
# load the .csv with species tolerances from LINKAGES
# load the .csv with species climate tolerances from silvics manual
clim.ranges <- read.csv("data/species_climate_ranges.csv")
# Load climate data
load('climate/prism_clim.RData')

# load the species results to get species at each site:
load('out/taxon_detrended_AGBI.RData')
taxon_save_comb$loc <- ifelse(taxon_save_comb$site %in% c("HARVARD Model RW", "HARVARD Model RW + Census"), "HARVARD", 
                                                          taxon_save_comb$site)
site.taxons<- taxon_save_comb  %>% ungroup()  %>% select(taxon, loc)%>% distinct()

# calculate january mins and july maxes
site.july <- prism_long %>% group_by(loc) %>% filter(month %in% "06")%>% summarise( July.temp.max = max(Tmax2))
site.jan <- prism_long %>% group_by(loc) %>% filter(month %in% "01")%>% summarise( Jan.temp.min = min(Tmin2))
site.means <- prism_long %>% group_by(loc, year) %>% summarise( Tave = mean(Tmean2), 
                                                  Precip = sum(PPT2)) %>% ungroup()%>% group_by(loc)%>%
                                      summarise(MAT = mean(Tave), 
                                                MAP = mean (Precip))
site.info <- left_join(site.means, site.jan) %>% left_join(., site.july) %>% left_join(., site.taxons)
colnames(clim.ranges)[1] <- "taxon"

ggplot()+
  geom_segment(data = clim.ranges, aes(x = taxon, y = MAT.low.C, yend = MAT.hi.C),size = 5)+
  geom_point(data = site.info, aes(x = taxon, y = MAT, color = loc))+
  coord_flip()+ylab("MAT range from silvics manual")

ggplot()+
  geom_segment(data = clim.ranges, aes(x = taxon, y = MAP.low.mm, yend = MAP.hi.mm), size = 5)+
  geom_point(data = site.info, aes(x = taxon, y = MAP, color = loc))+
  coord_flip()+ylab("MAP range from silvics manual")

ggplot()+
  geom_segment(data = clim.ranges, aes(x = taxon, y = Jan.temp.min, yend = July.temp.max), size = 5)+
  geom_point(data = site.info, aes(x = taxon, y = Jan.temp.min, color = loc), shape = 12)+
  geom_point(data = site.info, aes(x = taxon, y = July.temp.max, color = loc), shape = 18)+
  coord_flip()+ylab("January min temp to July Max temp from silvics manual")
