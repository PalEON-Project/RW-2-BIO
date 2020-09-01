## Visualizing the different ROOSTER plots 
rm(list=ls())
library(dplyr)
library(gridExtra)

# 1. Andria's sampling correction plot 
RDSloc1 = '~/Desktop/PalEON/SDA/sites/ROOSTER/NPP_STAT_MODEL_ROOSTER.RDS'
dat1 = readRDS(RDSloc1)

plot1 = dat1 %>% filter(type == 'AB', !is.na(value)) %>% 
  group_by(plot, year, iter) %>%
  summarize(total = sum(value)) %>%
  ungroup() %>%
  group_by(plot, year) %>%
  summarize(total = median(total))

pl1 = ggplot(plot1) + 
  geom_line(aes(x = year, y = total, group = plot, color = as.factor(plot))) + 
  theme(legend.position = 'none') + 
  labs(title = 'Shared Drive Alpha') 

rm(dat1, plot1)

# 2. Andria's nonsampling correction plot (I think this one I do not have)

# 3. My version of Andria's sampling correction plot 
RDSloc3 = '~/Desktop/RW-2-BIO/sites/ROOSTER/output/NPP_STAT_MODEL_ROOSTER_ALPHA.RDS'
dat3 = readRDS(RDSloc3)

plot3 = dat3 %>% filter(type == 'AB', !is.na(value)) %>% 
  group_by(plot, year, iter) %>%
  summarize(total = sum(value)) %>%
  ungroup() %>%
  group_by(plot, year) %>%
  summarize(total = median(total))

pl3 = ggplot(plot3) + 
  geom_line(aes(x = year, y = total, group = plot, color = as.factor(plot))) + 
  theme(legend.position = 'none') + 
  labs(title = 'Rerun Original Alpha')

rm(dat3, plot3)

# 4. My version of Andria's nonsampling correction plot
RDSloc4 = '~/Desktop/RW-2-BIO/sites/ROOSTER/output/NPP_STAT_MODEL_ROOSTER_NOALPHA.RDS'
dat4 = readRDS(RDSloc4)

plot4 = dat4 %>% filter(type == 'AB', !is.na(value)) %>% 
  group_by(plot, year, iter) %>%
  summarize(total = sum(value)) %>%
  ungroup() %>%
  group_by(plot, year) %>%
  summarize(total = median(total))

pl4 = ggplot(plot4) + 
  geom_line(aes(x = year, y = total, group = plot, color = as.factor(plot))) + 
  theme(legend.position = 'none') + 
  labs(title = 'Rerun Original No Alpha')

rm(dat4, plot4)

# 4. My sampling correction plot 
RDSloc5 = '~/Desktop/RW-2-BIO/sites/ROOSTER/output/AGB_STAN_ROOSTER_v2.0_082020.RDS'
dat5 = readRDS(RDSloc5)

plot5 = dat5 %>% filter(type == 'ab', !is.na(value)) %>% 
  group_by(plot, year, iter) %>%
  summarize(total = sum(value)) %>%
  ungroup() %>%
  group_by(plot, year) %>%
  summarize(total = median(total))

pl5 = ggplot(plot5) + 
  geom_line(aes(x = year, y = total, group = plot, color = as.factor(plot))) + 
  theme(legend.position = 'none') + 
  labs(title = 'Updated Alpha')

rm(dat5, plot5)

# 5. My nonsampling correction plot 
RDSloc6 = '~/Desktop/RW-2-BIO/sites/ROOSTER/output/AGB_NOFIX_STAN_ROOSTER_v2.0_082020.RDS'
dat6 = readRDS(RDSloc6)

plot6 = dat6 %>% filter(type == 'ab', !is.na(value)) %>% 
  group_by(plot, year, iter) %>%
  summarize(total = sum(value)) %>%
  ungroup() %>%
  group_by(plot, year) %>%
  summarize(total = median(total))

pl6 = ggplot(plot6) + 
  geom_line(aes(x = year, y = total, group = plot, color = as.factor(plot))) + 
  theme(legend.position = 'none') + 
  labs(title = 'Updated No Alpha')

rm(dat6, plot6)

grid.arrange(pl1, pl4, pl3, pl6, pl5)
