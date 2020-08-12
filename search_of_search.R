#### Share of Search ####
library(gtrendsR); library(tidyverse); library(ggplot2); library(scales)
library(ggthemes); library(magrittr); library(zoo)

search_data <- gtrends(keyword = c('metro', 'wong', 'plaza vea', 'tottus', 
                                   'makro'),
        geo = 'PE', time = 'all', gprop = 'web')
search_data <- search_data$interest_over_time
search_data$hits <- as.numeric(search_data$hits)
search_data$hits[is.na(search_data$hits)] <- 0

roll_data <- search_data %>% 
  group_by(date) %>% 
  arrange(date) %>% 
  mutate(rollbase = sum(hits)) %>%  
  ungroup() %>% 
  group_by(keyword) %>% 
  mutate(rollmean = rollmean(x = rollbase, k = 12, na.pad = TRUE, 
                             align = 'right')) %>% 
  mutate(rollkey = rollmean(x = hits, k = 12, na.pad = TRUE,
                            align = 'right')) %>% 
  ungroup() %>% 
  mutate(SOS = hits/rollmean) %>% 
  group_by(date) %>% 
  mutate(SOS_per = SOS/sum(SOS)) %>% 
  ungroup() %>% 
  filter(date >= "2017-01-01")

ggplot(data = roll_data, aes(x = date, y = SOS_per, fill = keyword)) +
  geom_area(color = 'white') + scale_y_continuous(labels = percent) + 
  scale_fill_discrete(name = "Marca", labels = c('Makro', "Metro", 'Plaza Vea',
                                                 'Tottus', 'Wong')) +
  ylab('Search of Search (%)') + xlab('Fecha') +
  ggtitle('Evolutivo Share of Search (%) - Supermercados')