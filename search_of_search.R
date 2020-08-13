#### Share of Search ####
library(gtrendsR); library(tidyverse); library(ggplot2); library(scales)
library(ggthemes); library(magrittr); library(zoo); library(broom)

# Get the data from Google Trends, currently Trends is limited to 5 search terms
# at the time. Remember this returnS MONTHLY DATA.
search_data <- gtrends(keyword = c('metro', 'wong', 'plaza vea', 'tottus', 
                                   'makro'),
        geo = 'PE', time = 'all', gprop = 'web')

# gtrendsR returns a list, you need a data frame (interest_over_time)
search_data <- search_data$interest_over_time

# Change the class of the hits data and change all NAs to 0
search_data$hits <- as.numeric(search_data$hits)
search_data$hits[is.na(search_data$hits)] <- 0

# Wrapper function to calculate SoS
# It uses 2 obligatory parameters and one optional
# search_data: The data frame from gtrendsR previously shown (Obligatory)
# initial_period: The first day of the month & year you want to start running 
# the analysis (Obligatory)
# final_period: The first day of the month & year you want to finish running 
# the analysis. If left blank, it will default to the final month of the data 
# (Optional)
sos <- function(search_data, initial_period, final_period){
  if(missing(final_period)){
    search_data %>% 
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
      filter(date >= initial_period & date <= last(date))
  } else {
  search_data %>% 
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
    filter(date >= initial_period & date <= final_period)
  }
}

#### Example ####
sos_data <- sos(search_data = search_data, initial_period = "2017-01-01")

# You can use ggplot2 to see how your brand's category evolved
ggplot(data = sos_data, aes(x = date, y = SOS_per, fill = keyword)) +
  geom_area(color = 'white') + scale_y_continuous(labels = percent) + 
  scale_fill_discrete(name = "Marca", labels = c('Makro', "Metro", 'Plaza Vea',
                                                 'Tottus', 'Wong')) +
  ylab('Search of Search (%)') + xlab('Fecha') +
  ggtitle('Evolutivo Share of Search (%) - Supermercados')

# Furher development: Test for normality
roll_data %>% 
  group_by(keyword) %>% 
  do(tidy(shapiro.test(.$SOS_per))) %>% 
  ungroup() %>% 
  select(-method)
# Results are not normal, can't run lm() without normalizing data