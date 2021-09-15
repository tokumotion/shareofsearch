#### Share of Search ####
library(gtrendsR); library(tidyverse); library(ggplot2); library(scales)
library(ggthemes); library(magrittr); library(zoo); library(broom);
library(lubridate)

# Get the data from Google Trends, currently Trends is limited to 5 search terms
# at the time, for additional brands to measure, you need to run gtrends() with 
# the biggest brand and the reminaing set of brands.. 
# Remember this returns MONTHLY DATA.
search_data1 <- gtrends(keyword = c('betfair', 'wplay', 'rushbet', 
                                    'bwin'),
        geo = 'CO', time = 'today 1-m', gprop = 'web')
search_data2 <- gtrends(keyword = c('wplay', 'yajuego', 'zamba',
                                    'rivalo', 'codere'),
                       geo = 'CO', time = 'today 1-m', gprop = 'web')

# gtrendsR returns a list, you need a data frame (interest_over_time)
search_data1 <- search_data1$interest_over_time
search_data2 <- search_data2$interest_over_time

# standarize the data class of search_data$hits
search_data1$hits <- as.character(search_data1$hits)
search_data2$hits <- as.character(search_data2$hits)

# Unite both data frames
search_data <- union(x = search_data1, y = search_data2)

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
# periods: 7 if daily data, 12 if monthly
# (Optional)
sos <- function(search_data, initial_period, final_period, periods){
  if(missing(final_period)){
    search_data %>% 
      group_by(date) %>% 
      arrange(date) %>% 
      mutate(rollbase = sum(hits)) %>%  
      ungroup() %>% 
      group_by(keyword) %>% 
      mutate(rollmean = rollmean(x = rollbase, k = periods, na.pad = TRUE, 
                                 align = 'right')) %>% 
      mutate(rollkey = rollmean(x = hits, k = periods, na.pad = TRUE,
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
    mutate(rollmean = rollmean(x = rollbase, k = periods, na.pad = TRUE, 
                               align = 'right')) %>% 
    mutate(rollkey = rollmean(x = hits, k = periods, na.pad = TRUE,
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
sos_data <- sos(search_data = search_data, initial_period = "2021-09-05", periods = 7)
sos_data$date <- as.Date(sos_data$date)
datebreaks <- seq(as.Date("2021-09-06"), as.Date("2021-09-12"), by="1 day")

# You can use ggplot2 to see how your brand's category evolved
ggplot(data = sos_data, aes(x = date, y = SOS_per, fill = keyword)) +
  geom_area(color = 'white') + scale_y_continuous(labels = percent) + 
  scale_x_date(breaks = datebreaks) +
  theme_clean() +
  scale_fill_discrete(name = "Brands", 
                      labels = c('BetFair', "BWin", 'Codere', 
                                 'Rivalo', 'RushBet', 'WPlay', "YaJuego", 'Zamba')) +
  ylab('Search of Search (%)') + xlab('Date') +
  ggtitle('Evolution of Share of Search (%) - Casas de Apuestas') 

# Create the facets
ggplot(data = sos_data, aes(x = date, y = SOS_per, color = keyword)) +
  geom_line() + scale_y_continuous(labels = percent) + 
  theme_clean() +
  facet_grid(keyword ~ .) +
  scale_color_discrete(name = "Brands", 
                      labels = c('Ann Chery', 'Fajate', "Leonisa", 'Maria E', 
                                 'Salome', 'Fajitex', 'MyD', "Yina Calderon")) +
  ylab('Search of Search (%)') + xlab('Date') +
  ggtitle('Evolution of Share of Search (%) - Fajas') 

sos_data %>% 
  mutate(year = year(date)) %>% 
  group_by(year, keyword) %>% 
  summarise(mean = mean(SOS_per)) %>% 
  ggplot(aes(x = year, y = mean, fill = keyword)) +
  geom_bar(stat = 'identity') + scale_y_continuous(labels = percent) + 
  theme_clean() +
  #scale_fill_discrete(name = "Brands", 
  #                    labels = c('Ann Chery', 'Fajate', "Leonisa", 'Maria E', 
  #                               'Salome', 'Fajitex', 'MyD', "Yina Calderon")) +
  ylab('Search of Search (%)') + xlab('Date') +
  ggtitle('Yearly Evolution of Share of Search (%) - Casas de Apuestas') 

sos_data %>% 
  group_by(date, keyword) %>% 
  summarise(mean = mean(SOS_per)) %>% 
  ggplot(aes(x = date, y = mean, fill = keyword)) +
  geom_bar(stat = 'identity') + scale_y_continuous(labels = percent) + 
  theme_clean() +
  #scale_fill_discrete(name = "Brands", 
  #                    labels = c('Ann Chery', 'Fajate', "Leonisa", 'Maria E', 
  #                               'Salome', 'Fajitex', 'MyD', "Yina Calderon")) +
  ylab('Search of Search (%)') + xlab('Date') +
  ggtitle('Yearly Evolution of Share of Search (%) - Casas de Apuestas') 

# Further development: Test for normality
sos_data %>% 
  group_by(keyword) %>% 
  do(tidy(shapiro.test(.$SOS_per))) %>% 
  ungroup() %>% 
  select(-method)

# Results are not normal, can't run lm() without normalizing data