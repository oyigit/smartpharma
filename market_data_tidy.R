library(openxlsx)
library(tidyverse)
library(plotly)
library(zoo)
library(anonymizer)

data <- read.xlsx("market_data.xlsx")

data$prd <- anonymize(data$prd)


unit_sales <- data %>% 
  select(2, 54:113) %>%
  gather(key = month, value = revenue, -prd) %>%
  mutate(month = str_sub(month, 7, 12)) %>%
  mutate(month = as.yearmon(month, "%b/%y")) %>%
  mutate(month = as.Date(month))

tl_sales <- data %>% 
  select(2, 148:207) %>%
  gather(key = month, value = revenue, -prd) %>%
  mutate(month = str_sub(month, 4, 9)) %>%
  mutate(month = as.yearmon(month, "%b/%y")) %>%
  mutate(month = as.Date(month))





