# global

library(shiny)
library(broman)
library(tidyverse)
library(lubridate)
library(plotly)
options(warn = -1)

# data inclusion
train.data <- read_csv('train.csv')
colnames.na <-
    colnames(train.data)[apply(is.na(train.data),MARGIN = 2, sum) > 0]
train.data <- train.data %>% sample_frac(size = 0.25)
test.data <- read_csv('test.csv')
macro.data <- read_csv('macro.csv', guess_max = 2000)
description <- scan('data_dictionary.txt',
                    what = 'character',
                    sep = '\n',
                    blank.lines.skip = FALSE)

# train.data$price_doc <- log1p(train.data$price_doc)
wday.stamp <- apply(train.data['timestamp'], MARGIN = 1, wday, label = TRUE)
month.stamp <- apply(train.data['timestamp'], MARGIN = 1, month, label = TRUE)
year.stamp <- apply(train.data['timestamp'], MARGIN = 1, year)
train.data <- train.data %>% mutate(wday_stamp = wday.stamp,
                                    month_stamp = month.stamp,
                                    year_stamp = as.factor(year.stamp)) %>% 
    mutate(floor_ratio = floor/as.numeric(max_floor),
           life_full_sq = life_sq/full_sq)
train.data$max_floor <- as.numeric(train.data$max_floor)
train.data$kitch_sq <- as.numeric(train.data$kitch_sq)
train.data$life_full_sq <- replace(train.data$life_full_sq,
                                   list = is.infinite(train.data$life_full_sq),
                                   values = 1)
train.data$floor_ratio <- replace(train.data$floor_ratio,
                                   list = is.infinite(train.data$floor_ratio),
                                   values = 1)
features <- train.data %>% select(-id, -price_doc)
features.na <- train.data[colnames.na]
colnames(features)[53:67] <- 
    paste0(rep('pop_', 15), colnames(features)[53:67])
colnames(train.data)[54:68] <- 
    paste0(rep('pop_', 15), colnames(train.data)[54:68])




# unique(sapply(features, class))
# features.factor <- 
#     colnames(features)[sapply(features, class) == 'factor']
features.factor <-
    colnames(features)[sapply(features, class) == 'character' |
                           sapply(features, class) == 'factor' ]

features.numeric <-
    colnames(features)[sapply(features, class) == 'numeric' | 
                           sapply(features, class) == 'integer']
features.integer <- 
    colnames(features)[sapply(features, class) == 'integer']
features.Date <- 
    colnames(features)[sapply(features, class) == 'Date']
features.na.numeric <- 
    colnames(features.na)[sapply(features.na, class) == 'integer' |
                              sapply(features.na, class) == 'numeric']
features.na.factor <- 
    colnames(features.na)[sapply(features.na, class) == 'character']

# macro data
macro.data['child_on_acc_pre_school'] <- 
    sapply(macro.data['child_on_acc_pre_school'],
           FUN = sub,
           pattern = '#!',
           replacement = NA)
macro.data['child_on_acc_pre_school'] <- 
    sapply(macro.data['child_on_acc_pre_school'],
           FUN = sub,
           pattern = ',',
           replacement = '')
macro.data['child_on_acc_pre_school'] <- 
    sapply(macro.data['child_on_acc_pre_school'], as.numeric)

macro.features <- macro.data %>% select(-timestamp)
macro.data.201108 <- 
    macro.data %>% filter(timestamp > "2011-08-01")
macro.colnames.na <-
    colnames(macro.data.201108)[apply(is.na(macro.data.201108), MARGIN = 2, sum) > 0]
macro.features.na <- macro.data.201108[macro.colnames.na] # all numeric
macro.features.factor <-
    colnames(macro.features)[sapply(macro.features, class) == 'character'] # null
macro.features.integer <-
    colnames(macro.features)[sapply(macro.features, class) == 'integer']
macro.features.numeric <-
    colnames(macro.features)[sapply(macro.features, class) == 'numeric']
macro.features.matrix <-
    colnames(macro.features)[sapply(macro.features, class) == 'matrix']

train.data.macro <-
    train.data %>% left_join(macro.data, by = 'timestamp')
train.data.macro <- 
    train.data.macro %>%
    select(one_of('timestamp',
                  'month_stamp',
                  'year_stamp',
                  colnames(macro.features),
                  'price_doc'))

