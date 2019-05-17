library(tidyverse)
library(plotly)
library(lubridate)
library(broman)

# data inclusion
train.data <- read_csv('train.csv')
macro.data <- read_csv('macro.csv', guess_max = 2000)

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
macro.features.factor <-
    colnames(macro.features)[sapply(macro.features, class) == 'character']
macro.features.integer <-
    colnames(macro.features)[sapply(macro.features, class) == 'integer']
macro.features.numeric <-
    colnames(macro.features)[sapply(macro.features, class) == 'numeric']
macro.features.matrix <-
    colnames(macro.features)[sapply(macro.features, class) == 'matrix']


wday.stamp <- apply(train.data['timestamp'], MARGIN = 1, wday, label = TRUE)
month.stamp <- apply(train.data['timestamp'], MARGIN = 1, month, label = TRUE)
year.stamp <- apply(train.data['timestamp'], MARGIN = 1, year)

train.data <- train.data %>% mutate(wday_stamp = wday.stamp,
                                    month_stamp = month.stamp,
                                    year_stamp = as.factor(year.stamp))
train.data <- train.data %>% left_join(macro.data, by = 'timestamp')

gg.wday <- ggplot(data = train.data) +
    geom_boxplot(mapping = aes(x = wday_stamp, y = price_doc),
                 color = brocolors('crayons')['Yellow Orange'],
                 outlier.fill = brocolors('crayons')['Shadow'],
                 outlier.color = brocolors('crayons')['Shadow'],
                 outlier.size = 0.1,
                 outlier.alpha = 0.2) +
    ylim(c(0, 3e7))
ggplotly(gg.wday)

gg.month <- ggplot(data = train.data) +
    geom_boxplot(mapping = aes(x = month_stamp, y = price_doc),
                 color = brocolors('crayons')['Yellow Orange'],
                 outlier.fill = brocolors('crayons')['Shadow'],
                 outlier.color = brocolors('crayons')['Shadow'],
                 outlier.size = 0.1,
                 outlier.alpha = 0.2) +
    ylim(c(0, 3e7))
ggplotly(gg.month)

gg.year <- ggplot(data = train.data) +
    geom_boxplot(mapping = aes(x = as.factor(year_stamp), y = price_doc),
                 color = brocolors('crayons')['Yellow Orange'],
                 outlier.fill = brocolors('crayons')['Shadow'],
                 outlier.color = brocolors('crayons')['Shadow'],
                 outlier.size = 0.1,
                 outlier.alpha = 0.2) +
    ylim(c(0, 3e7))
ggplotly(gg.year)
