library(broman)
library(tidyverse)
library(plotly)

# data inclusion
train.data <- read_csv('train.csv')
colnames.na <-
    colnames(train.data)[apply(is.na(train.data),MARGIN = 2, sum) > 0]
test.data <- read_csv('test.csv')
macro.data <- read_csv('macro.csv')
description <- scan('data_dictionary.txt',
                    what = 'character',
                    sep = '\n',
                    blank.lines.skip = FALSE)

train.median <- sapply(train.data, median, na.rm = TRUE)
test.median <- sapply(test.data, median, na.rm = TRUE)

