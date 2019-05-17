library(dplyr)
library(ggplot2)
library(plotly)
library(broman)

setwd('D:/wd/Kaggle_HousePrices_shiny/')
train.data <- read.csv('train.csv')
test.data <- read.csv('test.csv')
View(train.data)

broman::plot_crayons()
p1 <- ggplot() +
    geom_histogram(mapping = aes(x = SalePrice),
                   data = train.data,
                   bins = 100,
                   alpha = 0.5,
                   fill = brocolors('crayons')['Denim'],
                   color = brocolors('crayons')['Gray'])
ggplotly(p1)


logarithmatic_price <- data.frame(log_price = log1p(train.data$SalePrice))

p2 <- ggplot() +
    geom_histogram(mapping = aes(x = log_price),
                   data = logarithmatic_price,
                   bins = 100,
                   alpha = 0.5,
                   fill = brocolors('crayons')['Denim'],
                   color = brocolors('crayons')['Gray'])
ggplotly(p2)
rm('p1', 'p2', 'logarithmatic_price')
