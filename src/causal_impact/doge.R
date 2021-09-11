# Reference:
# https://fabiandablander.com/r/Causal-Doge.html

# This analysis examines the causal effect for Elon Mask's tweet on cryptos.
# Covariates are input by adding them next to the target column.

library('dplyr')
library('ggplot2')
library('CausalImpact')

# load data ----
doge1 <- read_csv("input/doge-data-1.csv") %>% 
    mutate(date = as.POSIXct(date, tz = "UTC"))

# intervention ----
tweets <- as.POSIXct(
    c('2021-01-28 22:47:00 UTC',
      '2021-02-04 07:29:00 UTC',
      '2021-02-04 08:15:00 UTC', 
      '2021-02-04 07:57:00 UTC', 
      '2021-02-04 08:27:00 UTC'), 
    tz = 'UTC'
)

# fitting function ----
fit_model <- function(datsel, tweet_time, use_cov = TRUE) {
    
    doge <- filter(datsel, crypto == 'Dogecoin')
    bit <- filter(datsel, crypto == 'Bitcoin')
    
    times <- doge$date
    if (use_cov) {
        tofit <- zoo(cbind(doge$price, bit$price), times)
    } else {
        tofit <- zoo(doge$price, times)  # w/o BitCoin as a covariates
    }

    tweet_ix <- which(times == tweet_time)
    fit <- CausalImpact(
        tofit, times[c(1, tweet_ix)], times[c(tweet_ix + 1, length(times))]
    )
    
    fit
    
}

# Select subset of data for analysis ----
start_analysis <- as.POSIXct('2021-01-28 11:00:00 UTC', tz = 'UTC')
end_analysis <- as.POSIXct('2021-01-29 01:00:00 UTC', tz = 'UTC')
datsel <- filter(doge1, between(date, start_analysis, end_analysis))

fit1 <- fit_model(datsel, tweets[1])
fit2 <- fit_model(datsel, tweets[1], use_cov = FALSE)

plot(fit1) +
    xlab('Time') +
    ylab('Price') +
    ggtitle('CI w/ a BitCoin covariate') +
    theme_light()
ggsave("fig/DogeCoin_CausalImpact_with_covariate.png", 
       width = 10, height = 15, dpi = 300)

plot(fit2) +
    xlab('Time') +
    ylab('Price') +
    ggtitle('CI w/o a covariate') +
    theme_light()
ggsave("fig/DogeCoin_CausalImpact_without_covariate.png", 
       width = 10, height = 15, dpi = 300)