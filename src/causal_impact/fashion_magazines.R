library(zoo)
library(tidyverse)
library(bsts)
library(CausalImpact)
library(patchwork)
library(extrafont)

# Environment ----
font.1 <- "Vogue"
color.0 <- "#101010"
color.1 <- "#501021"  # Chocolate Cosmos
color.2 <- "#8C2335"  # Red-Violet
color.3 <- "#AE4841"  # Deep Chestnut
color.4 <- "#E6B15D"  # Sunray
color.5 <- "#56A5A2"  # Cadet Blue
color.6 <- "#1F5281"  # Blue Sapphire


# Load data ----
dat <- read_csv("input/fashion_magazines.csv")
dat <- dat %>% mutate(PERIOD = as.Date(PERIOD))

# Visual plot to see data trend ----
ggplot(dat, aes(x = PERIOD, y = VOGUE_JAPAN))+
    geom_point(fill = color.2, color = color.1, alpha = 0.7, size = 3) +
    geom_line(color = color.2, alpha = 0.7) +
    labs(x = "Date", y = "Number of copies printed") +
    theme_minimal() +
    theme(text = element_text(family = font.1, 
                              color = color.0, size = 8.5))
ggsave("fig/fashion_magazines_01.png", dpi = 150)

# Create ts zoo data ----
# Post Intervention Period is filled with NA for bsts
zoo_causal <- dat %>% 
  mutate(
    VOGUE_JAPAN = replace(VOGUE_JAPAN, PERIOD >= as.Date("2020-01-01"), NA)
    )

# zoo: ordered observations which includes irregular time series.
zoo_causal <- zoo(zoo_causal$VOGUE_JAPAN, zoo_causal$PERIOD)

# Model fit ----
ss <- list()

# Add local trend and weekly-seasonal
ss <- AddSemilocalLinearTrend(ss, zoo_causal)
ss <- AddSeasonal(ss, zoo_causal, nseasons = 4)  # quarterly

model <- bsts(zoo_causal,
              state.specification = ss, 
              niter = 1000, burn = 250)
plot(model, main = "Local trend and seasonal Model")
plot(model, "components")

# Causal Impact of SoE ----
pre.period <- as.Date(c("2015-01-01", "2020-01-01"))
post.period <- as.Date(c("2020-01-01", "2021-04-30"))

# Obtain post period data
dat_post <- dat %>% filter(PERIOD >= as.Date("2020-01-01"))

# Do CausalImpact
impact <- CausalImpact(bsts.model = model,
                       post.period.response = dat_post$VOGUE_JAPAN, 
                       alpha = 0.05)
plot(impact)

# Summarize via visualization ----
impact.res <- as.data.frame(impact$series)

p1 <- impact.res %>% 
    mutate(Date = as.Date(rownames(impact.res))) %>% 
    ggplot() +
    geom_rect(aes(xmin = as.Date("2020-01-01"), xmax = as.Date("2021-04-01"),
                  ymin = 0, ymax = 80000), 
              fill = "gray", color = NA, alpha = 0.2) +
    geom_point(mapping = aes(x = Date, y = response),
               size = 1.5, color = "gray0", alpha = 0.8) +
    geom_line(mapping = aes(x = Date, y = response),
              color = "gray0", alpha = 0.5) + 
    geom_text(aes(x = as.Date("2020-08-15"), y = 20000, label = "COVID-19"),
              color = "white", size = 7, family = font.1) +
    geom_point(mapping = aes(x = Date, y = point.pred),
               size = 2, color = "deeppink3", alpha = 0.5) +
    geom_ribbon(aes(x = Date, ymin = point.pred.lower, ymax = point.pred.upper), 
                fill = "violetred1", alpha = 0.2) +
    annotate("text", x = as.Date("2020-02-15"), y = 70000, 
             label = "point-wise impact and credible interval", 
             color = "violetred1", size = 3, family = font.1) +
    annotate("text", x = as.Date("2017-08-15"), y = 40000, 
             label = "obserbation", color = "gray0", size = 4, family = font.1) +
    labs(title = "Causal Impact Analysis for VOGUE JAPAN",
         subtitle = "Paper copies only, not including digital versions.",
         caption = "JMPA: https://www.j-magazine.or.jp/user/printed/",
         x = "Date (by 3 months)", y = "Number of copies printed") +
    theme_minimal() +
    theme(text = element_text(family = font.1,
                              color = color.0, size = 10))

p2 <- impact.res %>% 
    mutate(Date = as.Date(rownames(impact.res))) %>% 
    filter(Date >= "2019-01-01") %>% 
    ggplot() +
    geom_rect(aes(xmin = as.Date("2020-01-01"), xmax = as.Date("2021-04-01"),
                  ymin = min(cum.effect.lower) - 100, 
                  ymax = max(cum.effect.upper) + 100), 
              fill = "gray", color = NA, alpha = 0.2) +
    geom_point(mapping = aes(x = Date, y = cum.effect),
               size = 1.5, color = "darkviolet", alpha = 0.8) +
    geom_ribbon(aes(x = Date, ymin = cum.effect.lower, ymax = cum.effect.upper), 
                fill = "darkviolet", alpha = 0.2) +
    annotate("text", x = as.Date("2020-08-15"), y = -75000, 
             label = "COVID-19", color = "white", size = 7, family = font.1) +
    labs(title = "Cumulative effect of COVID-19 on paper copies",
         x = "Date (by 3 months)", y = "Number of copies printed") +
    theme_minimal() + 
    theme(text = element_text(family = font.1, 
                              color = color.0, size = 8.5))

p1 / p2

ggsave("fig/VOGUE_causal-impact.png", width = 8, height = 8, dpi = 150)

summary(impact)
summary(impact, "report")
