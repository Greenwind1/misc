library(zoo)
library(tidyverse)
library(bsts)
library(CausalImpact)
library(patchwork)


# Load data: Shinjuku City data: PM 2.5 ----
dat <- read_csv("input/shinjuku-air-quality.txt")
dat <- dat %>% 
  mutate(date = as.Date(date)) %>% 
  filter(date >= as.Date("2016-04-04"))

# EDA and Preprocessing ----
# Visual plot to see data trend
ggplot(dat, aes(x = date, y = pm25))+
  geom_point(color = "darkgoldenrod3", alpha = 0.7)+
  labs(title = "Shinjuku PM2.5 data from 2014 to 2021", 
       x = "Date", y = "PM2.5") +
  theme_minimal()
ggsave("fig/Sinjuku_pm25.png", dpi = 300)

# Missing data
dat %>% filter(is.na(pm25))

# Convert data to aggregation by week
dat_pm25_wk <- dat %>% 
  group_by(week = cut(date, "week")) %>%
  summarise(pm25 = mean(pm25, na.rm = TRUE)) %>%
  mutate(week = as.Date(as.character(week)))

# Create row index for week
dat_pm25_wk <- dat_pm25_wk %>%
  mutate(index = row_number())

# Visual plot to see data trend
# SoE reference:
# /https://www.nippon.com/ja/japan-data/h00908/
ggplot(dat_pm25_wk, aes(x = week, y = pm25))+
  geom_rect(aes(xmin = as.Date("2020-04-07"), xmax = as.Date("2020-05-25"),
                ymin = min(pm25) - 5, ymax = max(pm25) + 5), 
            fill = "gray", color = NA, alpha = 0.3) +
  geom_rect(aes(xmin = as.Date("2021-01-08"), xmax = as.Date("2021-02-22"),
                ymin = min(pm25) - 5, ymax = max(pm25) + 5), 
            fill = "gray", color = NA, alpha = 0.3) +
  annotate("text", x = as.Date("2020-04-07"), y = 80, 
           label = "1st SoE", color = "tomato", size = 3)+
  annotate("text", x = as.Date("2021-01-08"), y = 80, 
           label = "2nd SoE", color = "tomato", size = 3) +
  geom_point(color = "darkgoldenrod3", alpha = 0.8) + 
  geom_line(color = "darkgoldenrod3", alpha = 0.5) + 
  labs(title = "Shinjuku PM2.5 data from Apr.2016 to Feb.2021",
       subtitle = "pale gray shows State of Emergency period",
       caption = "SoE: State of Emergency",
       x = "Date", y = "PM2.5") +
  theme_minimal()
ggsave("fig/Sinjuku_pm25_SoE.png", dpi = 300)

# Post Intervention Period is filled with NA
dat_pm25_wk_causal <- dat_pm25_wk %>% 
  mutate(pm25 = replace(pm25, week >= as.Date("2020-04-07"), NA))

# Create ts zoo data
# zoo: ordered observations which includes irregular time series.
ts_pm25_wk <- zoo(dat_pm25_wk_causal$pm25, dat_pm25_wk_causal$week)

# Model fit ----
ss <- list()

# Add local trend and weekly-seasonal
ss <- AddSemilocalLinearTrend(ss, ts_pm25_wk)
# ss <- AddLocalLinearTrend(ss, ts_pm25_wk)
ss <- AddSeasonal(ss, ts_pm25_wk, nseasons = 52)

model <- bsts(ts_pm25_wk,
              state.specification = ss, 
              niter = 1500, burn = 500)
plot(model, main = "Local trend and seasonal Model")
plot(model, "components")

# Causal Impact for quarantine ----
pre.period <- as.Date(c("2016-04-04", "2020-04-07"))
post.period <- as.Date(c("2020-04-07", "2021-02-15"))

# Obtain post period data
dat_pm25_wk_causal_post <- dat_pm25_wk %>% 
  filter(week >= as.Date("2020-04-07"))

# Do CausalImpact
impact <- CausalImpact(bsts.model = model,
                       post.period.response = dat_pm25_wk_causal_post$pm25, 
                       alpha = 0.05)
plot(impact)

impact.res <- as.data.frame(impact$series)

p1 <- impact.res %>% 
  mutate(Date = as.Date(rownames(impact.res))) %>% 
  ggplot() +
  geom_rect(aes(xmin = as.Date("2020-04-07"), xmax = as.Date("2021-02-22"),
                ymin = 0, ymax = max(response) + 5), 
            fill = "gray", color = NA, alpha = 0.3) +
  geom_point(mapping = aes(x = Date, y = response),
             size = 0.5, color = "darkgoldenrod3", alpha = 0.8) +
  geom_line(mapping = aes(x = Date, y = response),
             color = "darkgoldenrod3", alpha = 0.5) + 
  annotate("text", x = as.Date("2020-04-07"), y = 80, 
           label = "Start of 1st SoE", color = "tomato", size = 3) +
  labs(title = "Causal Impact Analysis for Shinjuku PM2.5",
       subtitle = "pale gray is the period after the 1st SoE.",
       x = "Date", y = "PM2.5") +
  theme_minimal()

p2 <- impact.res %>% 
  mutate(Date = as.Date(rownames(impact.res))) %>% 
  ggplot() +
  geom_rect(aes(xmin = as.Date("2020-04-07"), xmax = as.Date("2021-02-22"),
                ymin = 0, ymax = max(response) + 5), 
            fill = "gray", color = NA, alpha = 0.3) +
  geom_point(mapping = aes(x = Date, y = response),
             size = 0.5, color = "darkgoldenrod3", alpha = 0.8) +
  geom_line(mapping = aes(x = Date, y = response),
            color = "darkgoldenrod3", alpha = 0.5) + 
  geom_point(mapping = aes(x = Date, y = point.pred),
             size = 1, color = "violetred2", alpha = 0.8) +
  geom_ribbon(aes(x = Date, ymin = point.pred.lower, ymax = point.pred.upper), 
              fill = "violetred1", alpha = 0.2) +
  labs(title = "1 step forward prediction (inc. conterfactual period)",
       x = "Date", y = "PM2.5") +
  theme_minimal()

p3 <- impact.res %>% 
  mutate(Date = as.Date(rownames(impact.res))) %>% 
  ggplot() +
  geom_rect(aes(xmin = as.Date("2020-04-07"), xmax = as.Date("2021-02-22"),
                ymin = min(point.effect.lower), 
                ymax = max(point.effect.upper)), 
            fill = "gray", color = NA, alpha = 0.3) +
  geom_point(mapping = aes(x = Date, y = point.effect),
             size = 1, color = "darkviolet", alpha = 0.8) +
  geom_ribbon(aes(x = Date, 
                  ymin = point.effect.lower, ymax = point.effect.upper), 
              fill = "darkviolet", alpha = 0.2) +
  labs(title = "pointwise causal effect",
       x = "Date", y = "PM2.5") +
  theme_minimal()

p1 / p2 / p3

ggsave("fig/Sinjuku_pm25_SoE_CausalImpact.png", 
       width = 10, height = 15, dpi = 300)

summary(impact)
summary(impact, "report")


