# =============================================================================
#  EDA: ALD simulation data
# =============================================================================
library(tidyverse)
library(extrafont)  # fonttable(); "Candara"
library(patchwork)

# 1. Load data ----
NAME <- "ald_simulation_04"
dat <- read_csv(paste0("input/", NAME, "-data.csv"))


# 2. Env setting ----
source("utility/environments.R")


# REVISION_YEARS
if (!exists("REVISION_YEARS")) {
  REVISION_YEARS <- c(2012, 2014, 2016, 2018, 2020)
}
revision_df <- data.frame(obs_year = REVISION_YEARS)

# common theme for ggplot
font.base <- "Times New Roman"
theme_ald <- theme_minimal(base_family = font.base, base_size = 13) + 
  theme(
    legend.position  = "bottom",
    strip.background = element_rect(fill = "gray95"),
    panel.grid.minor = element_blank()
  )

# palette
cohort_colors <- setNames(
  RColorBrewer::brewer.pal(5, "Set2"),
  as.character(sort(unique(dat$birth_year)))
)


# 3-1. Age profile for each cohort ----
p1 <- dat %>%
  group_by(birth_year, age) %>%
  summarise(mean_cost = mean(medical_cost), .groups = "drop") %>%
  ggplot(aes(x = age, y = mean_cost,
             color = factor(birth_year), group = factor(birth_year))) +
  geom_line(linewidth = 1.1, alpha = 0.85) +
  scale_color_manual(values = cohort_colors, name = "Cohort (birth year)") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title    = "Age profile for average medical expenditure by cohort",
    x = "Age", y = "Average medical expenditure"
  ) +
  theme_ald
p1


# 3-2. Trend for average medical expenditure ----
# - X: observation year, Y: Average medical expenditure
# - Vertical broken lines show revision year for 
p2 <- dat %>%
  group_by(obs_year, birth_year) %>%
  summarise(mean_cost = mean(medical_cost), .groups = "drop") %>%
  ggplot(aes(x = obs_year, y = mean_cost,
             color = factor(birth_year), group = factor(birth_year))) +
  geom_vline(data = revision_df, aes(xintercept = obs_year),
             linetype = "dashed", color = "gray50", alpha = 0.8) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  scale_color_manual(values = cohort_colors, name = "Cohort (birth year)") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = seq(2010, 2035, by = 2)) +
  labs(
    title    = "Trend for average medical expenditure",
    subtitle = "Vertical broken lines: revision year for medical expenditure",
    x = "Observation year", y = "Average medical expenditure"
  ) +
  theme_ald +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p2


# 3-3. Frequency by Age for each cohort ----
p3 <- dat %>% 
  group_by(birth_year, age) %>%
  summarise(visit_rate = mean(visited), .groups = "drop") %>%
  ggplot(aes(x = age, y = visit_rate,
             color = factor(birth_year), group = factor(birth_year))) +
  geom_line(linewidth = 1.1, alpha = 0.85) +
  scale_color_manual(values = cohort_colors, name = "Cohort (birth year)") + 
  scale_y_continuous(labels = scales::percent, limits = c(0, .5)) +
  labs(
    title    = "Frequency by age for each cohort",
    x = "Age", y = "Frequency"
  ) +
  theme_ald
p3


# 3-4. Average incurred medical expenditure by age for each cohort ----
p4 <- dat %>%
  filter(visited == 1) %>%
  group_by(birth_year, age) %>%
  summarise(mean_cost_pos = mean(medical_cost), .groups = "drop") %>%
  ggplot(aes(x = age, y = mean_cost_pos,
             color = factor(birth_year), group = factor(birth_year))) +
  geom_line(linewidth = 1.1, alpha = 0.85) +
  scale_color_manual(values = cohort_colors, name = "Cohort (birth year)") + 
  scale_y_continuous(labels = scales::comma) +
  labs(
    title    = "Incurred medical expenditure by age for each cohort",
    x = "Age", y = "Average incurred medical expenditure"
  ) +
  theme_ald
p4


# 3-5. Histogram for medical expenditure by each cohort ----
p5 <- dat %>%
  filter(visited == 1) %>%
  ggplot(aes(x = medical_cost, fill = factor(birth_year))) +
  geom_histogram(bins = 60, alpha = 0.7, position = "identity") +
  facet_wrap(~ factor(birth_year), nrow = 1, labeller = label_both) +
  scale_fill_manual(values = cohort_colors, guide = "none") +
  scale_x_continuous(labels = scales::comma, 
    limits = c(0, quantile(dat$medical_cost[dat$visited==1], 0.99))) + 
  labs(
    title    = "Histogram for medical expenditure by each cohort",
    x = "Medical expenditure", y = ""
  ) +
  theme_ald +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p5


# 3-6. Heatmap of average medical expenditure by obs year and age ----
p6 <- dat %>%
  group_by(obs_year, age) %>%
  summarise(mean_cost = mean(medical_cost), .groups = "drop") %>%
  ggplot(aes(x = obs_year, y = age, fill = mean_cost)) +
  geom_tile() +
  geom_vline(data = revision_df, aes(xintercept = obs_year), 
             color = col.os, linewidth = 0.6, alpha = 0.9) + 
  scale_fill_viridis_c(option = "plasma", labels = scales::comma,
                       name = "Average medical expenditure") +
  scale_x_continuous(breaks = seq(2010, 2035, by = 2)) +
  labs(
    title    = "Heatmap of average medical expenditure by obs year and age",
    x = "Observation year", y = "Age"
  ) + 
  theme_ald +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p6


# 3-7. Average medical expenditure for all cohorts in the same age ----
overlap_summary <- dat %>%
  group_by(age) %>%
  summarise(
    n_cohorts     = n_distinct(birth_year),
    mean_cost     = mean(medical_cost),
    sd_cost       = sd(medical_cost),
    .groups       = "drop"
  )

p7 <- ggplot(overlap_summary, aes(x = age)) +
  geom_col(aes(y = n_cohorts), fill = "steelblue", alpha = 0.4, width = 0.8) +
  geom_line(
    data = overlap_summary %>% 
      mutate(scaled = mean_cost / max(mean_cost) * max(n_cohorts)),
    aes(y = scaled), color = "tomato", linewidth = 1.2
  ) +
  scale_y_continuous(
    name     = "Num of cohorts (distinct birth year)", 
    sec.axis = sec_axis(
      ~ . * max(overlap_summary$mean_cost) / max(overlap_summary$n_cohorts),
      name   = "Average medical expenditure for all cohorts",
      labels = scales::comma
    )
  ) +
  labs(
    title = "Average medical expenditure for all cohorts in the same age",
    x = "Age"
  ) +
  theme_ald
p7


# 4. Save figures ----
fig_top <- (p1 | p2)
fig_bot <- (p3 | p4)
fig_all <- fig_top / fig_bot

ggsave(paste0("fig/", NAME, "-eda.jpg"), fig_all, width = 16, height = 10, dpi = 300)
