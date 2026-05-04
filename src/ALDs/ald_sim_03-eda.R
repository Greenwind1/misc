# =============================================================================
# EDA: ALD simulation data
# =============================================================================
library(tidyverse)
library(extrafont)  # fonttable(); "Candara"
library(patchwork)

NAME <- "ald_simulation_03"

# -----------------------------------------------------------------------------
# 1. Load data
# -----------------------------------------------------------------------------
dat <- read_csv("input/ald_simulation_03-data.csv")


# -----------------------------------------------------------------------------
# 2. Env setting
# -----------------------------------------------------------------------------
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


# =============================================================================
# P1. Age profile for each cohort
# =============================================================================
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


# =============================================================================
# P2. Trend for average medical expenditure
# - X: observation year, Y: Average medical expenditure
# - Vertical broken lines show revision year for 
# =============================================================================
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


# =============================================================================
# P3. Frequency by Age for each cohort
# =============================================================================
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


# =============================================================================
# P4. Average incurred medical expenditure by age for each cohort
# =============================================================================
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


# =============================================================================
# P5. Histogram for medical expenditure by each cohort
# =============================================================================
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


# =============================================================================
# P6. Heatmap of average medical expenditure by obs year and age
# =============================================================================
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


# =============================================================================
# P7. Average medical expenditure for all cohorts in the same age
# =============================================================================
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


# 3-8. Frequency by observation year for each cohort ----
p8 <- dat %>%
  group_by(obs_year, birth_year) %>%
  summarise(visit_rate = mean(visited), .groups = "drop") %>%
  ggplot(aes(x = obs_year, y = visit_rate,
             color = factor(birth_year), group = factor(birth_year))) +
  geom_vline(data = revision_df, aes(xintercept = obs_year),
             linetype = "dashed", color = "gray50", alpha = 0.8) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  scale_color_manual(values = cohort_colors, name = "Cohort (birth year)") +
  scale_y_continuous(labels = scales::percent, limits = c(0, .5)) +
  scale_x_continuous(breaks = seq(2010, 2035, by = 2)) +
  labs(
    title    = "Frequency by observation year for each cohort",
    subtitle = "Vertical broken lines: revision year for medical expenditure",
    x = "Observation year", y = "Frequency"
  ) +
  theme_ald +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p8


# 3-9. Box plot of incurred medical expenditure by observation year for each cohort ----
p9 <- dat %>%
  filter(visited == 1) %>%
  ggplot(aes(x = factor(obs_year), y = medical_cost,
             fill = factor(birth_year))) +
  geom_boxplot(outlier.size = 0.5, outlier.alpha = 0.3, linewidth = 0.4) +
  facet_wrap(~ factor(birth_year), nrow = 1) +
  scale_fill_manual(values = cohort_colors, guide = "none") + 
  scale_y_continuous(labels = scales::comma, 
                     limits = c(0, quantile(dat$medical_cost[dat$visited == 1], 0.99))) +
  labs(
    title    = "Incurred medical expenditure by observation year for each cohort",
    subtitle = "Vertical broken lines: revision year | Upper 1% excluded",
    x = "Observation year", y = "Incurred medical expenditure"
  ) +
  theme_ald + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
p9


# 4. Save figures ----
fig_top <- (p1 | p2)
fig_mid <- (p3 | p8)
fig_bot <- (p4 | p9)
fig_all <- fig_top / fig_mid / fig_bot

ggsave(paste0("fig/", NAME, "-eda.jpg"), fig_all, width = 16, height = 10, dpi = 300)
