# note survival analysis 2

library(MASS)
library(survival)
library(survminer)

# Step 1: Load Data ----
data(gehan)

# Step 2: Create a survival object ----
surv.obj <- Surv(time = gehan$time, event = gehan$cens)

# Step 3: Create survival curves ----
ge.sf <- survfit(surv.obj ~ treat, data = gehan)

# Step 4: Create a summary dataframe for survival curves ----
ge.sf.df <- surv_summary(ge.sf, data = gehan)
View(ge.sf.df)

# Step 5: Plot ggplot-style survival curves ----
ggsurvplot(
    fit = ge.sf,
    data = gehan,
    conf.int = T,
    pval = T,
    risk.table = T,
    cumevents = T,
    cumcensor = T,
    ggtheme = ggplot2::theme_light(),
    tables.height = 0.15
)
