library(dplyr)

library(clustMD)
library(survival)

# Loading dataset: Byar and Green, 1980
# SurvStat = 1: dead from prostate cancer
# SurvStat = 0: alive
# SurvStat = 2 to 9: dead from others
data(Byar)
View(Byar)

# Preprocessing: making event indicators
Byar_analysis <- Byar |> 
  mutate(
    # Only deaths from prostate cancer are treated as events (1), 
    # all others are censored (0)
    prostate_death = ifelse(SurvStat == 1, 1, 0),
    time = Observation
  )

# Cox-PH modeling
cox_model <- coxph(
  Surv(time, prostate_death) ~ 
    Age + 
    Weight + 
    Performance.rating + 
    Cardiovascular.disease.history +
    Systolic.Blood.pressure +
    Size.of.primary.tumour +
    Index.of.tumour.stage.and.histolic.grade +
    Serum.prostatic.acid.phosphatase +
    Bone.metastases +
    Stage, 
  data = Byar_analysis
)

summary(cox_model)

confint(cox_model)

cox.zph(cox_model)

library(survminer)
fit_stage <- survfit(Surv(time, prostate_death) ~ Stage, data = Byar_analysis)
ggsurvplot(fit_stage, 
           data = Byar_analysis,
           risk.table = TRUE,
           conf.int = TRUE,
           title = "Survival curve from prostate cancer")
