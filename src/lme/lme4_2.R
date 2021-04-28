# lmerTest JSS paper check

# Library ----
library(lme4)
library(lmerTest)

# Dataset ----
View(lmerTest::TVbo)
View(lmerTest::carrots)

# LME ----
tv.lmer <- lmer(
  formula = Sharpnessofmovement ~ TVset * Picture + (1 | Assessor) +
    (1 | Assessor:TVset) + (1 | Assessor:Picture), 
  data = TVbo
)

carrots.lmer <- lmer(
  Preference ~ sens1 + sens2 +
    (1 + sens1 + sens2 | Consumer) + (1 | Product), 
  data = carrots
)


# ANOVA type I-III ----
anova(tv.lmer, type = 1)
anova(tv.lmer, type = 2, ddf = "Satterthwaite")
anova(tv.lmer, type = 2, ddf = "Kenward-Roger")
anova(tv.lmer, type = 3)

# Summary ----
summary(carrots.lmer)
