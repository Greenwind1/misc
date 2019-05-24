library(tidyverse)
library(CASdatasets)

data("usmassBI2")
View(usmassBI2)
str(usmassBI2)

AutoClaimIn <- usmassBI2 %>% filter(YEAR < 1998)

AutoClaimIn %>% ggplot() +
  geom_point(mapping = aes(
    x = YEAR, y = AC, color = TOWNCODE
  ), cex = 3) +
  geom_line(mapping = aes(
    x = YEAR, y = AC, color = TOWNCODE
  ))

AutoClaimIn %>% ggplot() +
  geom_point(mapping = aes(
    x = log(PCI), y = AC, color = TOWNCODE
  ))
AutoClaimIn %>% ggplot() +
  geom_point(mapping = aes(
    x = log(PPSM), y = AC, color = TOWNCODE
  ))

Pool.fit <- lm(formula = AC ~ log(PCI) + log(PPSM) + factor(YEAR),
               data = AutoClaimIn)
anova(Pool.fit)

FE.fit <- lm(
  formula = AC ~ factor(TOWNCODE) + log(PCI) + log(PPSM) + factor(YEAR) - 1, 
  data = AutoClaimIn
)
summary(FE.fit)
anova(FE.fit)

anova(Pool.fit, FE.fit)
anova(FE.fit, Pool.fit)  # Inverted
resid(Pool.fit)


# Compound Symmetry, AR(1), Unstructured
library(nlme)
SCex.fit <- gls(model = AC ~ log(PCI) + log(PPSM) + factor(YEAR),
                data = AutoClaimIn,
                correlation = corCompSymm(form = ~ 1 | TOWNCODE))
summary(SCex.fit)
intervals(SCex.fit)
getVarCov(SCex.fit)

SCex.fit <- gls(model = AC ~ log(PCI) + log(PPSM) + factor(YEAR) + TOWNCODE,
                data = AutoClaimIn,
                correlation = corCompSymm(form = ~ 1 | TOWNCODE))
summary(SCex.fit)
intervals(SCex.fit)
getVarCov(SCex.fit)
