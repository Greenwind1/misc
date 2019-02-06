library(HSAUR)
library(stringr)
library(lme4)

# Beating the Blues program...
# drug : did the patient take anti-depressant drugs (No or Yes).
# length : the length of the current episode of depression.
# treatment : treatment, TAU (treatment as usual) and BtheB (Beat the Blues)
# bdi.pre : Beck Depression Inventory II before treatment.
# bdi.2m : Beck Depression Inventory II after two months.
# bdi.4m : Beck Depression Inventory II after four months.
# bdi.6m : Beck Depression Inventory II after six months.
# bdi.8m : Beck Depression Inventory II after eight months.
data("BtheB")
View(BtheB)

BtheB['ID'] <- factor(rownames(BtheB))
BtheB.long <-
  reshape(BtheB, 
          varying = c("bdi.2m", "bdi.4m", "bdi.6m", "bdi.8m"),
          idvar = 'ID', 
          direction = 'long')
BtheB.long['time'] <- 
  apply(BtheB.long['time'],
        MARGIN = 1,
        function(x){as.integer(str_split(x, 'm')[[1]][1])})

# random effect on intercept
lmer1 <- lmer(
  bdi ~ bdi.pre + time + treatment + drug + length + (1 | ID),
  data = BtheB.long,
  na.action = na.omit
)

# random effect on intercept and time
lmer2 <- lmer(
  bdi ~ bdi.pre + time + treatment + drug + length + (time | ID),
  data = BtheB.long,
  na.action = na.omit
)

anova(lmer1, lmer2)

summary(lmer1)
