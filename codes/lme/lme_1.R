library(HSAUR)
library(stringr)
library(lme4)
library(Metrics)
library(ggplot2)

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

# https://hikaru1122.hatenadiary.jp/entry/2015/12/21/203157
icc <- lmer(
  formula = bdi ~ 1 +(1 | ID),
  data = BtheB.long,
  na.action = na.omit
  )
summary(icc)
# 100.41 / (100.41 + 27.79) = 0.783 > 0.1


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

lm1 <- lm(
  bdi ~ bdi.pre + time + treatment + drug + length,
  data = BtheB.long,
)
summary(lm1)

r2 <- function(pred, actual){
  return(1 - sum((actual - pred) ^ 2) / sum((actual - mean(actual)) ^ 2))
  }

rmse(as.numeric(lm1$fitted.values),
     BtheB.long$bdi[!is.na(BtheB.long$bdi)])
r2(as.numeric(lm1$fitted.values),
   BtheB.long$bdi[!is.na(BtheB.long$bdi)])

rmse(as.numeric(predict(lmer1)),
     BtheB.long$bdi[!is.na(BtheB.long$bdi)])
r2(as.numeric(predict(lmer1)),
   BtheB.long$bdi[!is.na(BtheB.long$bdi)])

rmse(as.numeric(predict(lmer2)),
     BtheB.long$bdi[!is.na(BtheB.long$bdi)])
r2(as.numeric(predict(lmer2)),
   BtheB.long$bdi[!is.na(BtheB.long$bdi)])

data.frame(
  actual = BtheB.long$bdi[!is.na(BtheB.long$bdi)],
  lm1 = as.numeric(lm1$fitted.values),
  lmer1 = as.numeric(predict(lmer1)),
  lmer2 = as.numeric(predict(lmer2))
) %>% ggplot() +
  geom_point(mapping = aes(x = actual, y = lm1),
             color = "slategray", size = 1.5, alpha = 0.5) +
  geom_point(mapping = aes(x = actual, y = lmer1),
             color = "slateblue", size = 2, alpha = 0.7, shape = 17) +
  geom_point(mapping = aes(x = actual, y = lmer2), 
             color = "darkorange", size = 2, alpha = 0.7, shape = 18) +
  labs(x = "actual", y = "predicted", title = "Model Comparison")
