# http://www.lessrstats.com/

library(lessR)


n1 <- 19
m1 <- 9.57
s1 <- 1.45
# ss1 <- n1 * s1 ^ 2
ss1 <- (n1 - 1) * s1 ^ 2  # lessR::tt.brief assumes `unbiased std`!

n2 <- 15
m2 <- 8.09
s2 <- 1.59
# ss2 <- n2 * s2 ^ 2
ss2 <- (n2 - 1) * s2 ^ 2

tt.brief(n1 = n1, m1 = m1, s1 = s1, n2 = n2, m2 = m2, s2 = s2, 
         alternative = "less")
# Hypothesis Test of 0 Mean Diff:  t-value = 2.832,  df = 32,  p-value = 0.004

t <- (m1 - m2) / sqrt((1 / n1 + 1 / n2) * ((ss1 + ss2) / (n1 + n2 - 2)))
pt(t, df = n1 + n2 - 2, lower.tail = F)

