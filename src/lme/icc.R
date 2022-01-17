# Library ----
library(irr)  # icc()
library(psych)  # ICC()


# Data ----
data(anxiety)


# ICC(1, 1) ----
irr::icc(anxiety, model = "oneway", type = "c")
irr::icc(anxiety, model = "twoway", type = "a")
