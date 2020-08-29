# -------------------------------------------------------------------
#   0. Load Library
# -------------------------------------------------------------------
library(devtools)
install_github("fpechon/rfCountData")
library(tidyverse)
library(rfCountData)


# -------------------------------------------------------------------
#   1. Load Data
#   https://bit.ly/2BPwF89
# 
#   age: age of driver, continuous feature in {18, . . . , 90} years
#   ac: age of car, continuous feature in {0, . . . , 35} years
#   power: power of car, continuous feature in {1, . . . , 12}
#   gas: fuel type of car (diesel/regular petrol), binary feature
#   brand: brand of car, categorical feature with 11 labels
#   area: area code, categorical feature with 6 labels
#   dens: density at the living place of the driver, continuous in [1, 270000]
#   ct: Swiss canton of the car license plate, categorical with 26 labels
# -------------------------------------------------------------------

mtpl <- read_delim(
    file = "codes/rfPoisson/MTPL_data.csv",
    delim = ";",
    escape_double = FALSE,
    trim_ws = TRUE
)


# -------------------------------------------------------------------
#   2. Basic Check
# -------------------------------------------------------------------
View(mtpl)
dim(mtpl)
str(mtpl)
table(mtpl$area, mtpl$ct)
summary(mtpl)


# -------------------------------------------------------------------
#   3. rfPoisson
# -------------------------------------------------------------------
cat.features <- c("gas", "brand", "area", "ct")
for (c in cat.features) {
    mtpl[c] <- as.factor(mtpl[c][[1]])
}

features <- 
    c("age", "ac", "power", "gas", "brand", "area", "dens", "ct")



rf <- rfPoisson(
    x = mtpl[, features], 
    offset = log(mtpl$expo), 
    y = mtpl$claims, 
    ntree = 10 , mtry = 3, replace = TRUE , nodesize = 1000
    )


