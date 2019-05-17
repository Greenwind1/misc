# feature exploration of housing data
library(mice)

# loading data
source('data_inc.R')

# transformation of prices
train.data$SalePrice <- log1p(train.data$SalePrice)

# categorical data transformation
train.data$MSSubClass <- as.factor(train.data$MSSubClass)

# imputation
na.check <- apply(is.na(train.data), 2, sum)
na.columns <- names(na.check)[na.check > 0]
rm('na.check')
summary(train.data[na.columns])


# column names including NA's
# [1] "LotFrontage"  "Alley"        "MasVnrType"   "MasVnrArea"   "BsmtQual"    
# [6] "BsmtCond"     "BsmtExposure" "BsmtFinType1" "BsmtFinType2" "Electrical"  
# [11] "FireplaceQu"  "GarageType"   "GarageYrBlt"  "GarageFinish" "GarageQual"  
# [16] "GarageCond"   "PoolQC"       "Fence"        "MiscFeature" 

p1 <- ggplot() +
    geom_histogram(mapping = aes(x = LotFrontage),
                   data = train.data,
                   bins = 100,
                   alpha = 0.3,
                   fill = brocolors('crayons')['Denim'],
                   color = brocolors('crayons')['Gray'])
ggplotly(p1)

g1 <- ggplot(data = train.data) +
    geom_point(mapping = aes(x = LotFrontage, y = SalePrice),
               color = brocolors('crayons')['Razzmatazz'],
               alpha = 0.3)
ggplotly(g1)


