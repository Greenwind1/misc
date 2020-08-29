install.packages("biglasso")

library(MASS)
library(cvTools)
library(glmnet)
library(biglasso)
library(doParallel)
library(Metrics)

data("Boston")

View(Boston)


hold.out.idx <- sample(1:nrow(Boston), size = round(nrow(Boston) / 5))

train.x <- Boston[-hold.out.idx, -ncol(Boston)]
train.y <- Boston[-hold.out.idx, ncol(Boston)]
hold.out.x <- Boston[hold.out.idx, -ncol(Boston)]
hold.out.y <- Boston[hold.out.idx, ncol(Boston)]

f <- cvFolds(n = nrow(train.x), K = 10, type = "random")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#   Lasso with glmnet
#   https://glmnet.stanford.edu/articles/glmnet.html#linear-regression
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

set.seed(2020)

lasso.cv <- cv.glmnet(
    x = as.matrix(train.x), y = as.matrix(train.y), 
    type.measure = "mse",
    nfolds = 10,
    foldid = f$which,
    family = 'gaussian', 
    alpha = 1  # Lasso
    )

plot(lasso.cv)
cat(lasso.cv$lambda.min, lasso.cv$lambda.1se)

rmse(hold.out.y, 
     predict(lasso.cv, as.matrix(hold.out.x), s = "lambda.min"))
rmse(hold.out.y, 
     predict(lasso.cv, as.matrix(hold.out.x), s = "lambda.1se"))

coef(lasso.cv, s = "lambda.min")
coef(lasso.cv, s = "lambda.1se")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#   Lasso with biglasso
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

train.x.big <- as.big.matrix(train.x)
hold.out.x.big <- as.big.matrix(hold.out.x)

biglasso.cv <- cv.biglasso(
    X = train.x.big, y = train.y, 
    family = 'gaussian',
    seed = 2020, 
    nfolds = 10,
    cv.ind = f$which,
    ncores = detectCores(), 
    alpha = 1  # Lasso
    )
plot(biglasso.cv)
cat(biglasso.cv$lambda.min)

rmse(hold.out.y, predict(biglasso.cv, hold.out.x.big)[, 1])
coef(biglasso.cv)


biglasso.model <- biglasso(
    X = train.x.big, y = train.y, 
    family='gaussian', 
    ncores=detectCores(),
    alpha = 1,
    lambda = lasso.cv$lambda.min
)

rmse(hold.out.y, predict(biglasso.model, hold.out.x.big)[, 1])
coef(biglasso.model)
