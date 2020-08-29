library(foreach)
library(doParallel)
# library(iterators)

# https://qiita.com/hoxo_m/items/04903cbbe9d023f0ed6d

foreach(i = seq(1, 3), .combine = "c") %do% sqrt(i)
foreach(i = seq(1, 3), .combine = "cbind") %do% sqrt(i)
foreach(i = seq(1, 3), .combine = "rbind") %do% sqrt(i)
foreach(i = seq(1, 10), .combine = "c", .inorder = F) %do% i^2


cores <- detectCores(logical=FALSE)
cluster <- makeCluster(cores - 1)
registerDoParallel(cluster)

X <- iris$Sepal.Length

foreach(i = X, .combine = "c", .inorder = T) %dopar% {
    i^2
}

foreach (mu = seq_len(8L)) %dopar% {
    rnorm(3L, mu)
}

stopCluster(cluster)
