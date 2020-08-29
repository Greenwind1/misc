library(igraph)
library(linkcomm)

test <-
  matrix(c(0, 0, 1, 0, 1, 0, 0, 0, 2, 0, 0, 0, 4, 3, 0, 0), 4, 4)

colnames(test) <- c('A', 'B', 'C', 'D')
rownames(test) <- c('A', 'B', 'C', 'D')

test.g <- graph.adjacency(test, mode = 'directed', weighted = TRUE)

V(test.g)
E(test.g)
E(test.g)$weight

## plot.igraph
plot(test.g)
plot(test.g,
     layout = layout.circle,
     edge.label = E(test.g)$weight)

vcount(test.g)
ecount(test.g)
neighbors(test.g, 'A')
mean(degree(test.g))

graph.density(test.g)
closeness(test.g)
betweenness(test.g)
page.rank(test.g)
bonpow(test.g)
transitivity(test.g)
transitivity(test.g, type = 'local')

g <- graph(c(1, 2, 2, 3, 3, 1, 1, 4), directed = FALSE)
plot(g)
assortativity.degree(g)

library(igraphdata)
library(gridExtra)
data("foodwebs")

food.df <- data.frame(
  'ID' = c(1:3), 
  'v' = rep(NaN, 3),
  'e' = rep(NaN, 3), 
  'd_mean' = rep(NaN, 3),
  'density' = rep(NaN, 3),
  'close_mean' = rep(NaN, 3)
  )

op <- par(mfrow = c(1, 3))
for (i in 1:3) {
  plot(
    foodwebs[[i]],
    layout = layout.kamada.kawai,
    vertex.size = 2,
    edge.arrow.size = 0.1,
    vertex.label.color = 'deeppink',
    vertex.label.cex = 0.9
  )
  food.df['v'][i, ] <- vcount(foodwebs[[i]])
  food.df['e'][i, ] <- ecount(foodwebs[[i]])
  food.df['d_mean'][i, ] <- mean(degree(foodwebs[[i]]))
  food.df['density'][i, ] <- graph.density(foodwebs[[i]])
  # food.df['close_mean'][i, ] <- mean(closeness(foodwebs[[i]]))
}
food.df
par(op)

g21 <- graph.difference(foodwebs[[2]], foodwebs[[1]])
plot(g21, 
     vertex.size = 10,
     edge.arrow.size = 0.5,
     vertex.label.color = 'deeppink',
     vertex.label.cex = 0.7)

data(karate)
data <- get.data.frame(karate)
glc <- getLinkCommunities(network = data, hcmethod = "ward.D")
