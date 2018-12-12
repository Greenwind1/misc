library(igraph)

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
