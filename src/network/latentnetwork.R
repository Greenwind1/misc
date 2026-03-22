# ============================================================
# ANI (Approximate Neighborhood Interferece) framework 
# implementation with latentnetwork package.
#  
# Based on Leung, Michael P. 2022. 
# “Causal Inference Under Approximate Neighborhood Interference.” 
# Econometrica 90(1): 267–93. doi:10.3982/ECTA17841.
# ============================================================

# install.packages("latenetwork")
# install.packages("igraph")
library(latenetwork)
library(igraph)

# ============================================================
# Step 1: Data generation (using DGP in latentnetwork package)
# ============================================================
# datageneration() can replicate the same Monte Carlo setting in the paper.
# n     : # of units
# beta  : coefficients of linear-in-means (|β|<1 => ANI)
# sigma : standard error

set.seed(2026)
n <- 500

data <- latenetwork::datageneration(n = n)

# data
# $Y : Outcome vector (n × 1)
# $D : Treatment vector (n × 1, binary, noncompliance)
# $Z : Instrumental variable (n × 1, random allocation)
# $A : Adjacency matrix (n × n, sparse matrix)

Y <- data$Y
D <- data$D
Z <- data$Z
A <- data$A

# ============================================================
# Step 2: Interference Exposure Mapping (IEM)
# ============================================================
# Binary vector indicating 
IEM <- ifelse(A %*% Z > 0, 1, 0)
