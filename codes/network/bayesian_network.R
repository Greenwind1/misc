library(bnlearn)
data(marks)
head(marks)

cor(marks)

res <- rsmax2(marks)
res

plot(res)
