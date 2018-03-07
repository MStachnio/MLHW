# install.packages('matrixStats')
library(matrixStats)

# manually insterting data
D1 <- c(12,-2,-1)
D2 <- c(21,-1,2)
D3 <- c(17,1.2,-2)
D4 <- c(10,0,1)

# calculating Descriptive Statistics
B <- matrix(c(D1, D2, D3, D4), nrow=3, ncol=4)
Means<- rowMeans(B, na.rm = FALSE, dims = 1)
StandardDev <-rowSds(B, na.rm = FALSE,dims = 1)
DescrStats <- matrix(c(Means,StandardDev), nrow=3,ncol=2)
rownames(DescrStats) <- c("x1","x2", "x3")
colnames(DescrStats) <- c("Mean", "Std")
print(DescrStats)

# Normalizing the data
BNorm <-(B-Means)/StandardDev
rownames(BNorm) <- c("x1","x2", "x3")
colnames(BNorm) <- c("D1", "D2", "D3", "D4")
print(BNorm)


