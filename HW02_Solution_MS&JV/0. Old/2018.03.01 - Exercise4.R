library(matrixStats)
D1 <- c(2,3,1)
D2 <- c(3,-1,-1)
D3 <- c(1,0,-1)
D4 <- c(3,1,1)
D5 <- c(5,3,1)
D6 <- c(-1,-1,-1)

# calculating Descriptive Statistics
B <- matrix(c(D1, D2, D3, D4, D5, D6), nrow=3, ncol=6)
Means<- rowMeans(B, na.rm = FALSE, dims = 1)
StandardDev <-rowSds(B, na.rm = FALSE,dims = 1)
DescrStats <- matrix(c(Means,StandardDev), nrow=3,ncol=2)
rownames(DescrStats) <- c("x1","x2", "y")
colnames(DescrStats) <- c("Mean", "Std")
print(DescrStats)

# Normalizing the data
BNorm <- matrix(0.0,3,6)
BNorm[1:2,]<-(B[1:2,]-DescrStats[1:2,1])/DescrStats[1:2,2]
BNorm[3,]<-B[3,]
rownames(BNorm) <- c("x1","x2", "y")
colnames(BNorm) <- c("D1", "D2", "D3", "D4", "D5", "D6")
print(BNorm)

# Perceptron Algorithm
w0=0.5
w1=0.5
w2=0.5
eta=0.1

for (outerloop in 1:1){
  for (i in 1:6){
    if(w0+w1*BNorm[1,i]+w2*BNorm[2,i]<0){prediction<- -1}else{prediction<- 1} 
    error <- BNorm[3,i]-(w0+w1*BNorm[1,i]+w2*BNorm[2,i])
    w0 = w0 + eta*error
    w1 = w1 + eta*error*BNorm[1,i]
    w2 = w2 + eta*error*BNorm[2,i]
  }
}
# Decision Boundry
intercept = -w0/w2
slope = -w1/w2
# Plotting
# Important: Coloring - If I do plot(BNorm[1,], BNorm[2,], xlab="x1", ylab="x2", col=BNorm[3,]), 
# it does not work due to "-1" values that do not correspond to a color. I fixed it manually but should find a better way 
Q<- c(1,2,2,1,1,2)
plot(BNorm[1,], BNorm[2,], xlab="x1", ylab="x2", col=Q)
abline(intercept, slope, h = NULL, v = NULL, reg = NULL,
       coef = NULL, untf = FALSE)