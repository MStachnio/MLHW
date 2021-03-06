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
w2=0.0
eta=0.1

#set an outer loop to run two times over our data, the resulting weight are stored in the matrix weight
#where the first row correspond to the weight of the first loop and the second line to the weight of the
#the second loop
weight = matrix(0,3,6)
weight2 = matrix(0,3,3)
rownames(weight2) <- c("w0", "w1","w2")
colnames(weight2) <- c("1","2", "3")

for ( l in 1:3){
  for (i in 1:6){
    if(w0+w1*BNorm[1,i]+w2*BNorm[2,i]<0){prediction <- -1}
    else{prediction <- 1} 
    error <- BNorm[3,i]-(w0+w1*BNorm[1,i]+w2*BNorm[2,i])
    
    #Storage of the Weight to see the adjsutement 
    weight[1,i]=w0
    weight[2,i]=w1
    weight[3,i]=w2
    
    #Correction
    w0 = w0 + eta*error
    w1 = w1 + eta*error*BNorm[1,i]
    w2 = w2 + eta*error*BNorm[2,i]
  }
  weight2[1,l]=w0
  weight2[2,l]=w1
  weight2[3,l]=w2
}

print(weight)
print(weight2)
# Decision Boundry
intercept = -w0/w2
slope = -w1/w2

#Ploting

# Setting y<0 as red dote and y>0 as black losange
color = rep(0, ncol(BNorm))
symb = rep(0, ncol(BNorm))
for (j in 1:ncol(BNorm)){
  if(BNorm[3,j]>0)
  {color[j]=1 
  symb[j]=18}
  else{color[j]=2
  symb[j]=19}
}

plot(BNorm[1,], BNorm[2,], xlab="x1", ylab="x2", col =color,pch= symb )

abline(intercept, slope, h = NULL, v = NULL, reg = NULL,
       coef = NULL, untf = FALSE)


