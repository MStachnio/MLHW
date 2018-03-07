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
# note: only the attribute variables are standardized
BNorm <- matrix(0.0,3,6)
BNorm[1:2,]<-(B[1:2,]-DescrStats[1:2,1])/DescrStats[1:2,2]
BNorm[3,]<-B[3,]
rownames(BNorm) <- c("x1","x2", "y")
colnames(BNorm) <- c("D1", "D2", "D3", "D4", "D5", "D6")
print(BNorm)

# Perceptron Algorithm
w0=0.5
w1=0.5
w2=0
eta=0.1
lastError =0
outerloop=3


#defining the weight matrix, which includes the information whether the prediction was successfull
weight = matrix(0,4,6*outerloop)
rownames(weight) <- c("w0","w1", "w2","pred?")


#set an outer loop to run several times over our data, the resulting weight are stored in the matrix weight
#where the first row correspond to the weight of the first loop and the second line to the weight of the
#the second loop
index = matrix(0,1,6*outerloop)

for (l in 1:outerloop){
  for (i in 1:6){
    index[i]= w0+w1*BNorm[1,i]+w2*BNorm[2,i]
    if(index[i]<0){prediction <- -1}
    else{prediction <- 1} 
    error <- BNorm[3,i]-prediction
    
    # Storage of the Weight to see the adjustement
    weight[1,i +(l-1)*6]=w0
    weight[2,i+(l-1)*6]=w1
    weight[3,i+(l-1)*6]=w2
    # Also tests whether our prediction was accurate and stores it in the 4th row of the matrix weight
    if ((index[i]<0) & (BNorm[3,i]<0) | ((index[i])>=0) & (BNorm[3,i]>=0)){weight[4,i+(l-1)*6]=1}
    else{weight[4,i+(l-1)*6]=0
    lastError=i+(l-1)*6}
    #Correction
    w0 = w0 + eta*error
    w1 = w1 + eta*error*BNorm[1,i]
    w2 = w2 + eta*error*BNorm[2,i]
  }
}




print(weight)
# Decision Boundry
intercept = -w0/w2
slope = -w1/w2

#Ploting

# Setting y<0 as red dot and y>0 as black losange
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

# If the number of outerloops was too small, and we did not test the new weights for all the data, we would like to be warned.
# Furthermore, we want to know after how many iterations we got the correct weights.
if(i+(l-1)*6 -lastError <6 ){print("Please increase the number of outerloops to make sure the weights are calibrated correctly!")
}else{print(paste("The last prediction mistake was observed after ",lastError," iterations"))}
