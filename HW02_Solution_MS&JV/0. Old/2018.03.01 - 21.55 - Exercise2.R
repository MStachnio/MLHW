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
outerloop=3
#set an outer loop to run two times over our data, the resulting weight are stored in the matrix weight
#where the first row correspond to the weight of the first loop and the second line to the weight of the
#the second loop
weight = matrix(0,4,6*outerloop)
rownames(weight) <- c("w0","w1", "w2","pred?")

for ( l in 1:outerloop){
  for (i in 1:6){
    if(w0+w1*BNorm[1,i]+w2*BNorm[2,i]<0){prediction <- -1}
    else{prediction <- 1} 
    error <- BNorm[3,i]-prediction
    #error_2(i) <- BNorm[3,i]-prediction
    #Storage of the Weight to see the adjustement 
    weight[1,(l-1)*6+i]=w0
    weight[2,(l-1)*6+i]=w1
    weight[3,(l-1)*6+i]=w2
    weight[4,(l-1)*6+i]= error
    
    #if(weight[4,i] = 0){weight[4,i] <- 1}
    #else{weight[4,i] <- 0}
    
    #Correction
    w0 = w0 + eta*error
    w1 = w1 + eta*error*BNorm[1,i]
    w2 = w2 + eta*error*BNorm[2,i]
     }
}
last_error =0
#to see for which iteration the results of the algorithm is correct (1) or wrong (0)
for( l in 1:outerloop){
for( i in 1:6){
 if(weight[4,(l-1)*6+i] == 0){ weight[4,(l-1)*6+i] <- 1 }
  else { weight[4,(l-1)*6+i] <- 0
    last_error = (l-1)*6+i
  }
}
}
if(last_error - (l-1)*6+i <6){print(please increase outerloop)}
print(weight)
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


