library(matrixStats)
library(rgl)
D1 <- c(1,1,1,1)
D2 <- c(1,-1,-1,-1)
D3 <- c(-1,1,-1,-1)
D4 <- c(-1,-1,1,1)

# calculating Descriptive Statistics
B <- matrix(c(D1, D2, D3, D4), nrow=4, ncol=4)
Means<- rowMeans(B, na.rm = FALSE, dims = 1)
StandardDev <-rowSds(B, na.rm = FALSE,dims = 1)
DescrStats <- matrix(c(Means,StandardDev), nrow=4,ncol=2)
rownames(DescrStats) <- c("x1","x2","x3", "y")
colnames(DescrStats) <- c("Mean", "Std")
print(DescrStats)

# Normalizing the data not necessary

# Perceptron Algorithm
w0=0.5
w1=0.5
w2=0.5
w3=0
eta=0.1
lastError=0
outerloop=2
dataLength = ncol(B)


#defining the weight matrix, which includes the information whether the prediction was successfull
weight = matrix(0,5,dataLength*outerloop)
rownames(weight) <- c("w0","w1", "w2","w3","pred?")


#set an outer loop to run several times over our data, the resulting weight are stored in the matrix weight
#where the first row correspond to the weight of the first loop and the second line to the weight of the
#the second loop
index = matrix(0,1,dataLength*outerloop)

for (l in 1:outerloop){
  for (i in 1:4){
    index[i]= w0+w1*B[1,i]+w2*B[2,i]+w3*B[3,i]
    if(index[i]<0){prediction <- -1}
    else{prediction <- 1} 
    error <- B[4,i]-prediction
    
    # Storage of the Weight to see the adjustement
    weight[1,i +(l-1)*dataLength]=w0
    weight[2,i+(l-1)*dataLength]=w1
    weight[3,i+(l-1)*dataLength]=w2
    weight[4,i+(l-1)*dataLength]=w3
    # Also tests whether our prediction was accurate and stores it in the 4th row of the matrix weight
    if ((index[i]<0) & (B[4,i]<0) | ((index[i])>=0) & (B[4,i]>=0)){weight[5,i+(l-1)*dataLength]=1}
    else{weight[4,i+(l-1)*dataLength]=0
    lastError=i+(l-1)*dataLength}
    #Correction
    w0 = w0 + eta*error
    w1 = w1 + eta*error*B[1,i]
    w2 = w2 + eta*error*B[2,i]
    w3 = w3 + eta*error*B[3,i]
  }
}




print(weight)
# Decision Boundry
intercept = -w0/w2
slope = -w1/w2

#Ploting

# Setting y<0 as red dot and y>0 as black losange
color = rep(0, ncol(B))
symb = rep(0, ncol(B))
for (j in 1:ncol(B)){
  if(B[4,j]>0)
  {color[j]=1 
  symb[j]=18}
  else{color[j]=2
  symb[j]=19}
}

plot3d(B[1,], B[2,], B[3,], xlab="x1", ylab="x2", zlab="x3", col =color,pch= symb , type ="s")
planes3d(-(w0/w3),-(w1/w3),-(w2/w3), color="black",alpha =0.1,add=T)
#png(filename="/Users/Michal/Dropbox/UNISG/14. Machine Learning/3. Homework/HW03_Solution_MS&JV/Exercise3.png")
rgl.postscript("separatingplane.pdf","pdf")

# If the number of outerloops was too small, and we did not test the new weights for all the data, we would like to be warned.
# Furthermore, we want to know after how many iterations we got the correct weights.
if(i+(l-1)*dataLength - lastError <dataLength ){print("Please increase the number of outerloops to make sure the weights are calibrated correctly!")
}else{print(paste("The last prediction mistake was observed after ",lastError," iterations"))}
