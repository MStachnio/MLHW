setwd("~/Desktop/MBF Second Semester/Machine Learning/HW3")
library("ggplot2")


x1 = (-20:20)
#x2 = (-20:20)

w0= 1
w1= -2
w2= 2


x2 = -(w0/w2)-(w1/w2)*x1

size = 1


plot (x1, x2, type="l", asp=1, col="blue", lwd=2, xlim=c(-3,3), ylim=c(-4,4),xaxt='n',yaxt='n', axes=FALSE,
      ylab='x2',xlab='x1' , cex.lab =1) 
pranel.first = grid()
arrows (0,0,w1,0,length=0.05, col="darkgreen", lwd=2)
arrows (w1,0,w1,w2,length=0.05, col="gray28", lwd=2)
arrows (-w1*2, -w2*2, size*w1, size*w2, col="red", length=0.1, lwd=2 )
arrows (0,0,0,(-w0/w2), col="orange", length=0.0, lwd=4)
axis(1, pos = 0, cex.axis = 0.5 )
axis(2, pos = 0, cex.axis = 0.5, las = 2)
legend("topleft", legend = c("x2", "w1","w2","Vector w1,w2","Offset"),lwd = 2, col = c("blue", "darkgreen","gray28","red",
                                                                                       "orange"), cex=0.6)