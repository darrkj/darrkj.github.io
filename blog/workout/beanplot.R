sd(rpois(1000, 10))




x <- 1
d <- rpois(10000, x) - rpois(10000, x)
hist(d)






library(psych) 
library(beanplot)

var1 <- rpois(1000, x) - rpois(1000, x)
var2 <- round(rnorm(1000, 1))
mydata<-data.frame(var1,var2) 
table(mydata)

par(lend = 1, mai = c(0.8, 0.8, 0.5, 0.5))
beanplot(var1 ~ var2, data= mydata,  side = "both",log="", 
         what=c(1,1,1,0), border = NA, col = list("black", c("grey", "white")))
legend("bottomleft", fill =c("black", "grey"), legend = c("no", "yes"))
