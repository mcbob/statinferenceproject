# Illustrate via simulation and associated explanatory text the properties of
# the distribution of the mean of 40 exponential(0.2)s.  You should 
# 1. Show where the distribution is centered at and compare it to the 
# theoretical center of the distribution. 
# 2. Show how variable it is and compare it to the theoretical variance of the 
# distribution. 
# 3. Show that the distribution is approximately normal. 
# 4. Evaluate the coverage of the confidence interval for
# 1/lambda: X¯±1.96S√n. (This only needs to be done for the specific value of
# lambda).

set.seed(2222)
nosim <- 1000
n <- 40
lambda <- 0.2
means <- apply(matrix(rexp(nosim*n,lambda),nosim,n), 1, mean) 
#1 for application on rows

# Some descriptive statistics
summary(means)
sd(means)*sqrt(n)

# Plot of convergence
plot(cumsum(means)/(1:nosim), type = "l", lwd = 2, main = expression(
    paste("Convergence to population mean ", frac(1,lambda)," = ",5)),
    xlab = "Number of simulations", ylab = "Mean")
abline(h = 1/lambda, col = "blue")


# Histogram with normal curve, inspired by 
# http://www.statmethods.net/graphs/density.html
h<-hist(means, breaks=59, col="green", 
        xlab=expression(paste("Means of 40 rexp(",lambda,") observations")), 
        main="Histogram with Normal Curve, Mean and Median") 
xfit<-seq(min(means),max(means),length=59) 
yfit<-dnorm(xfit,mean=mean(means),sd=sd(means)) 
yfit <- yfit*diff(h$mids[1:2])*length(means) 
lines(xfit, yfit, col="blue", lwd=2)
abline(v = 1/lambda, col = "black", lwd = 2)
abline(v = mean(means), col = "navy", lwd = 2, lty = "dashed")
abline(v = median(means), col = "red", lwd = 2, lty = "dashed")
legend("topright", col = c("blue", "black", "navy", "red"), 
       lty = c("solid", "solid", "dashed", "dashed"), lwd = 2,
       legend = c("Normal distribution", "Population mean",
                  "Sample mean", "Sample median"))

# Computation of confidence intervals
set.seed(2222)
nosim <- 1000
n <- 40
lambda <- 0.2
simulations <- matrix(rexp(nosim*n,lambda),nosim,n)
means <- apply(simulations, 1, mean) 
S <- apply(simulations, 1, sd)  
ll <- means - 1.96 * S/sqrt(n)
ul <- means + 1.96 * S/sqrt(n)
# Fraction of confidence intervals containing the true mean 1/lambda
mean(ll < 1/lambda & 1/lambda < ul )