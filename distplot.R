distplot <- function(x){
        xx <- seq(mean(x)-3*sd(x),mean(x)+3*sd(x), length.out = 100)
        beta <- var(x)/mean(x)
        alpha <- mean(x)/beta
        
        par(mfrow = c(2, 2))
        qqnorm(x)
        qqline(x)
        
        plot(xx,dnorm(xx,mean(x),sd(x)),type='l', ylab = "f(X)", main = "Normal Distribution")
        points(x,rep(0,length(x)))

        plot(xx,dgamma(xx,shape= alpha,scale= beta),type='l', ylab = "f(X)", main = "Gamma Distribution")
        points(x,rep(0,length(x)))
        
        hist(x)

}