#load or install ggplot package
if (!require (ggplot2)) {
    install.packages('ggplot2')
}

library (ggplot2)

# Simulate one group
n <- 10  # set sample size
x <- rnorm(n = n, mean = 100, sd = 15)  # create sample from normal distribution


# plot data
ggplot(as.data.frame(x), aes(x))  + 
    geom_histogram(colour="black", fill="grey", aes(y=..density..), binwidth=2) +
    stat_function(fun=dnorm, args=c(mean=100, sd=15), size=1, color="red", lty=2) +
    #  geom_density(fill=NA, colour="black", size = 1) +
    xlab("IQ") + ylab("number of people") + ggtitle("Data") + theme_bw(base_size=20) + 
    theme(panel.grid.major.x = element_blank(), axis.text.y = element_blank(), panel.grid.minor.x = element_blank()) + 
    geom_vline(xintercept=mean(x), colour="gray20", linetype="dashed") + 
    coord_cartesian(xlim=c(50, 150)) + scale_x_continuous(breaks=c(50, 60, 70, 80, 90, 100, 110, 120, 130, 140, 150)) +
    annotate("text", x = mean(x), y = 0.02, label = paste("Mean = ", round(mean(x)), "\n", "SD = ", round(sd(x)), sep=""), size=8)

#? Daniel Lakens, 2016. 
# This work is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License. https://creativecommons.org/licenses/by-nc-sa/4.0/


##  QUESTION #1  ##
# What is the difference in IQ points between the highest and the lowest mean from your 10 simulations?
set.seed(1337)

n <- 10
means <- numeric(length=10)

for (i in 1:10) {
    x <- rnorm(n = n, mean = 100, sd = 15)
    means[i] <- mean(x)
}

# Maximum difference between highest/lowest means is 10.50319
max(means) - min(means)


##  QUESTION #2  ##
# The variability in the means is determined by the standard deviation of the measurement. In real life, the standard deviation can be reduced by for example using multiple and reliable measurements (which is why an IQ test has not just one question, but many different questions). It’s not always possible to reduce the standard deviation in the real world, but it is possible in our simulation. Change the sd = 15 in line 7 to sd = 2. And simulate 10 new samples. What happens?
#   A) There is no difference between the means when sd = 15 compared to when sd = 2.
#   B) There is no difference between the standard deviations when sd = 15 compared to when sd = 2.
#   C) With sd = 2, the variation in means has increased substantially compared to sd = 15.
#   D) With sd = 2, the variation in means has decreased substantially compared to sd = 15.
n <- 10
means <- numeric(length=10)

for (i in 1:10) {
    x <- rnorm(n = n, mean = 100, sd = 2)
    means[i] <- mean(x)
}

# Maximum difference between highest/lowest means is now 2.229, so (D) the variation in means has decreased compared to sd=15
max(means) - min(means)

##  QUESTION #3  ##
# Simulate at least 10 samples with n = 10, and 10 samples with n = 100. Look at the distribution of the data you have simulated. Which statement below is true?
#   A) With small samples, it is very clear the data does not come from a population where IQ scores are normally        distributed.
#   B) With small samples, it is very clear the data comes from a population where IQ scores are normally                distributed.
#   C) The data always come from a population where IQ scores are normally distributed, but this is very difficult        to see, especially when n = 10, but sometimes also when n = 100.
#   D) The data always come from a population where IQ scores are normally distributed. This can easily be seen          when we compare the true population normal distribution (the red dotted line) against the data.

n <- 100
means <- numeric(length=10)

for (i in 1:10) {
    x <- rnorm(n = n, mean = 100, sd = 2)
    means[i] <- mean(x)
}

# Maximum difference between highest/lowest means is now 0.7256
max(means) - min(means)

ggplot(as.data.frame(x), aes(x))  + 
    geom_histogram(colour="black", fill="grey", aes(y=..density..), binwidth=2) +
    stat_function(fun=dnorm, args=c(mean=100, sd=15), size=1, color="red", lty=2) +
    #  geom_density(fill=NA, colour="black", size = 1) +
    xlab("IQ") + ylab("number of people") + ggtitle("Data") + theme_bw(base_size=20) + 
    theme(panel.grid.major.x = element_blank(), axis.text.y = element_blank(), panel.grid.minor.x = element_blank()) + 
    geom_vline(xintercept=mean(x), colour="gray20", linetype="dashed") + 
    coord_cartesian(xlim=c(50, 150)) + scale_x_continuous(breaks=c(50, 60, 70, 80, 90, 100, 110, 120, 130, 140, 150)) +
    annotate("text", x = mean(x), y = 0.02, label = paste("Mean = ", round(mean(x)), "\n", "SD = ", round(sd(x)), sep=""), size=8)


# The answer is obviously (C)


##  QUESTION #4  ##

