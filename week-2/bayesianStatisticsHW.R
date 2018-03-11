#####################################
#  LOAD LIBRARIES/DEFINE FUNCTIONS  #
#####################################

library(data.table)
library(ggplot2)
library(binom)
library(dplyr)

plotBinomialBayesFactor <- function(H0, n, x, aprior, bprior) {
    # H0 <- 0.675  # Set the point null hypothesis you want to calculate the Bayes Factor for
    # n <- 20  # set total trials
    # x <- 10  # set successes
    # aprior <- 1  # Set the alpha for the Beta distribution for the prior
    # bprior <- 1  # Set the beta for the Beta distribution for the prior
    
    alikelihood <- x + 1  # Calculate the alpha for the Beta distribution for the likelihood
    blikelihood <- n - x + 1  # Calculate the beta for the Beta distribution for the likelihood
    aposterior <- aprior + alikelihood - 1  # Calculate the alpha for the Beta distribution for the posterior
    bposterior <- bprior + blikelihood - 1  # Calculate the beta for the Beta distribution for the posterior
    
    theta <- seq(0, 1, 0.001)  # create theta range from 0 to 1
    #png(file="PriorLikelihoodPosterior.png",width=3000,height=3000, res = 500)
    
    # Calculate the density functions of the prior, likelihood, and posterior distributions
    prior <- dbeta(theta, aprior, bprior)
    likelihood <- dbeta(theta, alikelihood, blikelihood)
    posterior <- dbeta(theta, aposterior, bposterior)
    
    plot(theta, posterior, ylim=c(0, 15), type = "l", lwd = 3, xlab = bquote(theta), ylab = "Density", las = 1)
    lines(theta, prior, col="grey", lwd = 3)
    lines(theta, likelihood, lty = 2, lwd = 3, col="dodgerblue")
    BF10 <- dbeta(H0, aposterior, bposterior)/dbeta(H0, aprior, bprior)
    points(H0,dbeta(H0, aposterior, bposterior), pch = 19)
    points(H0,dbeta(H0, aprior, bprior), pch = 19, col="grey")
    segments(H0, dbeta(H0, aposterior, bposterior), H0, dbeta(H0, aprior, bprior), lty=2)
    title(paste('Bayes Factor:', round(BF10, digits=2)))
}

plotBinomialPosteriorMean <- function(n, x, aprior, bprior) {
    ymax <- 10  # set max y-axis
    
    alikelihood <- x + 1  # Calculate the alpha for the Beta distribution for the likelihood
    blikelihood <- n - x + 1  # Calculate the beta for the Beta distribution for the likelihood
    aposterior <- aprior + alikelihood - 1  # Calculate the alpha for the Beta distribution for the posterior
    bposterior <- bprior + blikelihood-  1  # Calculate the beta for the Beta distribution for the posterior
    
    theta <- seq(0, 1, 0.001)  # create theta range from 0 to 1
    #png(file="BinomialPosteriorMean.png",width=4000,height=4000, res = 500)
    
    prior <- dbeta(theta, aprior, bprior)  # deterine prior distribution
    likelihood <- dbeta(theta, alikelihood, blikelihood)  # determine likelihood distribution
    posterior <- dbeta(theta, aposterior, bposterior)  # determine posterior distribution
    plot(theta, posterior, ylim=c(0, ymax), type = "l", lwd = 3, xlab = bquote(theta), ylab = "Density", las = 1) #draw posterior distribution
    
    lines(theta, prior, col="grey", lwd = 3) #draw prior distribution
    lines(theta, likelihood, lty = 2, lwd = 3, col="dodgerblue") #draw likelihood distribution
    LL<-qbeta(.025,aposterior, bposterior) #calculate lower limit credible interval
    UL<-qbeta(.975,aposterior, bposterior) #calculate upper limit credible interval
    abline(v = aposterior/(aposterior+bposterior)) #draw line mean
    abline(v = LL, col="grey",lty=3) #draw line lower limit
    abline(v = UL, col="grey",lty=3) #draw line upper limit
    polygon(c(theta[theta < LL], rev(theta[theta < LL])), c(posterior[theta < LL], rep(0, sum(theta < LL))), col="lightgrey", border=NA)
    polygon(c(theta[theta > UL], rev(theta[theta > UL])), c(posterior[theta > UL], rep(0, sum(theta>UL))), col="lightgrey", border=NA)
    title(paste('Mean posterior:', round((aposterior/(aposterior + bposterior)), digits=5), ", 95% Credible Interval:", round(LL, digits=2), ";", round(UL, digits=2)))
    #dev.off()
}

########################
#  HOMEWORK QUESTIONS  #
########################

##  QUESTIONS #1  ##
# The true believer had a prior of Beta(1, 0.5). After observing 10 heads out of 20 coin flips, what is the posterior distribution, given that α* = α + x and β* = β + n – x?
#   A) Beta(10, 10)
#   B) Beta(11, 10.5)
#   C) Beta(10, 20)
#   D) Beta(11, 20.5)

# For this case, x = 10, n = 20, α = 1, and β = 0.5, so the posterior distribution will be Beta(α*, β*), where
#   α* = 1 + 10 = 11
#   β* = 0.5 + 20 -10 = 10.5

# Thus, the correct answer is (B) Beta(11, 10.5)
#   CHECK: CORRECT!


##  QUESTION #2  ##
# The strong skeptic had a prior of Beta(100,100). After observing 50 heads out of 100 coin flips, what is the posterior distribution, given that α* = α + x and β* = β + n – x?
#   A) Beta(50, 50)
#   B) Beta(51, 51)
#   C) Beta(150, 150)
#   D) Beta(151, 151)

# Same idea as before. Here, α = 100, β = 100, x = 50, n = 100, so we get
#   α* = α + x = 100 + 50 = 150
#   β* = β + n – x = 100 + 100 - 50 = 150

# Thus, the posterior distribution is (C) Beta(150, 150)
#   CHECK: CORRECT!


##  QUESTION #3  ##
# Change the hypothesis in the first line from 0.5 to 0.675, and run the script. If you were testing the idea that this coin returns 67.5% heads, which statement is true?
#   A) Your belief in this hypothesis, given the data, would have decreased.
#   B) Your belief in this hypothesis, given the data, would have stayed the same.
#   C) Your belief in this hypothesis, given the data, would have increased.

# The Bayes factor for H0 = 0.675 is 1.0, so if you observed 10 heads from 20 coin tosses, then (B) there would be no change in the likelihood of H0 given the data we observed
#   CHECK: CORRECT!
plotBinomialBayesFactor(H0=0.675, n=20, x=10, aprior=1, bprior=1)


##  QUESTION #4  ##
# Change the hypothesis in the first line back to 0.5. Let’s look at the increase in the belief of the hypothesis θ = 0.5 for the strong skeptic after 10 heads out of 20 coin flips. Change the α for the prior in line 4 to 100 and the β for the prior in line 5 to 100. Run the script. Compare the Figure from R to the increase in belief for the newborn (in the plot on the previous page). Which statement is true?
#   A) The belief in the hypothesis that θ = 0.5, given the data, has increased for the strong skeptic, but not as        much as it has for the newborn.
#   B) The belief in the hypothesis that θ = 0.5, given the data, has increased for the strong skeptic, exactly as        much as it has for the newborn.
#   C) The belief in the hypothesis that θ = 0.5, given the data, has increased for the strong skeptic, and much         more than it has for the newborn.
#   D) The belief in the hypothesis that θ = 0.5, given the data, has decreased for the strong skeptic.

# The Bayes factor is 1.05, so (A) belief in the hypothesis for the theta = 0.5 has increased for the skeptic, but not as much as it did for the newborn, who had a Bayes factor of 3.7
#   CHECK: CORRECT!
plotBinomialBayesFactor(H0=0.5, n=20, x=10, aprior=100, bprior=100)


##  QUESTION #5  ##
# Assume the outcome of 20 coin flips had been 18 heads. Change x to 18 in line 2 and run the script. Remember that the mean of the prior Beta(1,1) distribution is α/(α+β), or 1/(1+1) = 0.5. The Frequentist mean is simply x/n (or 18/20=0.9). Which statement is true?
#   A) The frequentist mean is higher than the mean of the posterior, because the mean of the posterior is closer        to the mean of the prior distribution.
#   B) The frequentist mean is lower than the mean of the posterior, because the mean of the posterior is closer        to the mean of the prior distribution.
#   C) The frequentist mean is higher than the mean of the posterior, because the mean of the posterior is              further from to the mean of the prior distribution.
#   D) The frequentist mean is lower than the mean of the posterior, because the mean of the posterior is further        from to the mean of the prior distribution.

n <- 20  
x <- 18  
aprior <- 1 
bprior <- 1  

# From the plot, we can clearly see that the mean posterior (gray vertical line) is 0.863, which is lower than the frequentist mean of 18/20 = 0.9. This is because (A) the mean of the posterior is closer to the mean of the prior distribution
#   CHECK: CORRECT!
plotBinomialPosteriorMean(n=n, x=x, aprior=aprior, bprior=bprior)

binom.bayes(x, n, type = "central", prior.shape1 = aprior, prior.shape2 = bprior)
binom.bayes(x, n, type = "highest", prior.shape1 = aprior, prior.shape2 = bprior)


##  QUESTION #6  ##
# What is, today, your best estimate of the probability that the sun rises every day? Assume you were born with an uniform Beta(1,1) prior. The sun can either rise, or it does not. Assume you have seen the sun every day since you were born, which means there has been a continuous string of successes for every day you have been alive. It is ok to estimate the days you have been alive by just multiplying your age by 365 days. What is your best estimate of the probability that the sun will rise?

# As of December 1, 2016, I have been alive for 8,982 days
daysAlive <- as.numeric(as.Date('2016-12-01') - as.Date('1992-04-29'))

# Thus, I have observed n = 8982, x = 8982. From a Bayesian prior of Beta(1, 1), this gives me a 99.989% chance that the Sun will rise tomrrow
#   CHECK: CORRECT!
plotBinomialPosteriorMean(n=daysAlive, x=daysAlive, aprior=1, bprior=1)


## QUESTION #7  ##
# What would have been the best estimate from a Frequentist perspective?

# The best estimate from a frequentist perspective would have been 100%
#   CHECK: CORRECT!
