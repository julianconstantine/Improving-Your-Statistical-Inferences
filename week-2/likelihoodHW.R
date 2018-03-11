####################################
#  LOAD PACKAGES/DEFINE FUNCTIONS  #
####################################

library(data.table)
library(ggplot2)
library(dplyr)

##  FUNCTION plotLikelihood  ##
plotLikelihood <- function(n, x) {
    # Theta values for plotting
    theta <- seq(from=0, to=1, len=100) 
    
    # Compute binomial likelihoods
    likelihood <- dbinom(x=x, size=n, prob=theta) 
    
    data <- data.table(theta=theta, likelihood=likelihood)
    
    p <- ggplot(data=data, aes(x=theta, y=likelihood)) + geom_line() 
    p <- p + labs(x='Theta', y='Likelihood')
    p <- p + ggtitle('Likelihood Curve')
    
    return(p)
}


########################
#  HOMEWORK QUESTIONS  #
########################

##  QUESTION #1  ##
# Let’s assume you expect this is a fair coin. What is the binomial probability of observing 8 heads out of 10 coin flips, when θ = 0.5??
#   A) 0.044
#   B) 0.05
#   C) 0.5
#   D) 0.8

H0 <- 0.5
n <- 10
x <- 8

# The probability of 8/10 heads when theta = 0.5 is (A) 0.044
#   CHECK: CORRECT!
dbinom(x=x, size=n, prob=H0)


##  QUESTION #2  ##
# The likelihood curve rises up and falls down, except at the extremes, when 0 heads or only heads are observed. Open the PlotLikelihood.R script, and plot the likelihood curves for 0 heads by changing the number of successes in line 3 to 0, and running the script. What does the likelihood curve look like?
#   A) The likelihood curve is a horizontal line.
#   B) The script returns and error message: it is not possible to plot the likelihood curve for 0 heads.
#   C) The curve starts at its highest point at θ = 0, and then the likelihood decreases as θ increases.
#   D) The curve starts at its lowest point at θ = 0, and then the likelihood increases as θ increases.

p <- plotLikelihood(x=0, n=10)

# From exaimining the plot, we can see that (C) the curve starts at its highest point at θ = 0, then decreases as θ increases.
#   CHECK: CORRECT!
print(p)


##  QUESTION #3   ##
# Get a coin out of your wallet. Flip it 13 times, and count the number of heads. Open the R file CalculateLikelihoodRatio.R to calculate the likelihood that your coin is fair, compared to the likelihood that the coin is not fair, and will give the % of heads you observed. In line 3, set the number of successes to the number of heads you observed. In line 5, change the 0 in 0/13 to the number of heads you have observed (or leave it to 0 if you didn’t observe any heads at all!). Run the script to calculate the likelihood ratio. What is the likelihood ratio of a fair compared to a non-fair coin that flips heads as often as you have observed, based on the observed data?

# Let's say the coin came up heads 4 times
n <- 13  
x <- 4  

H0 <- 0.5 
H1 <- x/n

# The likelihood ratio of fair/not fair (4/13) is 37.3%
#   CHECK: CORRECT!
dbinom(x, n, H0)/dbinom(x, n, H1) 


##  QUESTION #4  ##
# Earlier we mentioned that with increasing sample sizes, we had collected stronger relative evidence. Let’s say we would want to compare L(θ) = 0.4 with L(θ) = 0.5. What is the likelihood ratio for 5 out of 10 heads?

n <- 10
x <- 5

H0 <- 0.4
H1 <- x/n

# Likelihood ratio (H0/H1) is 0.82
dbinom(x, n, H0)/dbinom(x, n, H1) 

# Likelihood ratio (H1/H0) is 1.23
#   CHECK: CORRECT!
dbinom(x, n, H1)/dbinom(x, n, H0) 


##   QUESTION #5  ##
# What is the likelihood ratio for 50 out of 100 heads?

n <- 100
x <- 50

H0 <- 0.4
H1 <- x/n

# Likelihood ratio (H0/H1) is 0.13
dbinom(x, n, H0)/dbinom(x, n, H1) 

# Likelihood ratio (H1/H0) is 7.70
#   CHECK: CORRECT!
dbinom(x, n, H1)/dbinom(x, n, H0) 


##  QUESTION  #6  ##
# What is the likelihood ratio for 500 out of 1000 heads?

n <- 1000
x <- 500

H0 <- 0.4
H1 <- x/n

# Likelihood ratio (H0/H1) is 1.37e-9
dbinom(x, n, H0)/dbinom(x, n, H1) 

# Likelihood ratio (H1/H0) is 731,784,961
#   CHECK: CORRECT!
dbinom(x, n, H1)/dbinom(x, n, H0) 


##  QUESTION #7  ##
#When comparing two hypotheses (θ = X vs θ = Y), a likelihood ratio of:
#   A) 0.02 means that neither of the two hypotheses is very likely.
#   B) 5493 means that hypothesis θ = X is very likely to be true.
#   C) 5493 means that hypothesis θ = X is much more likely than θ = Y.
#   D) 0.02 means that the hypothesis that θ = X is 2% more likely to be true than θ = Y.

# The answer here is obviously (C), because likelihood ratios only provide relative evidence
#   CHECK: CORRECT!