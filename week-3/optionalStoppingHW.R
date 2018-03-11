########################################
#  DEFINE FUNCTIONS AND LOAD PACKAGES  #
########################################

library(data.table)
library(dplyr)

simulatePValues <- function(n, D, SD, show=TRUE) {
    # n <- 200  # total number of datapoints (per condition) you are willing to collect after initial 10
    
    # D <- 0.0  # True effect size (Keep SD below to 1, otherwise, this is just mean dif, not d)
    # SD <- 1  # Set True standard deviation.
    
    p <- numeric(n)  # store p-values
    x <- numeric(n)  # store x-values
    y <- numeric(n)  # store y-values
    
    n <- n + 10  # script calculates p-values after 10 people in each condition, so add 10 to number of datapoints
    
    for (i in 10:n) {  # for each simulated participants after the first 10
        x[i] <- rnorm(n=1, mean=0, sd=SD)
        y[i] <- rnorm(n=1, mean=D, sd=SD)
        z <- t.test(x[1:i], y[1:i], var.equal=TRUE)  # perform the t-test
        p[i] <- z$p.value 
    }
    
    p <- p[10:n]  # Remove forst 10 empty p-values
    
    if (show) {
        # Create the plot
        # png(file="p-value_over_time.png",width=4000,height=2000, , units = "px", res = 500)
        plot(0, col="red", lty=1, lwd=3, ylim=c(0,1), xlim=c(10,n), type="l", xlab='sample size', ylab='p-value', cex.lab=1, cex.axis=1, xaxt = "n")
        lines(p, lwd=2)
        abline(h=0.05, col="darkgrey", lty=2, lwd=2)  # draw ine at p = 0.05
        axis(1, at=seq(0, n-10, by=(n-10)/4), labels = seq(10, n, by=(n-10)/4))
        # dev.off()
    }

    return(p)
}

simulateOptionalStopping <- function(N, Looks, nSim, alpha, D) {
    # N <- 100  # total number of datapoints (per condition) you are willing to collect
    # Looks <- 5  # set number of looks at the data
    # nSim <- 50000  # number of simulated studies
    # alpha <- 0.05  # set alpha
    
    # D <- 0  # True effect size (must be 0 when simulating Type 1 errors)
    
    # Take care of some settings
    options(scipen=100, digits=4)  # disable scientific notation for numbers
    LookN <- ceiling(seq(0,N,N/Looks))  # Determine at which N's to look
    LookN <- LookN[-1]  # remove look at 0
    LookN <- LookN[LookN > 2]  # Remove looks at N of 1 or 2 (not possible with t-test)
    Looks <- length(LookN)  # if looks are removed, change number of looks
    matp <- matrix(NA, nrow=nSim, ncol=Looks)  # Matrix for p-values at sequential tests
    SigSeq <- numeric(Looks)  # Variable to store final p-values
    OptStop <- numeric(nSim)  # variable to store positions of optional stopping
    p <- numeric(nSim)  # Variable to save optional stopping p-values
    
    #Loop data generation for each study, then loop to perform a test for each N 
    for (i in 1:nSim){
        x<-rnorm(n = N, mean = 0, sd = 1)
        y<-rnorm(n = N, mean = D, sd = 1)
        for (j in 1:Looks){
            matp[i,j]<-t.test(x[1:LookN[j]],y[1:LookN[j]], var.equal=TRUE)$p.value #perform the t-test, store
        }
        cat('Loop', i, 'of', nSim,'\n')
    }
    
    # Save Type 1 error rate for each look
    for (i in 1:Looks){
        SigSeq[i] <- sum(matp[, i]<alpha)
    }
    
    # Get the positions at which are stopped, and then these p-values
    for (i in 1:nSim){
        OptStop[i] <- min(which(matp[i, ] < alpha))
    }
    
    OptStop[is.infinite(OptStop)] <- Looks  # If nothing significant, take last p-value (fixes error warning)
    
    for (i in 1:nSim){
        p[i] <- matp[i, OptStop[i]]
    }
    
    breaks<-100
    hist(p, breaks=breaks,col="grey")
    abline(h=nSim/breaks, col = "red", lty=3)
    
    return(list(p, SigSeq))
}


########################
#  HOMEWORK QUESTIONS  #
########################

##  QUESTION #1  ##
# Run the script 20 times, and count how often the lowest p-value ends up below 0.05 (we will calculate the long run probability of this happening through more extensive simulations later).

minP <- numeric(20)

for (i in 1:20) {
    minP[i] <- min(simulatePValues(n=200, D=0, SD=1, show=TRUE))
}

# The minimum p-value drops below 0.05 70% of the time
#   CHECK: CORRECT!
mean(minP < 0.05)


##  QUESTION #2  ##
# Change the effect size on line 3 from D <- 0.0 to D <- 0.3. This is a relatively small true effect, and with 200 participants in each condition, we have 85% power (or an 85% probability of finding a significant effect).

# Run the script 20 times. Take a good look at the variation in the p-value trajectory. Remember that at N = 200, 85% of the times the p-value should have ended up below 0.05. The script returns the sample size the p-value is the lowest (which is often, but not always, at the maximum sample size, when there is a true effect) and the sample size at which the p-value drops below 0.05 for the first time. Which statement is true?
#   A) If the p-value drops below 0.05, it stays below 0.05.
#   B) The p-value randomly moves between 0 and 1, and will every now and then end up below 0.05.
#   C) The p-value often drops below 0.05 well before 200 participants in each condition. In around 50% of the           simulations, this already happens at N = 100.
#   D) The p-value will typically move below 0.05 and stay there for some time, but given a large enough sample,         it will always move back up to p > 0.05. 

pValueMatrix <- matrix(0, nrow=201, ncol=20)

for (i in 1:20) {
    pValueMatrix[, i] <- simulatePValues(n=200, D=0.3, SD=1, show=TRUE)
}

# We observe that 45% of the time, the p-value drops below 0.05 by N = 100 and stays there for the rest of the experiment, which means that (C) is closest to what we have observed
#   CHECK: CORRECT!
apply(pValueMatrix, MARGIN=2, FUN=function(x) all(x[101:201] < 0.05)) %>% mean()


##  QUESTION #3  ##
# Change the effect size in line 3 to D <- 0.8, which can be regarded as a large effect. Run the script 20 times. Take a good look at the variation in the p-value trajectory. Which statement is true?
#   A) The p-value randomly moves between 0 and 1, and will every now and then end up below 0.05.
#   B) The p-values drop below and stay below 0.05 much earlier than when the true effect size is 0.3.
#   C) p-values are meaningful when effect sizes are large (e.g., d = 0.8), but meaningless when effect sizes are        small (e.g., d = 0.3).
#   D) When you examine a large effect, whenever a p-value drops below 0.05, it will always stay below 0.05 as the        sample size increases.

pValueMatrix <- matrix(0, nrow=201, ncol=20)

for (i in 1:20) {
    pValueMatrix[, i] <- simulatePValues(n=200, D=0.8, SD=1, show=TRUE)
}

thresholds <- apply(pValueMatrix, MARGIN=2, FUN=function(x) {
    y <- x < 0.05
    j <- 1
    
    while (!all(y[j:length(y)])) {
        j <- j + 1
    }
    
    return (j)
})

# The p-values cross the (permanent) 0.05 threshold after the 21st observation, on average, which is much earlier than they did before. Thus, (B) is the correct answer.
#   CHECK: CORRECT!
mean(thresholds)


##  QUESTION #4  ##
# Start by running the simulation without changing any values, so simulating 100 participants in each condition, looking 5 times at your data, with an alpha of 0.05. Note the 50.000 simulations take a while!

# Looking at the graph, which statement is true?
#   A) Optional stopping does not impact the Type 1 error rate.
#   B) Optional stopping inflates the Type 1 error rate. We can see this in the first five bars (p-values between        0.00 and 0.05), which are substantially higher than the horizontal line.
#   C) Optional stopping inflates the Type 1 error rate. We can see this in the bars just above 0.05, which dip         substantially below the uniform distribution that should be present if there is no true effect. 

# Looking at the plot, we can see that the bars for p-values < 0.05 are way higher than they would be under the null (the dotted line), which means that (B) optional stopping inflates the Type I error rate
#   CHECK: CORRECT!
simulateOptionalStopping(N=100, Looks=5, nSim=50000, alpha=0.05, D=0)


##  QUESTION #5  ##
# Which statement is true?
#   A) At each look, the Type 1 error rate is higher than the alpha level (0.05). When using optional stopping          (and reporting only the lowest p-value), the Type 1 error rate is higher than 0.05.
#   B) At each look, the Type 1 error rate is approximately equal to the alpha level (0.05). When using optional        stopping (and reporting only the lowest p-value), the alpha level also approximately equals the alpha            level (0.05).
#   C) At each look, the Type 1 error rate is approximately equal to the alpha level (0.05). When using optional        stopping, the Type 1 error rate is also higher than the alpha level (0.05).

# From the output (below) we can clearly see that the Type I error rate is (C) approximately equal to the alpha level, but when using optional stopping the overall Type I error rate is higher than the alpha level
#   CHECK: CORRECT!
#       "Type 1 error rates for look 1 to 5 : 0.04826 0.05044 0.0505 0.05 0.05142"
#       "Type 1 error rate when only the lowest p-value for all looks is reported: 0.1416"


##  QUESTION #6  ##
# Change the number of looks to 2 (in line 2), and leave all other settings the same. Run the simulation again. What is the Type 1 error rate using optional stopping, rounded to 2 digits? (Note that due to small number of simulations, the exact alpha level you get might differ a little bit from the answer options below).
#   A) 0.05
#   B) 0.08
#   C) 0.12
#   D) 0.18

N <- 100
Looks <- 2
nSim <- 50000
alpha <- 0.05
D <- 0.0

out <- simulateOptionalStopping(N=N, Looks=Looks, nSim=nSim, alpha=apha, D=D)

p <- out[[1]]
SigSeq <- out[[2]]

# The Type I error rate for each look is still approximately 0.05
cat("Type 1 error rates for look 1 to", Looks,":", SigSeq/nSim)

# The Type I error rate is still inflated (8%) for optional stopping but not as much as before (14%). Thus, the correct answer for the Type I (overall) error rate is (B) 8%
#   CHECK: CORRECT!
cat("Type 1 error rate when only the lowest p-value for all looks is reported:", sum(p < alpha)/nSim)


##  QUESTION #7  ##
# What is the maximum Type 1 error rate when optional stopping is used when collecting 200 participants in each condition, and looking 200 times (or 198 times, given that you can’t perform a t-test on a sample size of 1 or 2 people)? Set the number of participants to 200, the number of looks to 200, the number of simulations to 10000 (this simulation will take even longer!), and the alpha to 0.05. 

# What is maximum Type 1 error rate when collecting 200 participants in each condition of an independent t-test, using optional stopping, rounded to 2 digits? (Note that the simulation will take a while, but still, due to the relatively small number of simulations, the exact alpha level you get might differ a little bit from the answer options below – choose the answer option closest to your result).
#   A) 0.05 
#   B) 0.11 
#   C) 0.20
#   D) 0.41

N <- 200
Looks <- 200
nSim <- 10000
alpha <- 0.05
D <- 0.0

out <- simulateOptionalStopping(N=N, Looks=Looks, nSim=nSim, alpha=alpha, D=D)

p <- out[[1]]
SigSeq <- out[[2]]

# The Type I error rate for each look is now close to 25% 
cat("Type 1 error rates for look 1 to", Looks,":", SigSeq/nSim)

# The Type I error rate overall is now nearly 42%, which is closest to (D) 41%
#   CHECK: CORRECT!
cat("Type 1 error rate when only the lowest p-value for all looks is reported:", sum(p < alpha)/nSim)


##  QUESTION #8  ##
# Set the number of participants to 100, the number of looks to 5, and the number of simulations to 50000. In the Wikipedia article on the Pocock boundary, find the corrected alpha level for 5 looks at the data. Change the alpha level in the simulation to this value. Run the simulation. Which of the following statements is true?
#   A) The Type 1 error rate at each look is approximately 0.03, and the overall alpha level is about 0.05.
#   B) The Type 1 error rate at each look is approximately 0.03, and the overall alpha level is about 0.15.
#   C) The Type 1 error rate at each look is approximately 0.016, and the overall alpha level is about 0.05.
#   D) The Type 1 error rate at each look is approximately 0.016, and the overall alpha level is about 0.08.

N <- 100
Looks <- 5
nSim <- 50000
alpha <- 0.0158  # Pocock boundary for 5 looks
D <- 0.0

out <- simulateOptionalStopping(N=N, Looks=Looks, nSim=nSim, alpha=alpha, D=D)

p <- out[[1]]
SigSeq <- out[[2]]

# The Type I error rate for each look is approximately equal to the correct alpha level (1.58%)
cat("Type 1 error rates for look 1 to", Looks,":", SigSeq/nSim)

# The Type I error rate overall is now about 5%
cat("Type 1 error rate when only the lowest p-value for all looks is reported:", sum(p < alpha)/nSim)

# Thus, the Type I error rate for each look is about 0.016 and the Type I error rate overall is about 5%, so the correct answer is (C)
#   CHECK: CORRECT!


##  QUESTION #9  ##
# Look at the graph of the p-value distribution when using the Pocock boundary, and compare it to the graph you got when not using the Pocock boundary. You can flip back and forth between plots you have generated in RStudio using the blue arrows on the plots tab:
#   A) Without Pocock’s boundary, small p-values (e.g., p = 0.01) are more likely than slightly higher p-values         (p = 0.04). With Pocock’s boundary, small p-values (e.g., p = 0.01) are also more likely than slightly           higher p-values (p = 0.04).
#   B) Without Pocock’s boundary, small p-values (e.g., p = 0.01) are more likely than slightly higher p-values         (p = 0.04). With Pocock’s boundary, small p-values (e.g., p = 0.01) are less likely than slightly higher p        -values (p = 0.04).
#   C) Without Pocock’s boundary, small p-values (e.g., p = 0.01) are less likely than slightly higher p-values         (p = 0.04). With Pocock’s boundary, small p-values (e.g., p = 0.01) are more likely than slightly higher p        -values (p = 0.04).
#   D) Without Pocock’s boundary, small p-values (e.g., p = 0.01) are less likely than slightly higher p-values         (p = 0.04). With Pocock’s boundary, small p-values (e.g., p = 0.01) are also less likely than slightly           higher p-values (p = 0.04).

# From examining the plot from the previous simulation, we can clearly see that (C) is the right answer. Without Pocock's boundary, small p-values (e.g. 0.01) are less likely than slightly higher p-values (e.g. 0.04), whereas with Pocock's boundary, we see the opposite.
#   CHECK: CORRECT!