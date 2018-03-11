library(data.table)
library(ggplot2)
library(dplyr)

# Compare Beta distributions to binomial distributions

theta <- seq(from=0, to=1, len=100)

n <- 10
k <- 6

# Theta values
data <- data.table(theta=theta)

# Generate binomial density
data$binomial <- dbinom(x=k, size=n, prob=theta)

# Generate (unnormalized) beta density
data$beta <- dbeta(theta, shape1=k+1, shape2=n-k+1)

# Now generate normalized data for beta distribution (divide through by n+1)
data[, beta_norm := beta/(n+1)]

plotdata <- melt(data, id.vars='theta')

# Plot the binomial vs unnormalized beta densities
# The two densities are very different; they have the same shape but beta always takes on higher values
ggplot(data=plotdata[!grepl('norm', variable)], aes(x=theta, y=value, color=variable)) + geom_line()

# Plot the binomial vs unnormalized beta densities
# Now the normalized beta distribution and binomial distributions plot directly on top of each other 
ggplot(data=plotdata[variable %in% c('binomial', 'beta_norm')], aes(x=theta, y=value, color=variable)) + geom_line()


