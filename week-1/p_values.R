##  WHICH P-VALUES CAN YOU EXPECT?  ##
# This file contains my further investigations and some plots

library(ggplot2)
library(data.table)
library(dplyr)
library(pwr)

gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
}

# Disable scientific notation
options(scipen=999)

# Number of simulations
num_sims <- 100000

true_mean <- 106
N_values <- c(15, 26, 51, 100)
sigma <- 15

# Initialize empty vector to store p-values
P <- matrix(0, nrow=num_sims, ncol=length(N_values))
colnames(P) <- paste0('n', N_values)

for (i in 1:num_sims) {
    for (j in seq_along(N_values)) {
        N <- N_values[j]
        
        x <- rnorm(n=N, mean=true_mean, sd=sigma)
        z <- t.test(x, mu=100)
        P[i, j] <- z$p.value
    }
}

data <- as.data.table(P)
colors <- gg_color_hue(n=4)

###########################
#  CREATE AND SAVE PLOTS  #
###########################

##  PLOT FOR N = 15  ##
p <- ggplot(data=select(data, value=n15), aes(x=cut(value, breaks=seq(0, 1, 0.05), labels=1:20))) + geom_bar(fill=colors[1])
p <- p + scale_x_discrete(
    name='p-value',
    breaks=1:20,
    labels=paste0(seq(0, 0.95, 0.05), '-', seq(0.05, 1, 0.05))
)
p <- p + scale_y_continuous(
    name='Frequency'
)
p <- p + theme(axis.text.x = element_text(angle=-60, vjust=0.5))
p <- p + ggtitle("p-Value Distribution for N = 15")
p

ggsave(filename='week-1/p_value_distn_15.png', plot=p)

##  PLOT FOR N = 26  ##
p <- ggplot(data=select(data, value=n26), aes(x=cut(value, breaks=seq(0, 1, 0.05), labels=1:20))) + geom_bar(fill=colors[2])
p <- p + scale_x_discrete(
    name='p-value',
    breaks=1:20,
    labels=paste0(seq(0, 0.95, 0.05), '-', seq(0.05, 1, 0.05))
)
p <- p + scale_y_continuous(
    name='Frequency'
)
p <- p + theme(axis.text.x = element_text(angle=-60, vjust=0.5))
p <- p + ggtitle("p-Value Distribution for N = 26")
p

ggsave(filename='week-1/p_value_distn_26.png', plot=p)

##  PLOT FOR N = 51  ##
p <- ggplot(data=select(data, value=n51), aes(x=cut(value, breaks=seq(0, 1, 0.05), labels=1:20))) + geom_bar(fill=colors[3])
p <- p + scale_x_discrete(
    name='p-value',
    breaks=1:20,
    labels=paste0(seq(0, 0.95, 0.05), '-', seq(0.05, 1, 0.05))
)
p <- p + scale_y_continuous(
    name='Frequency'
)
p <- p + theme(axis.text.x = element_text(angle=-60, vjust=0.5))
p <- p + ggtitle("p-Value Distribution for N = 51")
p

ggsave(filename='week-1/p_value_distn_51.png', plot=p)

##  PLOT FOR N + 100  ##
p <- ggplot(data=select(data, value=n100), aes(x=cut(value, breaks=seq(0, 1, 0.05), labels=1:20))) + geom_bar(fill=colors[4])
p <- p + scale_x_discrete(
    name='p-value',
    breaks=1:20,
    labels=paste0(seq(0, 0.95, 0.05), '-', seq(0.05, 1, 0.05))
)
p <- p + scale_y_continuous(
    name='Frequency'
)
p <- p + theme(axis.text.x = element_text(angle=-60, vjust=0.5))
p <- p + ggtitle("p-Value Distribution for N = 100")
p

ggsave(filename='week-1/p_value_distn_100.png', plot=p)
