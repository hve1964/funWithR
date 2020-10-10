################################################################################
# Samuelson's problem
#     Effect of aggregation of independent simple lotteries (--> CLT)
# Source: Kahneman (2011): 336-340
# Do, 22.11.2018
################################################################################

# clear memory
graphics.off()
rm(list = ls())

# stakes [$]
win <- 200
lose <- -100

# Bernoulli parameter
probability <- 1 / 2
# no. of of independent and identical repetitions of coin tosses
#   (--> binomial distribution)
nTosses <- 100

# no. of independent and identical repetitions of Samuelson's experiment
k <- 1e5

# Samuelson's experiment
samuelsonExperiment <- function () {
  tosses <- rbinom(n = nTosses , size = 1 , prob = probability)
  
  outcome <- c(rep(0 , length(tosses)))
  
  for (i in 1:length(tosses)) {
    if (tosses[i] == 1) {
      outcome[i] <- win
    }
    else {
      outcome[i] <- lose
    }
  }
  
  output <- cumsum(outcome)
  return(output)
}

# sampling: matrix of outcomes
values <- replicate(n = k , expr = samuelsonExperiment())

# sampling distribution of sample mean
hist(
  x = values[nTosses, ] ,
  breaks = 1e2 ,
  freq = FALSE ,
  main = "Samuelson's experiment" ,
  xlab = "Total win [$]" ,
  ylab = "Relative frequency [1/$]"
)
mtext(paste0("Sampling distribution of sample mean from" , k , "repetitions"))
mean(values[nTosses, ])
sd(values[nTosses, ])

# probability of losing
mean(values[nTosses, ] < 0)

################################################################################
################################################################################
