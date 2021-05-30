################################################################################
# The Number of Times a Coin Needs to Be Flipped Until Head Appears
#   URL: https://math.stackexchange.com/questions/2738638/the-number-of-times-a-coin-needs-to-be-flipped-until-head-appears
# Expected value of the number of flips until the first head
#   URL: https://math.stackexchange.com/questions/1196452/expected-value-of-the-number-of-flips-until-the-first-head
# Date: So, 30.05.2021
################################################################################

# Specify probability for success
prob_for_success <- 0.5

# Specify number of trials
number_of_trials <- 1e6

tosses_til_head <- 0 # An empty vector

for(sim in 1:number_of_trials) {
  # Number of simulations: ...

  i <- 1 # Counter
  repeat {
    if (rbinom(n = 1, size = 1, prob = prob_for_success) == 0) {
      # Coin toss with Boolean (if "Tails" ...)
      i <- i + 1
    } # ... increase the counter by 1.
    else {
      break
    } # If "Heads", stop!
  }
  
  # Collect the number of tosses before we got "Heads".
  tosses_til_head[sim] <- i
}

mean(x = tosses_til_head) # Average of these ... simulations.

################################################################################
################################################################################