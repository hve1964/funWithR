################################################################################
# A plot of 250 random walks
# July 22, 2011 [Mo, 22.10.2018]
# By ALT: https://www.r-bloggers.com/2011/07/a-plot-of-250-random-walks/
# Mo, 22.10.2018
################################################################################

graphics.off()
rm(list = ls())

# Generate k random walks across time {0, 1, ... , T}
T <- 100
k <- 250
initial.value <- 10

GetRandomWalk <- function() {
  # Add a standard normal at each step
  initial.value + c(0, cumsum(x = rnorm(n = T)))
}

# Matrix of random walks
values <- replicate(n = k , expr = GetRandomWalk())

# Create an empty plot
dev.new(height = 8 , width = 12)
plot(
  x = 0:T ,
  y = rep(NA, T + 1) ,
  main = sprintf("%s random walks", k) ,
  xlab = "time [time unit]" ,
  ylab = "value [distance unit]" ,
  ylim = 10 + 4.5 * c(-1, 1) * sqrt(x = T)
)
mtext(sprintf(
  "%s%s}, with initial value of %s" ,
  "across time {0, 1, ... , " ,
  T ,
  initial.value
))
for (i in 1:k) {
  lines(x = 0:T , y = values[, i] , lwd = 0.25)
}
for (sign in c(-1, 1)) {
  curve(
    expr = initial.value + sign * 1.96 * sqrt(x) ,
    from = 0 ,
    to = T ,
    n = 2 * T ,
    col = "red" ,
    lty = 2 ,
    lwd = 2 ,
    add = TRUE
  )
}
legend(
  "topright" ,
  "+/- 1.96 * sqrt(time)" ,
  bty = "n" ,
  lty = 2 ,
  lwd = 2 ,
  col = "red"
)
savePlot("random_walks.png")

################################################################################
################################################################################
