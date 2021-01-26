################################################################################
# author: Henk van Elst
# file: simpleTimeSeriesModels.R
# date: Sa, 03.10.2020 / Do, 08.10.2020 / So, 24.01.2021 / Di, 26.01.2021
# description: Generation of simple time series: white noise and AR(1), with
#   special case random walk
# Cf.: https://rc2e.com/timeseriesanalysis
################################################################################

graphics.off()
rm(list=ls())

#-------------------------------------------------------------------------------
## Load packages
#-------------------------------------------------------------------------------
library(tidyverse)
library(xts)
library(urca)

#-------------------------------------------------------------------------------
## Specify parameter values
#-------------------------------------------------------------------------------
n <- 250
alpha <- 0.75
sigma <- 2.0

#-------------------------------------------------------------------------------
## Initialise synthetic time series
#-------------------------------------------------------------------------------
time <- Sys.Date() + 0:(n - 1)
y1 <- y2 <- y3 <- y4 <- c(rep(x = 0, times = n))

#-------------------------------------------------------------------------------
## Generate white noise
#-------------------------------------------------------------------------------
# Gaussian white noise
w <-
  rnorm(n = n ,
        mean = 0 ,
        sd = sigma)

# alternative: Gossetian white noise (df = 2)
#w <-
#  rt(n = n , df = 2)

#-------------------------------------------------------------------------------
## Generate synthetic time series
#-------------------------------------------------------------------------------
for (i in 2:n) {
  
  # white noise
  y1[i] <- w[i]
  
  # AR(1): negative coeff, 0 < absVal < 1
  y2[i] <- -alpha * y2[i - 1] + w[i]
  
  # AR(1): random walk
  y3[i] <- y3[i - 1] + w[i]
  
  # AR(1): positive coeff, 0 < absVal < 1
  y4[i] <- alpha * y4[i - 1] + w[i]
}

# Collect data in xts object
y_xts <- xts(x = t(rbind(y1, y2, y3, y4)) ,
             order.by = time)

#-------------------------------------------------------------------------------
# Definition: lines representing quantiles of N(0, sigma^2), scaled by sqrt(t)
#-------------------------------------------------------------------------------
quantileLineNorm <- function(x,
                             sign = 1,
                             prob = 0.89,
                             sigma = 1.0) {
  t1 <- (x - Sys.Date())
  t2 <- as.numeric(t1)
  out <- sign * qnorm(p = prob, mean = 0, sd = sigma) * sqrt(t2)
  return(out)
}

#-------------------------------------------------------------------------------
## Visualisation
#-------------------------------------------------------------------------------
plot.xts(
  x = y_xts ,
  main = "Simple time series models" ,
  xlab = "time" ,
  ylab = "displacement"
)

# https://stackoverflow.com/questions/38597464/multiple-time-series-in-ggplot2-from-xts-object
mseries <-
  cbind(y_xts$y1 ,
        y_xts$y2 ,
        y_xts$y3 ,
        y_xts$y4)
names(mseries) <-
  c("white noise" ,
    "negative alpha AR_1" ,
    "random walk" ,
    "positive alpha AR_1")
index(mseries) <-
  as.Date(index(mseries))

broom::tidy(x = mseries) %>%
  ggplot(data = .,
         mapping = aes(x = index, y = value, colour = series)) +
  geom_line(size = 1.0) +
  labs(title = "Simple time series models") +
  xlab(label = "time [days]") +
  ylab(label = "displacement [1]") +
  stat_function(
    fun = quantileLineNorm,
    args = list(sign = 1,
                prob = 0.89,
                sigma = sigma),
    linetype = "dashed",
    colour = "red"
  ) +
  stat_function(
    fun = quantileLineNorm,
    args = list(
      sign = -1,
      prob = 0.89,
      sigma = sigma
    ),
    linetype = "dashed",
    colour = "red"
  ) +
  stat_function(
    fun = quantileLineNorm,
    args = list(sign = 1,
                prob = 0.97,
                sigma = sigma),
    linetype = "dotdash",
    colour = "red"
  ) +
  stat_function(
    fun = quantileLineNorm,
    args = list(
      sign = -1,
      prob = 0.97,
      sigma = sigma
    ),
    linetype = "dotdash",
    colour = "red"
  ) +
  theme_bw()

#-------------------------------------------------------------------------------
## Autocorrelation and partial autocorrelation
#-------------------------------------------------------------------------------
# white noise
acf(x = y_xts$y1, main = "ACF: white noise")
pacf(x = y_xts$y1, main = "PACF: white noise")

# AR(1): negative coeff, 0 < absVal < 1
acf(x = y_xts$y2, main = "ACF: AR(1), negative coeff, 0 < absVal < 1")
pacf(x = y_xts$y2, main = "PACF: AR(1), negative coeff, 0 < absVal < 1")

# AR(1): random walk
acf(x = y_xts$y3, main = "ACF: AR(1), random walk")
pacf(x = y_xts$y3, main = "PACF: AR(1), random walk")

# AR(1): positive coeff, 0 < absVal < 1
acf(x = y_xts$y4, main = "ACF: AR(1), positive coeff, 0 < absVal < 1")
pacf(x = y_xts$y4, main = "PACF: AR(1), positive coeff, 0 < absVal < 1")

#-------------------------------------------------------------------------------
## Stationarity tests
#-------------------------------------------------------------------------------
coredata(y_xts) %>%
  apply(
    X = .,
    MARGIN = 2,
    FUN = ur.df,
    type = "none",
    #lags = 32 ,
    selectlags = "AIC"
  ) %>%
  map(function(urca.element) {
    return(
      tibble(
        critical_values = as.data.frame(urca.element@cval) %>%
          filter(row_number() == 1),
        test_statistic = urca.element@teststat
      )
    )
  }) %>%
  map(function(results_table) {
    results_table %>%
      transmute(
        .data = .,
        `0.01` = .$critical_values$`1pct`,
        `0.05` = .$critical_values$`5pct`,
        `0.10` = .$critical_values$`10pct`,
        teststat = .$test_statistic
      )
  }) %>%
  bind_rows(.id = "time_series")

coredata(y_xts) %>%
  apply(
    X = .,
    MARGIN = 2,
    FUN = ur.kpss,
    type = "mu" ,
    lags = "long"
  ) %>%
  map(function(urca.element) {
    return(
      tibble(
        critical_values = as.data.frame(urca.element@cval) %>%
          filter(row_number() == 1),
        test_statistic = urca.element@teststat
      )
    )
  }) %>%
  map(function(results_table) {
    results_table %>%
      transmute(
        .data = .,
        `0.01` = .$critical_values$`1pct`,
        `0.05` = .$critical_values$`5pct`,
        `0.10` = .$critical_values$`10pct`,
        teststat = .$test_statistic
      )
  }) %>%
  bind_rows(.id = "time_series")

#-------------------------------------------------------------------------------
## Generate synthetic lag-1-difference time series
#-------------------------------------------------------------------------------
y_diff_xts <- diff(x = y_xts, lag = 1) %>%
  na.omit(object = .)

#-------------------------------------------------------------------------------
## Visualisation
#-------------------------------------------------------------------------------
plot.xts(
  x = y_diff_xts ,
  main = "Simple lag-1-difference time series models" ,
  xlab = "time" ,
  ylab = "lag-1-difference displacement"
)

# https://stackoverflow.com/questions/38597464/multiple-time-series-in-ggplot2-from-xts-object
mseries2 <-
  cbind(y_diff_xts$y1 ,
        y_diff_xts$y2 ,
        y_diff_xts$y3 ,
        y_diff_xts$y4)
names(mseries2) <-
  c("white noise lag 1" ,
    "negative alpha AR_1 lag 1" ,
    "random walk lag 1" ,
    "positive alpha AR_1 lag 1")
index(mseries2) <-
  as.Date(index(mseries2))

broom::tidy(x = mseries2) %>%
  ggplot(data = .,
         mapping = aes(x = index, y = value, colour = series)) +
  geom_line(size = 1.0) +
  labs(title = "Simple lag-1-difference time series models") +
  xlab(label = "time [days]") +
  ylab(label = "lag-1-difference displacement [1]") +
  stat_function(
    fun = quantileLineNorm,
    args = list(sign = 1,
                prob = 0.89,
                sigma = sigma),
    linetype = "dashed",
    colour = "red"
  ) +
  stat_function(
    fun = quantileLineNorm,
    args = list(
      sign = -1,
      prob = 0.89,
      sigma = sigma
    ),
    linetype = "dashed",
    colour = "red"
  ) +
  stat_function(
    fun = quantileLineNorm,
    args = list(sign = 1,
                prob = 0.97,
                sigma = sigma),
    linetype = "dotdash",
    colour = "red"
  ) +
  stat_function(
    fun = quantileLineNorm,
    args = list(
      sign = -1,
      prob = 0.97,
      sigma = sigma
    ),
    linetype = "dotdash",
    colour = "red"
  ) +
  theme_bw()

#-------------------------------------------------------------------------------
## Autocorrelation and partial autocorrelation
#-------------------------------------------------------------------------------
# white noise
acf(x = y_diff_xts$y1,
    na.action = na.pass,
    main = "ACF: lag-1-difference white noise")
pacf(x = y_diff_xts$y1,
     na.action = na.pass,
     main = "PACF: lag-1-difference white noise")

# AR(1): negative coeff, 0 < absVal < 1
acf(x = y_diff_xts$y2,
    na.action = na.pass,
    main = "ACF: lag-1-difference AR(1), negative coeff, 0 < absVal < 1")
pacf(x = y_diff_xts$y2,
     na.action = na.pass,
     main = "PACF: lag-1-difference AR(1), negative coeff, 0 < absVal < 1")

# AR(1): random walk
acf(x = y_diff_xts$y3,
    na.action = na.pass,
    main = "ACF: lag-1-difference AR(1), random walk")
pacf(x = y_diff_xts$y3,
     na.action = na.pass,
     main = "PACF: lag-1-difference AR(1), random walk")

# AR(1): positive coeff, 0 < absVal < 1
acf(x = y_diff_xts$y4,
    na.action = na.pass,
    main = "ACF: lag-1-difference AR(1), positive coeff, 0 < absVal < 1")
pacf(x = y_diff_xts$y4,
     na.action = na.pass,
     main = "PACF: lag-1-difference AR(1), positive coeff, 0 < absVal < 1")

#-------------------------------------------------------------------------------
## Stationarity tests
#-------------------------------------------------------------------------------
coredata(y_diff_xts) %>%
  apply(
    X = .,
    MARGIN = 2,
    FUN = ur.df,
    type = "none",
    #lags = 32 ,
    selectlags = "AIC"
  ) %>%
  map(function(urca.element) {
    return(
      tibble(
        critical_values = as.data.frame(urca.element@cval) %>%
          filter(row_number() == 1),
        test_statistic = urca.element@teststat
      )
    )
  }) %>%
  map(function(results_table) {
    results_table %>%
      transmute(
        .data = .,
        `0.01` = .$critical_values$`1pct`,
        `0.05` = .$critical_values$`5pct`,
        `0.10` = .$critical_values$`10pct`,
        teststat = .$test_statistic
      )
  }) %>%
  bind_rows(.id = "time_series")

coredata(y_diff_xts) %>%
  apply(
    X = .,
    MARGIN = 2,
    FUN = ur.kpss,
    type = "mu" ,
    lags = "long"
  ) %>%
  map(function(urca.element) {
    return(
      tibble(
        critical_values = as.data.frame(urca.element@cval) %>%
          filter(row_number() == 1),
        test_statistic = urca.element@teststat
      )
    )
  }) %>%
  map(function(results_table) {
    results_table %>%
      transmute(
        .data = .,
        `0.01` = .$critical_values$`1pct`,
        `0.05` = .$critical_values$`5pct`,
        `0.10` = .$critical_values$`10pct`,
        teststat = .$test_statistic
      )
  }) %>%
  bind_rows(.id = "time_series")

################################################################################
################################################################################