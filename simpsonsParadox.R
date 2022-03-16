# Simpson's paradox: non-causal correlation
# URL: https://en.wikipedia.org/wiki/Simpson%27s_paradox
# Date: Mi, 16.03.2022

library(tidyverse)

asd <-
  tibble::tibble(
    x = rnorm(n = 200),
    y = rnorm(n = 200)
  )

asd %>%
  ggplot2::ggplot(data = ., mapping = aes(x = x, y = y)) +
  geom_point() +
  geom_abline(slope = -1.0, intercept = 1.0)

asd %>%
  cor()

asd %>%
  dplyr::filter(.data = ., x + y > 1.0) %>%
  ggplot2::ggplot(data = ., mapping = aes(x = x, y = y)) +
  geom_point() +
  geom_abline(slope = -1.0, intercept = 1.0)

asd %>%
  dplyr::filter(.data = ., x + y > 1.0) %>%
  cor()
