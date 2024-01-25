# Richard McElreath: The four elemental confounds
# Lecture 5: Elemental Confounds, Statistical Rethinking 2023

library(tidyverse)
# 1. The Fork ----
n <- 1000L

## Discrete case ----
tib_fork <-
  tibble::tibble(
    Z = stats::rbinom(
      n = n,
      size = 1L,
      prob = 0.5
    ),
    X = stats::rbinom(
      n = n,
      size = 1L,
      prob = (1 - Z) * 0.1 + Z * 0.9
    ),
    Y = stats::rbinom(
      n = n,
      size = 1L,
      prob = (1 - Z) * 0.1 + Z * 0.9
    )
  ) %>%
  dplyr::relocate(
    .data = .,
    Z,
    .after = Y
  )

### Association X and Y: not stratified by Z ----
table(tib_fork$X, tib_fork$Y)
stats::cor(x = tib_fork$X, y = tib_fork$Y)

### Association X and Y: stratified by Z ----
table(tib_fork$X[tib_fork$Z == 0], tib_fork$Y[tib_fork$Z == 0])
stats::cor(x = tib_fork$X[tib_fork$Z == 0], y = tib_fork$Y[tib_fork$Z == 0])

table(tib_fork$X[tib_fork$Z == 1], tib_fork$Y[tib_fork$Z == 1])
stats::cor(x = tib_fork$X[tib_fork$Z == 1], y = tib_fork$Y[tib_fork$Z == 1])

## Continuous case ----
cols <- c(4, 2)
N <- 300L
Z <-
  stats::rbinom(
    n = N,
    size = 1L,
    prob = 0.5
  )
X <-
  stats::rnorm(
    n = N,
    mean = (2 * Z - 1),
    sd = 1.0
  )
Y <-
  stats::rnorm(
    n = N,
    mean = (2 * Z - 1),
    sd = 1.0
  )
plot(
  x = X,
  y = Y,
  col = cols[Z + 1],
  lwd = 3,
  main = "The Fork: X <- Z -> Y"
)
abline(lm(Y[Z == 1] ~ X[Z == 1]), col = 2, lwd = 3)
abline(lm(Y[Z == 0] ~ X[Z == 0]), col = 4, lwd = 3)
abline(lm(Y ~ X), lwd = 3)

# 2. The Pipe ----

## Discrete case ----
tib_pipe <-
  tibble::tibble(
    X = stats::rbinom(
      n = n,
      size = 1L,
      prob = 0.5
    ),
    Z = stats::rbinom(
      n = n,
      size = 1L,
      prob = (1 - X) * 0.1 + X * 0.9
    ),
    Y = stats::rbinom(
      n = n,
      size = 1L,
      prob = (1 - Z) * 0.1 + Z * 0.9
    )
  ) %>%
  dplyr::relocate(
    .data = .,
    Z,
    .after = Y
  )

### Association X and Y: not stratified by Z ----
table(tib_pipe$X, tib_pipe$Y)
stats::cor(x = tib_pipe$X, y = tib_pipe$Y)

### Association X and Y: stratified by Z ----
table(tib_pipe$X[tib_pipe$Z == 0], tib_pipe$Y[tib_pipe$Z == 0])
stats::cor(x = tib_pipe$X[tib_pipe$Z == 0], y = tib_pipe$Y[tib_pipe$Z == 0])

table(tib_pipe$X[tib_pipe$Z == 1], tib_pipe$Y[tib_pipe$Z == 1])
stats::cor(x = tib_pipe$X[tib_pipe$Z == 1], y = tib_pipe$Y[tib_pipe$Z == 1])

## Continuous case ----
X <-
  stats::rnorm(n = N)
Z <-
  stats::rbinom(
    n = N,
    size = 1L,
    prob = (1 / (1 + exp(x = -X)))
  )
Y <-
  stats::rnorm(
    n = N,
    mean = (2 * Z - 1),
    sd = 1.0
  )
plot(
  x = X,
  y = Y,
  col = cols[Z + 1],
  lwd = 3,
  main = "The Pipe: X -> Z -> Y"
)
abline(lm(Y[Z == 1] ~ X[Z == 1]), col = 2, lwd = 3)
abline(lm(Y[Z == 0] ~ X[Z == 0]), col = 4, lwd = 3)
abline(lm(Y ~ X), lwd = 3)

# 3. The Collider ----

## Discrete case ----
tib_coll <-
  tibble::tibble(
    X = stats::rbinom(
      n = n,
      size = 1L,
      prob = 0.5
    ),
    Y = stats::rbinom(
      n = n,
      size = 1L,
      prob = 0.5
    ),
    Z = stats::rbinom(
      n = n,
      size = 1L,
      prob = ifelse(
        test = X + Y > 0.0,
        yes = 0.9,
        no = 0.2
      )
    )
  )

### Association X and Y: not stratified by Z ----
table(tib_coll$X, tib_coll$Y)
stats::cor(x = tib_coll$X, y = tib_coll$Y)

### Association X and Y: stratified by Z ----
table(tib_coll$X[tib_coll$Z == 0], tib_coll$Y[tib_coll$Z == 0])
stats::cor(x = tib_coll$X[tib_coll$Z == 0], y = tib_coll$Y[tib_coll$Z == 0])

table(tib_coll$X[tib_coll$Z == 1], tib_coll$Y[tib_coll$Z == 1])
stats::cor(x = tib_coll$X[tib_coll$Z == 1], y = tib_coll$Y[tib_coll$Z == 1])

## Continuous case ----
X <-
  stats::rnorm(n = N)
Y <-
  stats::rnorm(n = N)
Z <-
  stats::rbinom(
    n = N,
    size = 1L,
    prob = (1 / (1 + exp(x = -(2 * X + 2 * Y - 2))))
  )

plot(
  x = X,
  y = Y,
  col = cols[Z + 1],
  lwd = 3,
  main = "The Collider: X -> Z <- Y"
)
abline(lm(Y[Z == 1] ~ X[Z == 1]), col = 2, lwd = 3)
abline(lm(Y[Z == 0] ~ X[Z == 0]), col = 4, lwd = 3)
abline(lm(Y ~ X), lwd = 3)

# 4. The Descendant ----

## Discrete case ----
tib_desc <-
  tibble::tibble(
    X = stats::rbinom(
      n = n,
      size = 1L,
      prob = 0.5
    ),
    Z = stats::rbinom(
      n = n,
      size = 1L,
      prob = (1 - X) * 0.1 + X * 0.9
    ),
    Y = stats::rbinom(
      n = n,
      size = 1L,
      prob = (1 - Z) * 0.1 + Z * 0.9
    ),
    A = stats::rbinom(
      n = n,
      size = 1L,
      prob = (1 - Z) * 0.1 + Z * 0.9
    )
  ) %>%
  dplyr::relocate(
    .data = .,
    Z,
    .after = Y
  )

### Association X and Y: not stratified by A ----
table(tib_desc$X, tib_desc$Y)
stats::cor(x = tib_desc$X, y = tib_desc$Y)

### Association X and Y: stratified by A ----
table(tib_desc$X[tib_desc$A == 0], tib_desc$Y[tib_desc$A == 0])
stats::cor(x = tib_desc$X[tib_desc$A == 0], y = tib_desc$Y[tib_desc$A == 0])

table(tib_desc$X[tib_desc$A == 1], tib_desc$Y[tib_desc$A == 1])
stats::cor(x = tib_desc$X[tib_desc$A == 1], y = tib_desc$Y[tib_desc$A == 1])
