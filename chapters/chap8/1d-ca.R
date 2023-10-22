library(dplyr)

init_state <- function(n = 50, density = 0.5) {
  as.numeric(runif(n) < density)
}

wrap_state <- function(x) {
  c(tail(x, 1), x, x[1])
}

as_decimal <- function(b) {
  sum((2 ^ ((length(b) - 1):0)) * b)
}

as_binary <- function(n, n_bits = 8) {
  result <- c()
  for (i in 1:n_bits) {
    result <- c(n %% 2, result)
    n <- n %/% 2
  }
  result
}

generation <- function(states, rule) {
  state <- tail(states, 1) %>% c()
  n <- length(state)
  w_state <- wrap_state(state)
  result <- c()
  for (i in 1:n) {
    nbhd <- w_state[seq(i, i + 2)]
    result <- c(result, rule[as_decimal(nbhd) + 1])
  }
  matrix(c(states %>% t() %>% c(), result), nc = ncol(states), byrow = TRUE)
}

one_d_ca <- function(n = 64, t = 192, rule = 30, density = 0.5) {
  s <- init_state(n, density)
  ss <- matrix(s, nr = 1)
  for (i in 2:t) {
    ss <- generation(ss, rev(as_binary(rule)))
  }
  ss
}