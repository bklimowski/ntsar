library(tidyverse)
library(magrittr)
library(janitor)

ts <- cumsum(rnorm(1000, 1, 1))/100 + sin(rnorm(1000,4,100))
emb_dim <- 4
t_lag <- 1

patterns_prob <-
  map(1:(length(ts) - emb_dim),
    ~(ts[seq(.x, .x + emb_dim * t_lag - 1, t_lag)] %>% rank)) %>%
  map(~invoke(str_c,.x)) %>%
  flatten_chr() %>%
  tabyl() %>%
  .$n



perm_patterns <- function(ts, emb_dim, t_lag) {
  map(1:(length(ts) - emb_dim),
      ~(ts[seq(.x, .x + emb_dim * t_lag - 1, t_lag)] %>% rank)) %>%
    map(~invoke(str_c,.x)) %>%
    flatten_chr() %>%
    tabyl() %>%
    return()
}

permutation_entropy <- function(ts, emb_dim, t_lag) {
  perm_patterns(ts, emb_dim, t_lag) %>%
    .$n %>%
    divide_by(sum(.)) %>%
    shannon_entropy() %>%
    multiply_by((1/log(factorial(emb_dim)))) %>%
    return()
}

permutation_entropy(ts, emb_dim, t_lag)

shannon_entropy <- function(p) {
  return(-sum(p*log(p)))
}

kl_div <- function(p,q) {
  return(-sum(p*log(q/p)))
}

js_div <- function(p,q) {
  m <- (p+q)/2
  return((kl_div(p,m) + kl_div(q,m))/2)
}

ordinal_irreversibility <- function(ts, emb_dim, t_lag) {

  pattern_probs <-
    left_join(perm_patterns(ts, emb_dim, t_lag),
              perm_patterns(rev(ts), emb_dim, t_lag),
              by = '.')

  js_div(pattern_probs$percent.x,
         pattern_probs$percent.y) %>%
    return()
}




