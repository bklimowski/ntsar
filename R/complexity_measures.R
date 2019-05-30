# ts <- cumsum(rnorm(1000,0,10))
#
#
# porta_index <- function(ts, t_embd = 1) {
#   ts_diff <- diff(ts, lag = t_embd)
#   (length(ts_diff[ts_diff<0])/length(ts_diff[ts_diff!=0])) %>%
#     return()
# }
#
# guzik_index <- function(ts, t_embd = 1) {
#   ts_diff <- diff(ts, lag = t_embd)
#   ((ts_diff[ts_diff>=0]^2 %>% sum)/(ts_diff^2 %>% sum)) %>%
#     return()
# }
#
# poincare_plot <- function(ts, t_embd = 1) {
#   tibble(x = ts[1:(length(ts)-t_embd)],
#          y = ts[(t_embd+1):length(ts)]) %>%
#     ggplot(aes(x,y)) +
#     geom_point() %>%
#     return()
# }
#
# ehler_index <- function(ts, t_embd = 1) {
#   diff_ts <- diff(ts, lag = t_embd)
#
#   sum(diff_ts^3)/((sum(diff_ts^2))^(3/2)) %>%
#     return()
# }
#
# shannon_entrophy <- function(p) {
#   return(-sum(p*log(p)))
# }


ts <- rnorm(1000, 1, 3)
emb_dim <- 4
t_lag <- 1
r <- sd(ts)


apen_c <- function(ts, emb_dim, t_lag, r) {
  division <-
    map(1:(length(ts) - emb_dim),
        ~(ts[seq(.x, .x + emb_dim * t_lag - 1, t_lag)])) %>%
    invoke(rbind, .)


  map(1:nrow(division),
      ~(division %>%
          sweep(2, division[.x,]) %>%
          abs() %>%
          is_less_than(r) %>%
          apply(1, all) %>%
          sum %>%
          subtract(1))) %>%
    flatten_dbl() %>%
    divide_by((length(ts) - emb_dim + 1)) %>%
    log() %>%
    sum() %>%
    divide_by((length(ts) - emb_dim + 1)) %>%
    return()
}

apen_c(ts, emb_dim, t_lag, 0.2) - apen_c(ts, emb_dim+1, t_lag, 0.2)


sampen_c <- function(ts, emb_dim, t_lag, r) {
  division <-
    map(1:(length(ts) - emb_dim),
        ~(ts[seq(.x, .x + emb_dim * t_lag - 1, t_lag)])) %>%
    invoke(rbind, .)


  map(1:nrow(division),
      ~(sweep(division, 2, division[.x,]) %>%
        abs %>%
        apply(1, max) %>%
        is_less_than(r) %>%
        sum() %>%
        subtract(1))) %>%
    flatten_dbl() %>%
    sum() %>%
    divide_by((length(ts) - emb_dim + 1)^2) %>%
    return()
}



sampen_c(ts, emb_dim, t_lag, sd(ts)) - sampen_c(ts, emb_dim+1, t_lag, sd(ts))



ts <- rep(c(85, 80, 89), 17)
emb_dim <- 2
r <- 3
t_lag <- 1

log(apen_c(ts, emb, lag, r)/apen_c(ts, emb+1, lag, r))

division <-
  map(1:(length(ts) - emb_dim),
      ~(ts[seq(.x, .x + emb_dim * t_lag - 1, t_lag)])) %>%
  invoke(rbind, .)


map(1:nrow(division),
    ~((sweep(division, 2, division[.x,]) < r) %>%
        apply(1, all) %>%
        sum %>%
        subtract(1))) %>%
  flatten_dbl()

(abs(sweep(division, 2, division[3,])) < r) %>%
  apply(1, all) %>%
  sum %>%
  subtract(1)
