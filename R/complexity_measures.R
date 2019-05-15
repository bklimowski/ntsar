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
