#' @title transition_network
#' @param time_series time_series.
#' @param lag lag
#' @param embedding embds
#' @return The sum of \code{x} and \code{y}.
#' @export

# t <- 4
# m <- 3
#
# ts <- sin((1:1000)/30) + runif(1000)
#
#
#
# ordinal_patterns <-
#   1:(length(ts)-t*m) %>%
#   map(~ts[seq(.x, .x+(m*t-1), t)] %>%
#         rank %>%
#         invoke(str_c, .)) %>%
#   as_vector()
#
# gg <- tibble(vrtx_out = ordinal_patterns[1:(length(ordinal_patterns)-1)],
#              vrtx_in = ordinal_patterns[2:length(ordinal_patterns)]) %>%
#   group_by(vrtx_out, vrtx_in) %>%
#   summarise(count = n()) %>%
#   ungroup() %>%
#   mutate(count = count/sum(count)) %>%
#   igraph::graph_from_data_frame()
#
#
# igraph::get.edge.attribute(gg)$count
#
# plot(gg, edge.curved=.3, edge.arrow.size=.3, edge.width =
#        igraph::get.edge.attribute(gg)$count*50)
