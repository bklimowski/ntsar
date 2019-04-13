#' @title transition_network
#' @description Create transition network
#' @param time_series.
#' @return Transition network as igraph object.
#' @examples
#' transition_network(rnorm(100), 2, 5)
#' @export
transition_network <- function(ts, lag, embedding) {
  patterns_sequence <- ordinal_patterns(ts, lag, embedding)

  connections <- tibble(
    "out" = patterns_sequence[1:length(patterns_sequence)-1],
    "into" = patterns_sequence[2:length(patterns_sequence)]) %>%
    group_by(out, into) %>%
    summarise(weigth = n()) %>%
    mutate(weigth = weigth/sum(weigth))

  igraph::graph_from_data_frame(connections) %>%
    igraph::set_vertex_attr("weigth",
                    value =
                      patterns_sequence %>%
                      janitor::tabyl() %>%
                      pull(.)) %>%
    igraph::set_edge_attr("weigth", value = connections$weigth) %>%
    return()
}


ordinal_patterns <- function(ts, lag, embedding) {
  1:(length(ts)-embedding) %>% map(~ts[c(.x,.x+lag,.x+embedding)] %>%
    rank() %>%
    as.character() %>%
    paste(collapse = '')) %>%
    unlist() %>%
    as.factor() %>%
    return()
}


