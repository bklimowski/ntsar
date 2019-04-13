#' @title recurrence_network
#' @description Create recurrence network
#' @param time_series.
#' @return Recurrence network as igraph object.
#' @examples
#' recurrence_network(rnorm(100), 0.5)
#' @export
recurrence_network <- function(ts, eps) {
  ts %>%
    recurrence_matrix(eps) %>%
    igraph::graph_from_adjacency_matrix() %>%
    return()
}

#' @title Recurrence matrix
#' @description Create recurrence matrix
#' @param time_series.
#' @return Recurrence matrix
#' @examples
#' recurrence_matrix(rnorm(100), 0.5)
#' @export
recurrence_matrix <- function(ts, eps) {
  recurrence_mat <- matrix(0, length(ts), length(ts))

  for (ii in 1:length(ts)) {
    recurrence_mat[ii,] <- abs(ts - ts[ii]) < eps
  }

  return(recurrence_mat)
}


