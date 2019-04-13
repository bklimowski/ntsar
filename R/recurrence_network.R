#' @title recurrence_network
#' @description Create recurrence network
#' @param time_series time series
#' @param  eps eps
#' @return Recurrence network as igraph object.
#' @export
recurrence_network <- function(ts, eps) {
  ts %>%
    recurrence_matrix(eps) %>%
    igraph::graph_from_adjacency_matrix() %>%
    return()
}

#' @title Recurrence matrix
#' @description Create recurrence matrix
#' @param time_series time series
#' @param eps eps
#' @return Recurrence matrix
#' @export
recurrence_matrix <- function(ts, eps) {
  recurrence_mat <- matrix(0, length(ts), length(ts))

  for (ii in 1:length(ts)) {
    recurrence_mat[ii,] <- abs(ts - ts[ii]) < eps
  }

  return(recurrence_mat)
}


