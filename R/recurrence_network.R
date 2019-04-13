#' @title recurrence_network
#' @param time_series time_series.
#' @param eps lag
#' @return The sum of \code{x} and \code{y}.
#' @export
recurrence_network <- function(ts, eps) {
  ts %>%
    recurrence_matrix(eps) %>%
    igraph::graph_from_adjacency_matrix() %>%
    return()
}

#' @title recurrence_matrix
#' @param time_series time_series.
#' @param eps lag
#' @return The sum of \code{x} and \code{y}.
#' @export
recurrence_matrix <- function(ts, eps) {
  recurrence_mat <- matrix(0, length(ts), length(ts))

  for (ii in 1:length(ts)) {
    recurrence_mat[ii,] <- abs(ts - ts[ii]) < eps
  }

  return(recurrence_mat)
}


