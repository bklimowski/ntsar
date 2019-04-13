#' @import dplyr
#' @import purrr


#' @title visibility_graph
#' @param time_series time_series.
#' @return The sum of \code{x} and \code{y}.
#' @export
visibility_graph <- function(time_series) {

  fast_VG(time_series, 1, length(time_series)) %>%
    tibble::rownames_to_column() %>%
    group_by(rowname) %>%
    mutate(vrtx_out = min(v1, v2),
           vrtx_in = max(v1, v2)) %>%
    ungroup() %>%
    select(vrtx_out, vrtx_in) %>%
    filter(vrtx_out != vrtx_in) %>%
    arrange(vrtx_out, vrtx_in) %>%
    igraph::graph_from_data_frame() %>%
    return()
}

fast_VG <- function(ts, left, right) {
  if (left < right ) {
    ts_tmp <- ts[left:right]
    k <-  which(ts_tmp==max(ts_tmp))[1] + left -1

    tibble(v1 = k,
           v2 = left:right,
           is_connected =  left:right %>%
             map(~nodes_visibility(ts,k,.x)) %>%
             unlist()) %>%
      filter(is_connected == TRUE) %>%
      bind_rows(fast_VG(ts, left, k-1),
                fast_VG(ts, k+1, right)) %>%
      return()
  }
  else{
    return(tibble())
  }
}


nodes_visibility <- function(ts, i, j) {

  visibility_condition <- TRUE
  if (j > length(ts)) {
    warning("Too long!")
    return(NaN)
  }

  if (i==j) {
    return(visibility_condition)
  }

  if(i > j) {
    return(nodes_visibility(rev(ts), length(ts) - i +1, length(ts)- j +1))
  }

  ii <- i + 1
  while (visibility_condition) {
    if (ii == j) {break}
    visibility_condition <- ts[ii] <= ts[j] + (ts[i] - ts[j]) *((j - ii)/(j - i))
    ii <- ii + 1
  }
  return(visibility_condition)
}
