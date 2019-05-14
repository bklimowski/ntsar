#' @import dplyr
#' @import purrr

natural_visibility_graph <- function(time_series) {
  fast_VG(time_series, 1, length(time_series)) %>%
    tibble::rownames_to_column() %>%
    group_by(rowname) %>%
    mutate(vrtx_out = min(v1, v2),
           vrtx_in = max(v1, v2)) %>%
    ungroup() %>%
    select(vrtx_out, vrtx_in) %>%
    filter(vrtx_out != vrtx_in) %>%
    arrange(vrtx_out, vrtx_in) %>%
    mutate(view_angle =  atan((time_series[vrtx_in] - time_series[vrtx_out])/(vrtx_in - vrtx_out))) %>%
    igraph::graph_from_data_frame() %>%
    return()
}

fast_VG <- function(ts, left, right) {
  if (left < right ) {
    ts_tmp <- ts[left:right]
    k <-  which(ts_tmp == max(ts_tmp))[1] + left - 1

    tibble(v1 = k,
           v2 = left:right,
           is_connected =
             left:right %>%
             map(~nodes_visibility(ts,k,.x)) %>%
             unlist()
           ) %>%
      filter(is_connected == TRUE) %>%
      bind_rows(fast_VG(ts, left, k - 1),
                fast_VG(ts, k + 1, right)) %>%
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

  if (i == j) {
    return(visibility_condition)
  }

  if (i > j) {
    return(nodes_visibility(rev(ts), length(ts) - i + 1, length(ts) - j + 1))
  }

  ii <- i + 1
  while (visibility_condition) {
    if (ii == j) {break}
    visibility_condition <- ts[ii] <= ts[j] + (ts[i] - ts[j]) * ((j - ii)/(j - i))
    ii <- ii + 1
  }
  return(visibility_condition)
}

# horizontal visibility ---------------------------------------------------

horizontal_visibility_graph <- function(ts) {
  res_list <- map(1:length(ts), ~horizontal_visibility(ts, .x, length(ts)))

  matrix(c(map(1:length(res_list),
               ~rep(.x,length(res_list[[.x]]))) %>% flatten_int(),
           res_list %>% flatten_int()),
         nc = 2) %>%
    igraph::graph_from_edgelist() %>%
    return()
}

horizontal_visibility <- function(ts, left, right) {
  result <- c( )
  k <- first_greater(ts, left, right)
  while (!is.null(k)) {
    result <- c(result, k)
    k <-  max_val_ts(ts,left,k)
  }
  return(result)
}

first_greater <- function(ts, left, right) {
  ts_tmp <- ts[left:right]
  if (ts[left] == max(ts[left:right])) {
    return(right)
  }
  else{
    return(as.integer(which(ts_tmp > ts_tmp[1])[1] + left - 1) )
  }
}

max_val_ts <- function(ts, left, right) {

  if ((right - left) > 1) {
      ts_tmp <- ts[(left + 1):(right - 1)]
      return(which(ts_tmp == max(ts_tmp))[1] + left)
  }

  else {
    return(NULL)
  }
}


time_series <- ts
df <- fast_VG(time_series, 1, length(time_series)) %>%
  tibble::rownames_to_column() %>%
  group_by(rowname) %>%
  mutate(vrtx_out = min(v1, v2),
         vrtx_in = max(v1, v2)) %>%
  ungroup() %>%
  select(vrtx_out, vrtx_in) %>%
  filter(vrtx_out != vrtx_in) %>%
  arrange(vrtx_out, vrtx_in)

visibility_angle <- atan(ts[df$vrtx_in] - ts[df$vrtx_out])/(df$vrtx_in - df$vrtx_out)



