




#' @export
add_ticks <- function(vis, radians, outer, inner, ...) {
  
  df <- data.frame(theta = rep(radians, 2), r = c(rep(outer, length(radians)), rep(inner, length(radians))))
  
  vis %>% layer_paths(data = df %>% group_by(theta), ~sin(theta) * r, ~cos(theta) * r, ...)
  
}

