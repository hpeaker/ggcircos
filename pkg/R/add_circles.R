

add_circles <- function(vis, track_radians, ...) {
  
  UseMethod("add_circles", track_radians)
  
}


add_circles.numeric <- function(vis, track_radians, r, ..., interpolate = "linear-open") {
  
  df <- data.frame(name = rep(names(track_radians), length(r)), theta = rep(track_radians, length(r)), r = rep(r, each = length(track_radians)))
  
  vis %>% layer_paths(data = df %>% group_by(r, name), ~sin(theta) * r, ~cos(theta) * r, interpolate := interpolate, ...)
  
}

add_circles.reactive <- function(vis, track_radians, r, ..., interpolate = "linear-open") {
  
  df <- shiny::reactive({
    
    data.frame(name = rep(names(track_radians()), length(r)), theta = rep(track_radians(), length(r)), r = rep(r, each = length(track_radians())))
    
  })
  
  vis %>% layer_paths(data = df %>% group_by(r, name), ~sin(theta) * r, ~cos(theta) * r, interpolate := interpolate, ...)
  
}
