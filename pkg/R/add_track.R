


add_track <- function(vis, track_radians, ...) {
  
  UseMethod("add_track", track_radians)
  
}

add_track.numeric <- function(vis, track_radians, outer, inner, ..., interpolate = "linear-closed") {
  
  track_df <- create_track_df(track_radians, outer, inner)
  
  vis %>% layer_paths(data = track_df %>% group_by(group), ~sin(theta) * r, ~cos(theta) * r, interpolate := interpolate, ...)
  
} 

add_track.reactive <- function(vis, track_radians, outer, inner, ..., interpolate = "linear-closed") {
  
  track_df <- shiny::reactive({
    
    create_track_df(track_radians(), outer, inner)
    
  })
  
  vis %>% layer_paths(data = track_df %>% group_by(group), ~sin(theta) * r, ~cos(theta) * r, interpolate := interpolate, ...)
  
}
