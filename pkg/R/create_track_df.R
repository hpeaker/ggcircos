

create_track_df <- function(track_radians, outer, inner) {
  
  outer_df <- data.frame(r = outer, theta = track_radians, group = factor(names(track_radians), levels = unique(names(track_radians))))
  inner_df <- data.frame(r = inner, theta = track_radians, group = factor(names(track_radians), levels = unique(names(track_radians))))
  
  ## use interpolate := "linear-closed" in layer_paths to avoid the need to join up paths manually
  
  track_df <- rbind(outer_df, arrange(inner_df, -theta))
  
  return(track_df)
  
}
