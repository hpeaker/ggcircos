

create_track_radians <- function(radians,
                                 seq = unique(names(radians)),
                                 approx_points = 400,
                                 points_per_track = rep(max(c(2, ceiling(approx_points / length(seq)))), length(seq))) {
  
  track_radians <- do.call(c, lapply(seq_along(seq),
                                     function(i) {
                                       x <- c(seq(radians[2*i - 1], radians[2*i], length.out = points_per_track[i]))
                                       names(x) <- rep(seq[i], length(x))
                                       x
                                     }
  )
  )
  
  # having attributes on things which are passed to ggvis causes errors
  # could remove at a later point but this can be fixed an alternate way (see create_track_df)
  ##attr(track_radians, "seq") <- seq
  
  return(track_radians)
  
}

