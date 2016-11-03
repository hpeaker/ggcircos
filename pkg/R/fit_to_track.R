

fit_to_track <- function(values, outer, inner, max_value = max(values), min_value = min(values)) {
  
  if(is.null(max_value)) max_value <- max(values)
  if(is.null(min_value)) min_value <- min(values)

  stand <- (values - min_value) / (max_value - min_value)
  
  scaled <- stand * (outer - inner) + inner
  
  scaled
  
}
