

#' @export
add_points <- function(vis, seq_df, ...) {
  
  UseMethod("add_points", seq_df)
  
}

#' @export
add_points.default <- function(vis, seq_df, seq, position, value, outer, inner, ..., data = NULL, metadata = NULL, max_value = NULL, min_value = NULL) {
  
  seq_sub <- eval(substitute(seq), data, parent.frame())
  position_sub <- eval(substitute(position), data, parent.frame())
  value_sub <- eval(substitute(value), data, parent.frame())
  
  r <- outer
  if(is.null(max_value)) max_value <- max(value_sub) 
  if(is.null(min_value)) min_value <- min(value_sub)
  
  df <- fit_points(seq_sub, position_sub, value_sub, outer, inner, seq_df, metadata, max_value, min_value)
  
  vis %>% layer_points(data = df, ~sin(theta) * r, ~cos(theta) * r, ...)
  
}

#' @export
add_points.reactive <- function(vis, seq_df, seq, position, value, outer, inner, ..., data = NULL, metadata = NULL, max_value = NULL, min_value = NULL) {
  
  
  
  df <- shiny::reactive({
    
    if (shiny::is.reactive(data)) {
      
      data <- data()
      
    }
    
    seq_sub <- eval(substitute(seq, parent.env(parent.env(environment()))), data, parent.frame())
    position_sub <- eval(substitute(position, parent.env(parent.env(environment()))), data, parent.frame())
    value_sub <- eval(substitute(value, parent.env(parent.env(environment()))), data, parent.frame())
    
    
    if(is.null(max_value)) max_value <- max(value_sub) 
    if(is.null(min_value)) min_value <- min(value_sub)
    
    fit_points(seq_sub, position_sub, value_sub, outer, inner, seq_df(), metadata, max_value, min_value)
    
  })
  
  vis %>% layer_points(data = df, ~sin(theta) * r, ~cos(theta) * r, ...)
  
}

