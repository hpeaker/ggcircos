
#' @export
add_lines <- function(vis, seq_df) {
  
  UseMethod("add_lines", seq_df)
  
}

#' @export
add_lines.default <- function(vis, seq_df, seq, position, value, outer, inner, ...,
                              data = NULL, max_value = NULL, min_value = NULL) {
  
  
  seq_sub <- eval(substitute(seq), data, parent.frame())
  position_sub <- eval(substitute(position), data, parent.frame())
  value_sub <- eval(substitute(value), data, parent.frame())
  
  
  if(is.null(max_value)) max_value <- max(value_sub) 
  if(is.null(min_value)) min_value <- min(value_sub)
  
  df <- fit_lines(seq_sub, position_sub, value_sub, outer, inner, seq_df, max_value, min_value) %>% 
    group_by(seq) %>%
    arrange(theta)
  
  vis %>% layer_paths(data = df, ~sin(theta) * r, ~cos(theta) * r, ...)
  
}

#' @export
add_lines.reactive <- function(vis, seq, position, value, outer, inner, seq_df, ...,
                               data = NULL, max_value = NULL, min_value = NULL) {
  
  df <- shiny::reactive({
    
    if (shiny::is.reactive(data)) {
      data <- data()
    }
    
    seq_sub <- eval(substitute(seq, parent_env(environment())), data, parent.frame())
    position_sub <- eval(substitute(position, parent_env(environment())), data, parent.frame())
    value_sub <- eval(substitute(value, parent_env(envirnomnet())), data, parent.frame())
    
    if(is.null(max_value)) max_value <- max(value_sub) 
    if(is.null(min_value)) min_value <- min(value_sub)
    
    fit_lines(seq_sub, position_sub, value_sub, outer, inner, seq_df(), max_value, min_value) %>% group_by(seq) %>% arrange(theta)
    
  })
  
  vis %>% layer_paths(data = df, ~sin(theta) * r, ~cos(theta) * r, ...)
  
}

