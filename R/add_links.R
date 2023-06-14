
#' @export
add_links <- function(vis, seq_df, ...) {
  
  UseMethod("add_links", seq_df)
  
}

#' @export
add_links.data.frame <- function(vis, seq_df, name_from, name_to, pos_from, pos_to,
                              start_r, end_r, inner_r = 0.1, ..., data = NULL,
                              interpolate = "basis") {
  
  name_from_sub <- eval(substitute(name_from), data, parent.frame())
  name_to_sub <- eval(substitute(name_to), data, parent.frame())
  pos_from_sub <- eval(substitute(pos_from), data, parent.frame())
  pos_to_sub <- eval(substitute(pos_to), data, parent.frame())
  
  links_df <- fit_links(name_from_sub, name_to_sub, pos_from_sub, pos_to_sub, seq_df, start_r, end_r, inner_r)
  
  vis %>% layer_paths(data = links_df %>% group_by(link), ~sin(theta) * r, ~cos(theta) * r, ..., interpolate := interpolate)
  
}

#' @export
add_links.reactive <- function(vis, seq_df, name_from, name_to, pos_from, pos_to,
                               start_r, end_r, inner_r = 0.1, ..., data = NULL, interpolate = "basis") {
  
  
  links_df <- reactive({
    
    if (shiny::is.reactive(data)) {
      data <- data()
    }
    
    name_from_sub <- eval(substitute(name_from, parent.env(parent.env(environment()))), data, parent.frame())
    name_to_sub <- eval(substitute(name_to, parent.env(parent.env(environment()))), data, parent.frame())
    pos_from_sub <- eval(substitute(pos_from, parent.env(parent.env(environment()))), data, parent.frame())
    pos_to_sub <- eval(substitute(pos_to, parent.env(parent.env(environment()))), data, parent.frame())
    
    fit_links(name_from_sub, name_to_sub, pos_from_sub, pos_to_sub, seq_df(), start_r, end_r, inner_r)    
    
  })
  
  vis %>% layer_paths(data = links_df %>% group_by(link), ~sin(theta) * r, ~cos(theta) * r, ..., interpolate := interpolate)
  
}

