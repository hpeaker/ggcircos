

add_ribbons <- function(vis, seq_df, ...) {
  
  UseMethod("add_ribbons", seq_df)
  
}


add_ribbons.default <- function(vis, seq_df, name_from, name_to, pos_from_start, pos_from_end,
                                pos_to_start, pos_to_end, start_r, end_r, inner_r = 0.1,
                                ..., data = NULL, interpolate = "basis") {
  
  name_from_sub <- eval(substitute(name_from), data, parent.frame())
  name_to_sub <- eval(substitute(name_to), data, parent.frame())
  pos_from_start_sub <- eval(substitute(pos_from_start), data, parent.frame())
  pos_from_end_sub <- eval(substitute(pos_from_end), data, parent.frame())
  pos_to_start_sub <- eval(substitute(pos_to_start), data, parent.frame())
  pos_to_end_sub <- eval(substitute(pos_to_end), data, parent.frame())
  
  ribbons_df <- fit_ribbons(name_from_sub, name_to_sub, pos_from_start_sub,
                            pos_from_end_sub, pos_to_start_sub, pos_to_end_sub,
                            seq_df, start_r, end_r, inner_r)
  
  vis %>% layer_paths(data = ribbons_df %>% group_by(link), ~sin(theta) * r, ~cos(theta) * r,
                      ..., interpolate := interpolate)
  
}


add_ribbons.reactive <- function(vis, seq_df, name_from, name_to, pos_from_start, pos_from_end,
                                 pos_to_start, pos_to_end, start_r, end_r, inner_r = 0.1,
                                 ..., data = NULL, metadata = NULL, interpolate = "basis") {
  
  
  
  ribbons_df <- reactive({
    
    if (shiny::is.reactive(data)) {
      data <- data()
    }
    
    name_from_sub <- eval(substitute(name_from, parent.env(environment())), data, parent.frame())
    name_to_sub <- eval(substitute(name_to, parent.env(environment())), data, parent.frame())
    pos_from_start_sub <- eval(substitute(pos_from_start, parent.env(environment())), data, parent.frame())
    pos_from_end_sub <- eval(substitute(pos_from_end, parent.env(environment())), data, parent.frame())
    pos_to_start_sub <- eval(substitute(pos_to_start, parent.env(environment())), data, parent.frame())
    pos_to_end_sub <- eval(substitute(pos_to_end, parent.env(environment())), data, parent.frame())
    
    fit_ribbons(name_from_sub, name_to_sub, pos_from_start_sub,
                pos_from_end_sub, pos_to_start_sub, pos_to_end_sub,
                seq_df(), start_r, end_r, inner_r)
    
  })
  
  vis %>% layer_paths(data = ribbons_df %>% group_by(link), ~sin(theta) * r, ~cos(theta) * r,
                      ..., interpolate := interpolate)
  
}
