

add_text <- function(vis, seq_df, ...) {
  
  UseMethod("add_text", seq_df)
  
}


add_text.default <- function(vis, seq_df, seq, position, label, r, ..., data = NULL) {
  
  seq_sub <- eval(substitute(seq), data, parent.frame())
  position_sub <- eval(substitute(position), data, parent.frame())
  label_sub <- eval(substitute(label), data, parent.frame())
  r_sub <- eval(substitute(r), data, parent.frame())
  
  text_df <- fit_to_seq(seq_sub, position_sub, seq_df, metadata = data.frame(label_sub, r_sub))
  
  vis %>% layer_text(data = text_df, ~sin(theta) * r_sub, ~cos(theta) * r_sub, text := ~label_sub, angle := ~theta * 180/pi, ...)
  
}


add_text.reactive <- function(vis, seq_df, seq, position, label, r, ..., data = NULL) {
  
  text_df <- reactive({
    
    if (shiny::is.reactive(data)) {
      data <- data()
    }
    
    seq_sub <- eval(substitute(seq, parent.env(environment())), data, parent.frame())
    position_sub <- eval(substitute(position, parent.env(environment())), data, parent.frame())
    label_sub <- eval(substitute(label, parent.env(environment())), data, parent.frame())
    r_sub <- eval(substitute(r, parent.env(environment())), data, parent.frame())
    
    fit_to_seq(seq_sub, position_sub, seq_df, metadata = data.frame(label_sub, r_sub))
    
  })
  
  vis %>% layer_text(data = text_df, ~sin(theta) * r, ~cos(theta) * r, text := ~label_sub, angle := ~theta * 180/pi, ...)
  
}



