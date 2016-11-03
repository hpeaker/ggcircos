



# fit_points <- function(..., data = NULL) {
#   
#   UseMethod("fit_points", data)
#   
# }


fit_points <- function(seq, position, value, outer, inner, seq_df, metadata = NULL, max_value = NULL, min_value = NULL) {
  
  r <- fit_to_track(value, outer, inner, max_value, min_value)
  
  df <- fit_to_seq(seq, position, seq_df, metadata)
  
  data.frame(df, r, orig_value = value)
  
}



# fit_points.data.frame <- function(data, seq, position, value, outer, inner, seq_df, max_value = NULL, min_value = NULL) {
#   
#   sub_value <- eval(substitute(value), data, parent.frame())
#   
#   if(is.null(max_value)) max_value <- max(sub_value)
#   if(is.null(min_value)) min_value <- min(sub_value)
#   
#   fit_points(
#     seq = eval(substitute(seq), data, parent.frame()),
#     position = eval(substitute(position), data, parent.frame()),
#     value = sub_value,
#     outer, inner, seq_df, max_value, min_value
#   )
#    
# }
