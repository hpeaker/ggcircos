
## S3 method moved to fit_points() which calls this function
# fit_to_seq <- function(..., data = NULL) {
#   
#   UseMethod("fit_to_seq", data)
#   
# }
#' @export
fit_to_seq <- function(seq, positions, seq_df, metadata = NULL) {
  
  if (is.null(metadata)) {
  
    df <- data.frame(seq = seq, position = positions)
    
  } else {
    
    df <- data.frame(seq = seq, position = positions, metadata)
    
  }
  
  df %>% 
    inner_join(seq_df, by = "seq") %>% 
    mutate(theta = seq_start + (position * scale / length) * (seq_end - seq_start)) %>%
    dplyr::select(-seq_start, -seq_end, -length)
  
}


# fit_to_seq.data.frame <- function(data, seq, positions, seq_df) {
#   
#   sub_seq <- substitute(seq)
#   
#   fit_to_seq(seq = eval(sub_seq, data, parent.frame()),
#              positions = eval(substitute(positions), data, parent.frame()),
#              seq_df) %>%
#     cbind(data)
#   
# }
