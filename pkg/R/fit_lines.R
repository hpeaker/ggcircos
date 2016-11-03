

fit_lines <- function(seq, position, value, outer, inner, seq_df, max_value = NULL, min_value = NULL, between_points = 10) {
  
  seq <- as.character(seq)
  
  new_seq <- function(seq, between_points) {
    c(rep(as.character(seq[-length(seq)]), each = between_points - 1),
      as.character(seq[length(seq)])
    )
  }
  
  new_position <- function(position, between_points) {
    c(position[1],
      mapply(seq, lag(position)[-1], position[-1],
             MoreArgs = list(length.out = between_points))[-1,]
    )
  }
  
  new_value <- function(value, between_points) {
    c(value[1],
      mapply(seq, lag(value)[-1], value[-1],
             MoreArgs = list(length.out = between_points))[-1,]
    )
  }
  
  df <- data.frame(seq, position, value) %>%
    group_by(seq) %>%
    do(
      data.frame(seq = new_seq(.$seq, between_points),
                 position = new_position(.$position, between_points),
                 value = new_value(.$value, between_points)
      )
    )
  
  fit_points(df$seq, df$position, df$value, outer, inner, seq_df, max_value, min_value)
  
}
