
#' @export
fit_ribbons <- function(name_from, name_to, pos_from_start, pos_from_end,
                        pos_to_start, pos_to_end, seq_df,
                        start_r, end_r, inner_r = 0.1, metadata = NULL) {
  
  if(!is.factor(seq_df$seq)){
    seq_df$seq <- factor(seq_df$seq)
  }
  ribbon_df <- data.frame(name_from = factor(name_from, levels = levels(seq_df$seq)),
                          name_to = factor(name_to, levels = levels(seq_df$seq)),
                          pos_from_start = pos_from_start,
                          pos_from_end = pos_from_end,
                          pos_to_start = pos_to_start,
                          pos_to_end = pos_to_end
  )
  
  transformed <- ribbon_df %>%
    inner_join(seq_df, by = c("name_from" = "seq")) %>%
    inner_join(seq_df, by = c("name_to" = "seq")) %>%
    mutate(new_pos_from_start = seq_start.x + (pos_from_start * scale.x / length.x) * (seq_end.x - seq_start.x),
           new_pos_from_end = seq_start.x + (pos_from_end * scale.x / length.x) * (seq_end.x - seq_start.x),
           new_pos_to_start = seq_start.y + (pos_to_start * scale.y / length.y) * (seq_end.y - seq_start.y),
           new_pos_to_end = seq_start.y + (pos_to_end * scale.y / length.y) * (seq_end.y - seq_start.y)) %>%
    select(name_from, name_to, new_pos_from_start, new_pos_from_end,
           new_pos_to_start, new_pos_to_end) %>%
    group_by(link = row.names(.)) %>%
    do(fit_ribbon(.$name_from, .$name_to, .$new_pos_from_start, .$new_pos_from_end,
                  .$new_pos_to_start, .$new_pos_to_end, start_r, end_r, inner_r)) %>%
    mutate(inter = ifelse(name_to == name_from, "No", "Yes"))
  
  if (is.null(metadata)) {
    return(transformed)
  } else {
    metadata$link <- row.names(metadata)
    transformed %>%
      inner_join(metadata, "link")
  }
  
}