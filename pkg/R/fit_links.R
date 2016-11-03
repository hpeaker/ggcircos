

fit_links <- function(name_from, name_to, pos_from, pos_to, seq_df, start_r, end_r, inner_r = 0.1) {
  
  link_df <- data.frame(seq_from = factor(name_from, levels = levels(seq_df$seq)),
                        seq_to = factor(name_to, levels = levels(seq_df$seq)), pos_from, pos_to)
  
  link_df %>%
    inner_join(seq_df, by = c("seq_from" = "seq")) %>%
    inner_join(seq_df, by = c("seq_to" = "seq")) %>%
    mutate(new_pos_from = seq_start.x + (pos_from * scale.x / length.x) * (seq_end.x - seq_start.x),
           new_pos_to = seq_start.y + (pos_to * scale.y / length.y) * (seq_end.y - seq_start.y)) %>%
    select(seq_from, seq_to, new_pos_from, new_pos_to) %>%
    group_by(link = row.names(.)) %>%
    do(fit_link(.$seq_from, .$seq_to, .$new_pos_from, .$new_pos_to, start_r, end_r, inner_r)) %>%
    mutate(inter = ifelse(name_to == name_from, "No", "Yes"))
  
}
