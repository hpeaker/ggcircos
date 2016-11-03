

create_seq_df <- function(radians, seq = unique(names(radians)),
                          lengths = attr(radians, "lengths"),
                          scale = 1) {
  
  seq_starts <- radians[seq(1, length(radians), 2)]
  seq_ends <- radians[seq(2, length(radians), 2)]
  
  data.frame(seq = seq, length = lengths, seq_start = seq_starts,
             seq_end = seq_ends, scale = scale)
  
}






