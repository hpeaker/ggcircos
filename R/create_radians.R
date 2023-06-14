

#' @export
create_radians <- function(seq = c(1:22, "X", "Y"),
                           lengths = c(249250621,243199373,198022430,191154276,180915260,171115067,
                                       159138663,146364022,141213431,135534747,135006516,133851895,
                                       115169878,107349540,102531392,90354753,81195210,78077248,
                                       59128983,63025520,48129895,51304566,155270560,59373566),
                           total_gap = 0.2,
                           ind_gaps = rep(total_gap / length(seq), length(seq))) {
  
  stand_lengths = lengths / sum(lengths)
  
  all_lengths <- c(rbind(ind_gaps, stand_lengths))
  
  all_stand_lengths <- all_lengths / sum(all_lengths)
  
  all_radians <- cumsum(all_stand_lengths) * 2 * pi
  
  names(all_radians) <- rep(seq, each = 2)
  
  attr(all_radians, "lengths") <- lengths
  
  return(all_radians)
  
}
