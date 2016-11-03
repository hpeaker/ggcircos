

fit_link <- function(name_from, name_to, pos_from, pos_to, start_r, end_r, inner_r = 0.1) {
  
  theta <- c(pos_from, 
             mean(c(pos_from, pos_to)) + ifelse(abs(pos_from - pos_to) > pi, pi, 0),
             pos_to)
  
  ## altering the mid radius depending on the distance between points (far away pass through middle, close don't move too far away from start)
  ## this is just manually scaled to look okay on a circle of about radius 1 so needs more work
  
  r_inner <- pmin(abs(pos_from - pos_to), abs(abs(pos_from - pos_to) - 2*pi)) / 10
  
  r <- do.call(c, lapply(r_inner, function(x) c(start_r, inner_r + pi/10 - x, end_r)))
  
  #r <- c(start_r, inner_r, end_r)
  
  data.frame(name_from = name_from, 
             name_to = name_to,
             theta,
             r = r)
  
}
