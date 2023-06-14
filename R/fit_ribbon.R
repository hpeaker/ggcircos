

#' @export
fit_ribbon <- function(name_from, name_to, pos_from_start, pos_from_end,
                       pos_to_start, pos_to_end,
                       start_r, end_r, inner_r = 0.1) {
  
  t <- c(0, 0.02, 0.25, 0.5, 0.75, 0.98, 1)
  
  from_mid <- mean(c(pos_from_start, pos_from_end))
  to_mid <- mean(c(pos_to_start, pos_to_end))
  
  theta_mid <-  mean(c(from_mid, to_mid)) + 
    ifelse(abs(from_mid - to_mid) > pi, pi, 0)
  
  theta <- c((pos_from_start * (1 - t) + pos_from_end * t),
             theta_mid,
             (pos_to_start * (1 - t) + pos_to_end * t),
             theta_mid,
             pos_from_start
  )
  
  r_inner <- pmin(abs(from_mid - to_mid), abs(abs(from_mid - to_mid) - 2*pi)) / 10
  
  r <- do.call(c,
               lapply(r_inner, function(x) c(rep(start_r, length(t)),
                                                inner_r + pi/10 - x,
                                                rep(end_r, length(t)),
                                                inner_r + pi/10 - x,
                                                start_r)
               )
  )
  
  data.frame(name_from = name_from,
             name_to = name_to,
             theta,
             r
  )
             
  
}
