library(shiny)
library(ggcircos)

N <- 4000

x <- dget("../pi_digits.txt")

#x <- dget("../Documents/e_digits.txt")

#x <- dget("../Documents/phi_digits.txt")


#x <- floor(runif(N) * 10)

x <- x[1:N]


from <- x[1:(N-1)]
to <- x[2:N]

pos_from <- 1:(N-1)
pos_to <- 2:N


links_df <- data.frame(from, to, pos_from, pos_to) %>% filter(from != to)


radians <- create_radians(seq = 0:9, lengths = rep(N, 10), total_gap = 0.05)
track_radians <- create_track_radians(radians)
seq_df <- create_seq_df(radians)



domain <- 0:9
range <- c("#87674A", "#F16B39", "#2C0E37", "#CB429F", "#008491", "#54C6EB",
           "#192BC2", "#557CE8", "#474B24", "#8DDCA4")

#server <- function(input, output, session) {

ggvis() %>%
  layer_rects(data = data.frame(x=-1.5,y=-1.5, x2 = 1.5, y2 = 1.5), ~x, ~y, x2 = ~x2, y2 = ~ y2, fill:="#000000") %>%
  add_track(track_radians, 1, 0.95, fill = ~group, stroke := "white") %>%
  add_text(seq_df = seq_df, data = seq_df, seq, N/2, seq, r = 1.05, baseline := "middle", align := "center", stroke := "white") %>%
  add_links(seq_df = seq_df, data = links_df, from, to, pos_from, pos_to, 0.92, 0.92, 0.3,
            stroke = ~name_from, strokeWidth := 0.3) %>%
  layer_text(data = data.frame(x = 0, y = 0, label = "U{0,9}"), ~x, ~y, text := ~label,
             baseline := "middle", align := "center", fontSize := 40, fontWeight := "bold", fill := "grey") %>%
  #layer_images(data = data.frame(x = 0, y = 0), ~x, ~y, width := 60, height := 60,
  #             url := "http://i61.tinypic.com/28bh9o0.png", align := "center", baseline := "middle") %>%
  #layer_text(data = data.frame(x = 0, y = 0, label = "e"), ~x, ~y, text := ~label,
  #           baseline := "middle", align := "center", fontSize := 100, fontWeight := "bold", fill := "grey") %>%
  scale_numeric("x", domain = c(-1, 1), nice = FALSE, clamp = TRUE) %>%
  scale_numeric("y", domain = c(-1, 1), nice = FALSE, clamp = TRUE) %>%
  scale_nominal("fill", domain, range) %>%
  scale_nominal("stroke", domain, range) %>%
  hide_legend(c("fill", "stroke")) %>%
  hide_axis("x") %>%
  hide_axis("y") %>%
  set_options(width = 800, height = 800, keep_aspect = TRUE)
#  bind_shiny("plot")

#}


# 
# ui <- shinyUI({
#   
#   fluidPage(
#   
#     mainPanel(
#       ggvisOutput("plot")  
#     )
#     
#   )
#   
# })
# 
# 
# shinyApp(ui = ui, server = server)
