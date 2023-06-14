
library(ggcircos)

### possible workflow for creating a network graph

from <- sample(as.character(1:100), 100, prob = seq(1, 0.01, length.out = 100), replace = TRUE)
to <- sample(as.character(1:100), 100, replace = TRUE)

links <- data.frame(from, to, pos_from = 1, pos_to = 1)

metadata <- links %>% group_by(from) %>% summarise(count = n()) %>% arrange(-count)
zero_seq <- data.frame(from = as.character((1:100)[!(1:100 %in% metadata$from)]), count = 0)
metadata <- rbind(metadata, zero_seq)


seq <- metadata$from
lengths <- rep(1, 100)

radians <- create_radians(seq, lengths)

seq_df <- create_seq_df(radians)



fitted_links <- fit_links(name_from = links$from, name_to = links$to, pos_from = links$pos_from, pos_to = links$pos_to, seq_df = seq_df, 0.9, 0.9, 0)

fitted_links$opac <- ifelse(fitted_links$name_from == "1" | fitted_links$name_to == "1", 1, 0.3)

# opac_choice <- list("None" = 0.3,
#   "1",# = ifelse(fitted_links$name_from == "1" | fitted_links$name_to == "1", 1, 0.3),
#   "2",# = ifelse(fitted_links$name_from == "2" | fitted_links$name_to == "2", 1, 0.3)
# )

opac_choice <- c("None", "1", "2")

opac_map <- function(val) {
  
  if (val == "None") {
    0.3
  } else {
    
    ifelse(fitted_links$name_from == val | fitted_links$name_to == "1", 1, 0.3)
    
  }
  
}

points <- data.frame(name = metadata$from, pos = 1, value = 1)

ggvis() %>% add_points(seq_df = seq_df, data = points, seq = name, position = pos, value = value,
                       outer = 1, inner = 0.9, max_value = 2, min_value = 0,
                       metadata = metadata, size = ~count, fillOpacity := 0,
                       stroke = "black", fill := "red", fillOpacity.hover := 1) %>%
#   add_links(seq_df = seq_df, data = links[1:200,], name_from = from,
#             name_to = to, pos_from = pos_from, pos_to = pos_to,
#             0.9, 0.9, 0, stroke = ~name_from, strokeOpacity := input_slider(0, 1, value = 0.3)) %>%
  layer_paths(data = fitted_links, ~sin(theta) * r, ~cos(theta) * r, stroke = ~name_from, interpolate := "basis",
              strokeOpacity := ~c(0.2, 0.6, 1)) %>%
                #input_select(choices = opac_choice, selected = "None", map = opac_map)) %>%
  hide_axis("x") %>%
  hide_axis("y") %>%
  hide_legend("stroke")



### in an app

library(shiny)

server <- function(input, output) {
  
  
  re_values <- reactiveValues(hovered_point = NULL, hovered_link = NULL)
  
  
  fitted_links <- reactive({
    
    l <- fit_links(name_from = links$from, name_to = links$to, pos_from = links$pos_from, pos_to = links$pos_to, seq_df = seq_df, 0.9, 0.9, 0)
    
    if (is.null(re_values$hovered_point)) {
      
      l$opac <- 0.3
      
    } else {
      
      l$opac <- ifelse(l$name_from %in% re_values$hovered_point | l$name_to %in% re_values$hovered_point,
                       1, 0.1)
      
    }
    
    l
    
  })
  
  
  
  
  hover_point_fun_over <- function(data, ...) {
    
    if ("count" %in% names(data)) {
    
      re_values$hovered_point <- data$seq
      
    }
    
    if ("link" %in% names(data)) {
      
      str(data)
      
    }
    
  }
  
  hover_point_fun_out <- function(...) {
    
    re_values$hovered_point <- NULL
    
  }
  
  
  ggvis() %>% add_points(seq_df = seq_df, data = points, seq = name, position = pos, value = value,
                         outer = 1, inner = 0.9, max_value = 2, min_value = 0,
                         metadata = metadata, size = ~count, fillOpacity := 0,
                         stroke := "lightblue",
                         fill = ~seq, fillOpacity.hover := 1) %>%
    layer_paths(data = fitted_links %>% group_by(link), x = ~sin(theta) * r, y = ~cos(theta) * r, interpolate := "basis",
                stroke = ~name_to, strokeOpacity := ~opac, strokeOpacity.hover := 1,
                strokeWidth.hover := 2, strokeWidth := 1) %>%
#     add_links(seq_df = seq_df, data = links, name_from = from,
#               name_to = to, pos_from = pos_from, pos_to = pos_to,
#               0.9, 0.9, 0, stroke = ~name_from, strokeOpacity := 0.3,
#               strokeOpacity.hover := 1, strokeWidth.hover := 2, strokeWidth := 1) %>%
    handle_hover(hover_point_fun_over, hover_point_fun_out) %>%
    hide_axis("x") %>%
    hide_axis("y") %>%
    hide_legend(c("stroke", "fill")) %>%
    bind_shiny("network")
  
  
  
}

ui <- fluidPage(
  
  mainPanel(ggvisOutput("network"))
  
)

shinyApp(ui = ui, server = server)



