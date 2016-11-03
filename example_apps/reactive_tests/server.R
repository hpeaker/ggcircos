
library(ggcircos)
library(shiny)

## define some sequences
chroms <- c(1,2,3)
chrom_lengths <- c(10000, 5000, 2000)
total_gap <- 0.1

radians <- create_radians(chroms, chrom_lengths, total_gap)

# create sample data
chrom_positions <- sapply(chrom_lengths, sample, 20)
points_df <- data.frame(name = factor(chroms), position = c(t(chrom_positions))) %>% arrange(name, position)
points_df$value <- rnorm(20 * length(chroms))
points_df$key <- 1:60

chrom_positions <- sapply(chrom_lengths, sample, 20)
points_df2 <- data.frame(name = factor(chroms), position = c(t(chrom_positions))) %>% arrange(name, position)
points_df2$value <- rnorm(20 * length(chroms))
points_df2$key <- 1:60

chr_froms <- sample(chroms, 10, replace = T)
chr_tos <- sample(chroms, 10, replace = T)
pos_froms <- sapply(chr_froms, function(x) sample(chrom_lengths[chroms == x], 1))
pos_tos <- sapply(chr_tos, function(x) sample(chrom_lengths[chroms == x], 1))

chr_froms2 <- sample(chroms, 10, replace = T)
chr_tos2 <- sample(chroms, 10, replace = T)
pos_froms2 <- sapply(chr_froms2, function(x) sample(chrom_lengths[chroms == x], 1))
pos_tos2 <- sapply(chr_tos2, function(x) sample(chrom_lengths[chroms == x], 1))

links_df <- data.frame(chr_from = chr_froms, pos_from = pos_froms, chr_to = chr_tos, pos_to = pos_tos)
links_df2 <- data.frame(chr_from = chr_froms2, pos_from = pos_froms2, chr_to = chr_tos2, pos_to = pos_tos2)

# chr_to <- sample(chroms, 5, replace = T)
# chr_from <- sample(chroms, 5, replace = T)
# 
# chr_to2 <- sample(chroms, 5, replace = T)
# chr_from2 <- sample(chroms, 5, replace = T)

chr_to <- c(1,1,2,2,3)
chr_from <- c(2,3,3,1,1)

chr_to2 <- c(3,3,2,1,2)
chr_from2 <- c(1,2,1,2,3)

pos_from_start <- sapply(chr_from, function(x) sample(chrom_lengths[chroms == x] - 1000, 1))
pos_from_end <- pos_from_start + sample(100:1000, 5)
pos_to_start <- sapply(chr_to, function(x) sample(chrom_lengths[chroms == x] - 1000, 1))
pos_to_end <- pos_to_start + sample(100:1000, 5)

pos_from_start2 <- sapply(chr_from2, function(x) sample(chrom_lengths[chroms == x] - 1000, 1))
pos_from_end2 <- pos_from_start2 + sample(100:1000, 5)
pos_to_start2 <- sapply(chr_to2, function(x) sample(chrom_lengths[chroms == x] - 1000, 1))
pos_to_end2 <- pos_to_start2 + sample(100:1000, 5)


# chr_to <- c(1,1,2,2,3)
# chr_from <- c(1,3,3,1,1)
# 
# pos_from_start <- c()


ribbons_df <- data.frame(chr_from = chr_from, chr_to = chr_to,
                         pos_from_start = pos_from_start,
                         pos_from_end = pos_from_end,
                         pos_to_start = pos_to_start,
                         pos_to_end = pos_to_end)

ribbons_df2 <- data.frame(chr_from = chr_from2, chr_to = chr_to2,
                          pos_from_start = pos_from_start2,
                          pos_from_end = pos_from_end2,
                          pos_to_start = pos_to_start2,
                          pos_to_end = pos_to_end2)





server <- function(input, output, session) {
  
  re_values <- reactiveValues(scaling_factors = rep(1, 3), previous_radians = radians, chrom_clicked = chroms[1], previous_rotate = 0)
  
  radians <- reactive({
    
    rads <- create_radians(seq = chroms, lengths = chrom_lengths * re_values$scaling_factors, total_gap)
    
    # isolate here so that radians only re-evaluates when the click handle changes scaling_factors
    isolate(mid <- mean(rads[names(rads) == re_values$chrom_clicked]))
    
    isolate(prev_mid <- mean(re_values$previous_radians[names(rads) == re_values$chrom_clicked]))
    
    offset <- mid - prev_mid
    
    rads - offset + pi * (input$rotate - re_values$previous_rotate) / 180
    
  })
  
  track_radians <- reactive({
    
    create_track_radians(radians(), seq = chroms, approx_points = 400)
    
  })
  
  seq_df <- reactive({
    
    s <- create_seq_df(radians = radians(), scale = re_values$scaling_factors)
    
  })
  
  points <- reactive({
    
    if (input$points == 1) {
      #print(head(points_df))
      points_df
    } else {
      #print(head(points_df2))
      points_df2
    }
    
  })
  
  links <- reactive({
    
    if (input$points == 1) {
      links_df
    } else {
      links_df2
    }
    
  })
  
  ribbons <- reactive({
    
    if (input$points == 1) {
      ribbons_df
    } else {
      ribbons_df2
    }
    
  })
  
  click_handle <- function(data, location, session) {
    
    if(is.null(data)) return(NULL)
    
    isolate(re_values$chrom_clicked <- data$group)
    
    isolate(re_values$previous_radians <- radians())
    
    isolate(re_values$previous_rotate <- input$rotate)
    
    isolate(re_values$scaling_factors[which(chroms == data$group)] <- ifelse(re_values$scaling_factors[which(chroms == data$group)] == 1,
                                                                             input$scale_factor, 1))
    
    #print(data)
    
  }
  
  tooltip_fun <- function(data) {
    if ("key" %in% names(data)) {

      points()[points()$key == data$key, ]$value
      
    }
  }
  
  
  ggvis() %>%
    add_track(track_radians, 1, 0.9, fill = ~group) %>%
    add_track(track_radians, 0.85, 0.6) %>%
    add_circles(track_radians, c(0.8, 0.75, 0.7, 0.65), opacity := 0.2) %>%
    add_points(seq_df = seq_df, data = points, name, position, value,
               0.85, 0.6, metadata = data.frame(key = 1:60), size := 15, key := ~key, fill := "black", fill.hover := "red") %>%
    add_links(seq_df = seq_df, data = links, chr_from, chr_to,
              pos_from, pos_to, 0.6, 0.6, 0) %>%
    add_ribbons(seq_df = seq_df, data = ribbons, chr_from, chr_to,
                pos_from_start, pos_from_end, pos_to_start, pos_to_end, 0.6, 0.6, 0, fill = ~name_from, opacity := 0.6) %>%
    add_tooltip(tooltip_fun) %>%
    handle_click(click_handle) %>%
    hide_axis("x") %>%
    hide_axis("y") %>%
    hide_legend("fill") %>%
    set_options(duration = 1000) %>%
    bind_shiny("plot")
  
}

