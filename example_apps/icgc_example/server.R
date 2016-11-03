
library(ggcircos)
library(shiny)


donors <- read.csv("clean_data/donors_clean.csv")

donors <- filter(donors, donor_sex == "male")

snps <- read.csv("clean_data/snp_clean.csv")
struct <- read.csv("clean_data//struct_clean.csv")
cnv <- read.csv("clean_data/cnv_clean.csv")

chrom_lengths <- c(249250621,243199373,198022430,191154276,180915260,171115067,
                   159138663,146364022,141213431,135534747,135006516,133851895,
                   115169878,107349540,102531392,90354753,81195210,78077248,
                   59128983,63025520,48129895,51304566,155270560,59373566)

chroms <- c(1:22, "x", "Y")

initial_radians <- create_radians()


server <- function(input, output, session) {
  
  re_values <- reactiveValues(scaling_factors = rep(1, 24), previous_radians = initial_radians, chrom_clicked = "1")
  
  
  radians <- reactive({
    
    rads <- create_radians(lengths = chrom_lengths * re_values$scaling_factors)
    
    isolate(mid <- mean(rads[names(rads) == re_values$chrom_clicked]))
    
    isolate(prev_mid <- mean(re_values$previous_radians[names(rads) == re_values$chrom_clicked]))
    
    offset <- mid - prev_mid
    
    rads - offset
    
  })
  
  track_radians <- reactive({
    
    create_track_radians(radians())
    
  })
  
  seq_df <- reactive({
    
    create_seq_df(radians(), scale = re_values$scaling_factors)
    
  })

  
  selected_donor <- reactive({
    
    filter(donors, icgc_donor_id == input$donor)
    
  })
  
  selected_struct <- reactive({
    
    filter(struct, icgc_donor_id == input$donor)
    
  })
  
  selected_cnv <- reactive({
    
    filter(cnv, icgc_donor_id == input$donor)
    
  })
  
  
  click_handle <- function(data, location, session) {
    
    if(is.null(data)) return(NULL)
    
    isolate(re_values$chrom_clicked <- data$group)
    
    isolate(re_values$previous_radians <- radians())
    
    isolate(re_values$scaling_factors[which(chroms == data$group)] <- ifelse(re_values$scaling_factors[which(chroms == data$group)] == 1,
                                                                             3, 1))
    
  }
  
  
  tooltip_fun <- function(data) {
    
    str(data)
    
    if ("value" %in% names(data)) {
      
      paste("Copy Number:" ,data$copynumber)
      
    }
    
  }
  
  
  observe({
  ggvis() %>%
    add_track(track_radians, 1, 0.9, fill = ~group) %>%
    add_track(track_radians, 0.85, 0.6) %>%
    add_circles(track_radians, r = seq(0.85, 0.6, length.out = 9)[-c(1, 9)], strokeOpacity := 0.2) %>%
    add_points(seq_df = seq_df, data = selected_cnv, chromosome, (chromosome_start + chromosome_end) / 2, copy_number, 0.85, 0.6,
               min_value = 0, max_value = 8, size := 10, fill := "black", fill.hover := "red") %>%
    #add_lines(seq_df = seq_df, data = selected_cnv)
    add_links(seq_df = seq_df, data = selected_struct, chr_from, chr_to, chr_from_bkpt, chr_to_bkpt, 0.59, 0.59, 0.1,
              stroke = ~inter) %>%
    add_tooltip(tooltip_fun) %>%
    handle_click(click_handle) %>%
    hide_axis("x") %>%
    hide_axis("y") %>%
    hide_legend(c("fill", "stroke")) %>%
    bind_shiny("circos")
  })
  
}










