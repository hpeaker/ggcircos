
library(shiny)
library(ggcircos)

use_donors <- c("DO32875", "DO32878", "DO32900", "DO33091", "DO33256", "DO33336", "DO33344", "DO33368",
                "DO33376", "DO33392", "DO33400", "DO33408", "DO33480", "DO33512", "DO33528", "DO33544",
                "DO33552", "DO33600", "DO33632", "DO33656", "DO33984", "DO34240", "DO34264", "DO34288",
                "DO34312", "DO34368", "DO34376", "DO34432", "DO34448", "DO34600", "DO34608", "DO34656",
                "DO34696", "DO34736", "DO34785", "DO34793", "DO34801", "DO34809", "DO34817", "DO34849",
                "DO34905", "DO34961")


## filter datasets to these donors
## remove MT snps from snp data

data_path <- "example_apps/icgc_example/clean_data"
donors_data <- system.file(data_path, "donors_clean.csv", package = "ggcircos")
snps_data <- system.file(data_path, "snp_clean.csv", package = "ggcircos")
struct_data <- system.file(data_path, "struct_clean.csv", package = "ggcircos")
cnv_data <- system.file(data_path, "cnv_clean.csv", package = "ggcircos")

donors <- read.csv(donors_data) %>% filter(icgc_donor_id %in% use_donors)
snp <- read.csv(snps_data) %>% filter(icgc_donor_id %in% use_donors, chromosome != "MT")
struct <- read.csv(struct_data) %>% filter(icgc_donor_id %in% use_donors)
cnv <- read.csv(cnv_data) %>% filter(icgc_donor_id %in% use_donors)

##refactor snp$chromosome to remove MT level
snp$chromosome <- factor(snp$chromosome)


## snp and struct are from the same specimens. cnv has more than one specimen per donor
specimens <- unique(snp$icgc_specimen_id)

cnv <- cnv %>% filter(icgc_specimen_id %in% specimens)




chroms <- c(1:22, "X", "Y")
lengths <- c(249250621,243199373,198022430,191154276,180915260,171115067,
             159138663,146364022,141213431,135534747,135006516,133851895,
             115169878,107349540,102531392,90354753,81195210,78077248,
             59128983,63025520,48129895,51304566,155270560,59373566)

radians_f <- create_radians(chroms[1:23], lengths[1:23])
radians_m <- create_radians(chroms, lengths)




server <- function(input, output, session) {
  
  re_values <- reactiveValues(scaling_factors = rep(1, 24), previous_radians = radians_f,
                              chrom_clicked = "1", chroms_selected = NULL)
  
  radians <- reactive({
    
    donors_filt <- donors %>% filter(icgc_donor_id == input$donor)
    
    gender <- unique(donors_filt$donor_sex)
    
    rads <- create_radians(chroms, lengths * re_values$scaling_factors)
#       if(gender == "female") {
#       
#         create_radians(chroms[1:23], lengths[1:23] * re_values$scaling_factors[1:23])
#       
#       } else {
#       
#         create_radians(chroms, lengths * re_values$scaling_factors)
#       
#       }
    
    isolate(mid <- mean(rads[names(rads) == re_values$chrom_clicked]))
    
    isolate(prev_mid <- mean(re_values$previous_radians[names(rads) == re_values$chrom_clicked]))
    
    offset <- mid - prev_mid
    
    if(offset > pi/2) offset <- 0
    
    rads - offset
    
  })
  
  
  track_radians <- reactive({
    
    create_track_radians(radians(), points_per_track = rep(40, length(radians())))
    
  })
  
  seq_df <- reactive({
    
    donors_filt <- donors %>% filter(icgc_donor_id == input$donor)
    
    gender <- unique(donors_filt$donor_sex)
    
#     if (gender == "female") {
#       scale <- re_values$scaling_factors[1:23]
#     } else {
      scale <- re_values$scaling_factors
#    }
    
    create_seq_df(radians(), scale = scale)
    
  })
  
  
  snp_filt <- reactive({
    
    snp %>% filter(icgc_donor_id == input$donor)
    
  })
  
  snp_plot_data <- reactive({
    
    snp_data <- snp_filt() %>%
      group_by(chromosome, chromosome_start, chromosome_end, gene_affected,
               mutation_type, mutated_from_allele, mutated_to_allele) %>%
      summarise(transcripts = n())
      
    points <- fit_points(snp_data$chromosome, snp_data$chromosome_start, snp_data$transcripts,
                         0.8, 0.6, seq_df(),
                         metadata = snp_data[, c("transcripts", "mutation_type", "gene_affected",
                                                 "chromosome_start", "chromosome_end",
                                                 "mutated_from_allele", "mutated_to_allele")],
                         min_value = 0, max_value = max(snp_data$transcripts) + 1)
    
    points$id <- paste0("snp", 1:nrow(points))
    
    points
    
    
    
  })
  
  
  
  struct_plot_data <- reactive({
    
    
    struct_filt <- struct %>% filter(icgc_donor_id == input$donor, 
                                     chr_from != chr_to | abs(chr_from_bkpt - chr_to_bkpt) > 10^6)
    
    links <- fit_links(struct_filt$chr_from, struct_filt$chr_to, struct_filt$chr_from_bkpt, struct_filt$chr_to_bkpt,
                       seq_df(), 0.6, 0.6, 0)
    
    links <- links %>% ungroup() %>% mutate(link = as.numeric(link)) %>% arrange(link) %>% group_by(link)
    
    links$annotation <- rep(struct_filt$annotation, each = 3)
    links$pos_from <- rep(struct_filt$chr_from_bkpt, each = 3)
    links$pos_to <- rep(struct_filt$chr_to_bkpt, each = 3)
    
    links
    
    
  })
  
  
  cnv_filt <- reactive({

    cnv %>%
      filter(icgc_donor_id == input$donor) %>% 
      mutate(pos = (chromosome_end + chromosome_start) / 2)
    
  })
  
  cnv_plot_data <- reactive({
    
    cnv_filt <- cnv_filt()
    
    cnv_plot_data <- fit_to_seq(cnv_filt$chromosome, cnv_filt$pos, seq_df(),
                                metadata = cnv_filt[, c("copy_number", "mutation_type", "chromosome_start", "chromosome_end", "segment_median")])
    
    cnv_plot_data$copy_number <- paste0("cnv", cnv_plot_data$copy_number)
    
    cnv_inner <- 0.8
    cnv_outer <- 0.9
    
    cnv_plot_data <- data.frame(rbind(cnv_plot_data, cnv_plot_data),
                                r = c(rep(cnv_inner, nrow(cnv_plot_data)), rep(cnv_outer, nrow(cnv_plot_data))))
    
    cnv_plot_data$id <- paste0("cnv", 1:(nrow(cnv_plot_data)/2))
    
    cnv_plot_data %>% filter(seq %in% re_values$chroms_selected)
    
  })
  
  
  cnv_line_data <- reactive({
    
    donors_filt <- donors %>% filter(icgc_donor_id == input$donor)
    
    gender <- unique(donors_filt$donor_sex)
    
    cnv_filt <- cnv_filt() %>% group_by(chromosome) %>% arrange(pos)
    
#     if (gender == "female") {
#       
#       cnv_filt <- cnv_filt %>%
#         filter(chromosome != "Y")
#       
#     }
    
    cnv_line_data <- fit_points(cnv_filt$chromosome, cnv_filt$pos, cnv_filt$copy_number, 0.9, 0.8, seq_df())
    
    cnv_line_data
    
  })
  
  text_df <- reactive({
    
    seq_df() %>% mutate(theta = (seq_start + seq_end) / 2, r = 1.05)
    
  })
  
  tooltip_fun <- function(data) {
    
    if("mutation_type" %in% names(data)) {
      
      tt_data <- snp_plot_data()
      
      row <- tt_data[tt_data$id == data$id, ]
      
      paste0(
        
        "Start: ", row$chromosome_start, "<br>",
        "End :", row$chromosome_end, "<br>",
        "Base Change: ", row$mutated_from_allele, ">", row$mutated_to_allele, "<br>",
        "Mutation Type: ", row$mutation_type, "<br>",
        "Gene Affected: ", row$gene_affected, "<br>",
        "Number of Transcripts Affected: ", row$transcripts
                 
      )
      
    } else if ("copy_number" %in% names(data)) {
      
      tt_data <- cnv_plot_data()

      rows <- tt_data[tt_data$theta > data$theta - 0.0001 & tt_data$theta < data$theta + 0.0001, ]
      
      paste0(
        
        "Start: ", unique(rows$chromosome_start), "<br>",
        "End: ", unique(rows$chromosome_end), "<br>",
        "Segment Median ", unique(rows$segment_median), "<br>",
        "Copy Number: ", sub("cnv", "", unique(rows$copy_number)), "<br>",
        "Mutation Type: ", unique(rows$mutation_type)
      
      )
      
    } else if ("link" %in% names(data)) {
      
      tt_data <- struct_plot_data()
      
      row <- tt_data[tt_data$link == data$link, ][1,]
      
      paste0(
        
        "<center>",
        row$name_from, ":", row$pos_from, "<br>",
        "&rarr;", "<br>",
        row$name_to, ":", row$pos_to,
        "<center/>",
        "Annotation: ", row$annotation
        
        
      )
      
    }
    
    
  }
  
#   tooltip_click_fun <- function(data) {
#     
#     if ("mutation_type" %in% names(data)) {
#       
#       gene <- snp_plot_data()[snp_plot_data()$id == data$id, "gene_affected"]
#       
#       transcript_data <- snp_filt()[snp_filt()$gene_affected == gene, ]
#       
#       paste(
#         paste(transcript_data$transcript_affected, transcript_data$consequence_type, sep = ": "),
#         collapse = "<br>"
#       )
#       
#     }
#     
#   }
  
  click_handle <- function(data, location, session) {
    
    if(is.null(data)) return(NULL)
    
    if ("group" %in% names(data)) {
    
      isolate(re_values$chrom_clicked <- data$group)
    
      isolate(re_values$previous_radians <- radians())
    
      isolate(re_values$scaling_factors[which(chroms == data$group)] <- ifelse(re_values$scaling_factors[which(chroms == data$group)] == 1,
                                                                               input$scale, 1))
      
      isolate(re_values$chroms_selected <- chroms[which(re_values$scaling_factors > 1)])
      
    
    }
    #print(data)
    
  }
  
  fill_domain <- c(c(1:22, "X", "Y"), # chromosomes
                   "single base substitution", "insertion of <=200bp", "deletion of <=200bp") # snp types
  
  fill_range <- c(
    # chromosome colours from Circos
    "#996600", "#666600", "#99991E", "#CC0000", "#FF0000", "#FF00CC", "#FFCCCC", "#FF9900", "#FFCC00",
    "#FFFF00", "#CCFF00", "#00FF00", "#358000", "#0000CC", "#6699FF", "#99CCFF", "#00FFFF", "#CCFFFF",
    "#9900CC", "#CC33FF", "#CC99FF", "#666666", "#999999", "#CCCCCC",
                  
    # colours for Snps
    "red", "blue", "green"
  )
  
  
  stroke_domain <- c(c(1:22, "X", "Y"), #chromosomes
                     paste0("cnv", 0:8), # copy numbers
                     "Yes", "No") # interchromosomal or not
  
  stroke_range <- c(
    # chromosome colours from Circos
    "#996600", "#666600", "#99991E", "#CC0000", "#FF0000", "#FF00CC", "#FFCCCC", "#FF9900", "#FFCC00",
    "#FFFF00", "#CCFF00", "#00FF00", "#358000", "#0000CC", "#6699FF", "#99CCFF", "#00FFFF", "#CCFFFF",
    "#9900CC", "#CC33FF", "#CC99FF", "#666666", "#999999", "#CCCCCC",
    
    # colours for copy numbers
    "red", "red", "green", rep("red", 6),
    
    # colours for links
    "blue", "grey"
  )
  
  ggvis() %>%
    #layer_rects(data = data.frame(x=-1.5,y=-1.5, x2 = 1.5, y2 = 1.5), ~x, ~y, x2 = ~x2, y2 = ~ y2, fill:="#000000") %>%
    add_track(track_radians, 1, 0.9, fill = ~group, stroke = ~group, fillOpacity := 0.7, fillOpacity.hover := 1) %>%
    layer_paths(data = cnv_plot_data %>% group_by(theta), ~sin(theta) * r, ~cos(theta) * r, stroke = ~copy_number,
                strokeWidth := 2, strokeWidth.hover := 3) %>%
    layer_paths(data = cnv_line_data %>% group_by(seq), ~sin(theta) * r, ~cos(theta) * r, interpolate := "basis") %>%
    add_track(track_radians, 0.8, 0.6, strokeOpacity := 0.5, stroke := "black", strokeWidth := 0.5) %>%
    layer_points(data = snp_plot_data, ~sin(theta) * r, ~cos(theta) * r, fill = ~mutation_type,
                 key := ~id, size := 10, size.hover := 30, strokeOpacity := 0, strokeOpacity.hover := 1) %>%
#     add_links(seq_df = seq_df, data = struct_plot_data, name_from = chr_from, name_to = chr_to,
#               pos_from = chr_from_bkpt, pos_to = chr_to_bkpt, 0.6, 0.6, 0, stroke = ~inter,
#               strokeWidth := 1, strokeWidth.hover := 2) %>%
    layer_paths(data = struct_plot_data, ~sin(theta) * r, ~cos(theta) * r, stroke = ~inter,
                strokeWidth := 1, strokeWidth.hover := 2, interpolate := "basis") %>%
    layer_text(data = text_df, ~sin(theta) * r, ~cos(theta) * r, text := ~seq, align := "center", baseline := "middle",
               angle := ~180*(theta-pi*(cos(theta) < 0))/pi) %>%
    add_tooltip(tooltip_fun, "hover") %>%
    #add_tooltip(tooltip_click_fun, "click") %>%
    handle_click(click_handle) %>%
#     scale_numeric("x", domain = c(-1, 1), nice = FALSE, clamp = TRUE) %>%
#     scale_numeric("y", domain = c(-1, 1), nice = FALSE, clamp = TRUE) %>%
    scale_ordinal("fill", domain = fill_domain, range = fill_range) %>%
    scale_ordinal("stroke", domain = stroke_domain, range = stroke_range) %>%
    hide_axis("x") %>%
    hide_axis("y") %>%
    set_options(hover_duration = 0, width = 1000, height = 1000, keep_aspect = TRUE, duration = 1000) %>%
    bind_shiny("plot")
  
  
  
  
}

