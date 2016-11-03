library(ggvis)
library(dplyr)
library(shiny)
library(ggcircos)
# source("../R_scripts/circos_ggvis/app/track_functions.r")
# source("../R_scripts/circos_ggvis/app/transformation_functions.r")

population_flows <- read.csv("Data on the global flow of people_Version March2014.csv")

region_flows <- population_flows %>%
  group_by(region_orig, region_dest) %>%
  summarise(regionflow_1990 = first(regionflow_1990), regionflow_1995 = first(regionflow_1995),
            regionflow_2000 = first(regionflow_2000), regionflow_2005 = first(regionflow_2005))




region_totals_out <- region_flows %>%
  group_by(region_orig) %>%
  summarise(total_flow_1990 = sum(regionflow_1990), total_flow_1995 = sum(regionflow_1995),
            total_flow_2000 = sum(regionflow_2000), total_flow_2005 = sum(regionflow_2005))

region_totals_in <- region_flows %>%
  group_by(region_dest) %>%
  summarise(total_flow_1990 = sum(regionflow_1990), total_flow_1995 = sum(regionflow_1995),
            total_flow_2000 = sum(regionflow_2000), total_flow_2005 = sum(regionflow_2005))


link_one_region <- function(region_flows, region_totals_out, region) {
  
  region_flows %>% filter(region_orig == region) %>% dplyr::select(1:3) %>% ungroup() %>%
    left_join(region_totals_out, by = c("region_dest" = "region_orig")) %>%
    mutate(pos_from_start = c(0, cumsum(regionflow)[1:9]), pos_from_end = cumsum(regionflow) - 1,
           pos_to_start = total_flow, pos_to_end = total_flow + regionflow) %>%
    dplyr::select(region_orig, region_dest, regionflow, pos_from_start, pos_from_end, pos_to_start, pos_to_end)
  
}



server <- function(input, output, session) {
  
  hovered_region <- reactiveValues(region = NULL)
  
  flows <- reactive({
    
    x <- region_flows[, c("region_orig", "region_dest", paste0("regionflow_", input$year))]
    names(x) <- c("region_orig", "region_dest", "regionflow")
    
    x
    
  })
  
  totals_out <- reactive({
    
    x <- region_totals_out[, c("region_orig", paste0("total_flow_", input$year))]
    names(x) <- c("region_orig", "total_flow")
    
    x
    
  })
  
  totals_in <- reactive({
    
    x <- region_totals_in[, c("region_dest", paste0("total_flow_", input$year))]
    names(x) <- c("region_dest", "total_flow_in")
    
    x
    
  })
  
  total_flow <- reactive({
    (region_totals_out[, paste0("total_flow_", input$year)] + region_totals_in[, paste0("total_flow_", input$year)])[,1]
  })
  
  radians <- reactive({
    create_radians(seq = as.character(region_totals_out$region_orig), lengths = total_flow(), total_gap = 0.2)
  })
  
  track_radians <- reactive({
    create_track_radians(radians = radians(), seq = as.character(region_totals_out$region_orig), approx_points = 400)
  })
  
  seq_df <- reactive({
    create_seq_df(radians = radians())
  })
  
  all_regions <- reactive({
    
    data <- do.call(rbind, lapply(region_totals_out$region_orig, link_one_region, region_flows = flows(), region_totals_out = totals_out()))
    
    data <- data %>%
      group_by(region_dest) %>% 
      arrange(region_dest, region_orig) %>%
      mutate(pos_to_end = pos_to_start + cumsum(regionflow),
             pos_to_start = pos_to_start + lag(cumsum(regionflow), default = 0) - 1)
    
    ## filtering makes the app quicker but messes up the transistions
    ## This is made much better however if you group by something more permanent when plotting (group_by(name_from, name_to) instead of (group_by(link)))
    data %>% filter(regionflow > 50000)
    
  })
  
  track_df <- reactive({
    
    create_track_df(track_radians(), outer = 1, inner = 0.95) %>%
      inner_join(totals_out(), by = c("group" = "region_orig")) %>%
      inner_join(totals_in(), by = c("group" = "region_dest"))
    
  })
  
  ribbons <- reactive({
    
    x <- all_regions()
    
    ribb <- fit_ribbons(x$region_orig, x$region_dest, x$pos_from_start, x$pos_from_end, x$pos_to_start, x$pos_to_end, seq_df(), 0.95, 0.9, 0) %>%
      mutate(join = paste(name_from, name_to, sep = "-"))
    
    val <- x %>% mutate(join = paste(region_orig, region_dest, sep = "-"))
    
    ribb <- ribb %>% inner_join(val, by = "join")
    
    if (is.null(hovered_region$region)) {
    
      ribb$fill_opac <- 0.8
    
    } else {
      
      ribb$fill_opac <- ifelse(ribb$name_from %in% hovered_region$region | ribb$name_to %in% hovered_region$region,
                               0.8, 0)
  
    }
    
    ribb
      
      
  })
  
#   text_df <- reactive({
#     
#     radians <- radians()
#     
#     mids <- sapply(unique(names(radians)), function(x) mean(radians[names(radians) == x]))
#     
#     data.frame(theta = mids, r = 1.1, label = names(mids))
#     
#   })
  
  
  hover_region_fun_over <- function(data, ...) {
    
    if (length(names(data)) == 5) {
      
      hovered_region$region <- data$group
      
    }
    
  }
  
  hover_region_fun_out <- function(...) {
    
    hovered_region$region <- NULL
    
  }
  
  
  hover_tooltip_fun <- function(x) {
    
    #paste(names(x), collapse = ",")
    
    
    # hover over ribbon
    if (length(names(x)) == 6) {
      
      paste(
        paste(x$name_from, x$name_to, sep = " &rarr; "),
        "<br> <center>",
        prettyNum(x[,"regionflow/10^10"] * 10^10, big.mark = ","),
        "</center>"
      )
      
    # hover over outer track  
    } else {
      
      paste(
        x$group,
        "<br> Total Out:",
        prettyNum(x[, "total_flow/10^10"] * 10^10, big.mark = ","),
        "<br> Total In:",
        prettyNum(x[, "total_flow_in/10^10"] * 10^10, big.mark = ",")
      )
    }
  }
  
  
  ggvis() %>% 
    layer_paths(data = track_df %>% group_by(group), ~sin(theta) * r, ~cos(theta) * r,
                interpolate := "linear-closed", fill = ~group, strokeOpacity := ~total_flow_in/10^10,
                strokeWidth := ~total_flow/10^10) %>%
    layer_paths(data = ribbons %>% group_by(name_from, name_to), ~sin(theta) * r, ~cos(theta) * r,
                interpolate := "basis", fill = ~name_to, strokeOpacity := 0,
                fillOpacity := ~fill_opac, fillOpacity.hover := 1, strokeWidth := ~regionflow/10^10) %>%
    #layer_text(data = text_df, ~sin(theta) * r, ~cos(theta) * r, text := ~label, angle := ~theta * 180/pi, align := "center") %>%
    add_tooltip(hover_tooltip_fun, "hover") %>%
    handle_hover(hover_region_fun_over, hover_region_fun_out) %>%
    hide_axis("x") %>%
    hide_axis("y") %>%
    hide_legend("fill") %>%
    set_options(width = 800, height = 800, keep_aspect = TRUE) %>%
    bind_shiny("population_flows")
    
  
  
}

