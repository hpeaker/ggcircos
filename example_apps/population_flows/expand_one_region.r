

## example to generalise the app flow to work with more general origins and destinations.
## ie not confined to using region_dest, region_orig in the data so we expand to the country level

library(ggvis)
library(dplyr)
library(shiny)
library(ggcircos)

expand <- "Europe" #c("Europe", "Africa")
expand <- "Africa"
expand <- c("Europe", "Africa")

# source("../R_scripts/circos_ggvis/app/track_functions.r")
# source("../R_scripts/circos_ggvis/app/transformation_functions.r")

population_flows <- read.csv("example_apps/population_flows/Data on the global flow of people_Version March2014.csv")


## region order from d3 plot
regions <- c("North America", "Africa", "Europe", "Fmr Soviet Union", "West Asia",
             "South Asia", "East Asia", "South-East Asia", "Oceania", "Latin America")
  #sort(unique(population_flows$region_orig))


## order by region then by country. This is used as a factor levels for other objects
region_order <- regions %>% 
  lapply(function(x) 
    c(as.character(x),
      as.character(sort(unique(population_flows$country_orig[population_flows$region_orig == x]))))) %>%
  do.call(what = c)


## creating the links for a flow plot as if one (or more) region had been clicked on

countries <- unique(population_flows$country_orig[population_flows$region_orig %in% expand])

region_flows <- population_flows %>%
  select(region_orig, region_dest, country_orig, country_dest, countryflow_1990:countryflow_2005) %>%
  mutate(orig = ifelse(region_orig %in% expand, as.character(country_orig), as.character(region_orig)),
         dest = ifelse(region_dest %in% expand, as.character(country_dest), as.character(region_dest))) %>%
  select(orig, dest, countryflow_1990:countryflow_2005) %>%
  group_by(orig, dest) %>%
  summarise_each(funs(sum), countryflow_1990:countryflow_2005)

region_totals_out <- region_flows %>%
  group_by(orig) %>%
  summarise_each(funs(sum), countryflow_1990:countryflow_2005) %>%
  mutate(orig = factor(orig, levels = region_order)) %>%
  arrange(orig)


region_totals_in <- region_flows %>%
  group_by(dest) %>%
  summarise_each(funs(sum), countryflow_1990:countryflow_2005) %>%
  mutate(dest = factor(dest, levels = region_order)) %>%
  arrange(dest)


link_one_region <- function(region_flows, region_totals_out, region) {
  
  region_flows %>% filter(orig == region) %>% select(1:3) %>% ungroup() %>%
    left_join(region_totals_out, by = c("dest" = "orig")) %>%
    mutate(pos_from_start = c(0, cumsum(flow)[1:(n() - 1)]), pos_from_end = cumsum(flow),
           pos_to_start = out, pos_to_end = out + flow) %>%
    select(orig, dest, flow, pos_from_start, pos_from_end, pos_to_start, pos_to_end)
  
}


flows <- region_flows[, c("orig", "dest", paste0("countryflow_", 1990))]
names(flows) <- c("orig", "dest", "flow")

totals_out <- region_totals_out[, c("orig", paste0("countryflow_", 1990))]
names(totals_out) <- c("orig", "out")

totals_in <- region_totals_in[, c("dest", paste0("countryflow_", 1990))]
names(totals_in) <- c("dest", "in")


total_flow <- (region_totals_out[, paste0("countryflow_", 1990)] + region_totals_in[, paste0("countryflow_", 1990)])[,1]


radians <- create_radians(seq = as.character(region_totals_out$orig), lengths = total_flow, total_gap = 0.2)

track_radians <- create_track_radians(radians = radians, seq = as.character(region_totals_out$orig), approx_points = 600)

seq_df <- create_seq_df(seq = as.character(region_totals_out$orig), lengths = total_flow, radians = radians)


all_regions <- do.call(rbind, lapply(region_totals_out$orig, link_one_region, region_flows = flows, region_totals_out = totals_out))
  
all_regions <- all_regions %>%
  group_by(dest) %>% 
  arrange(dest, orig) %>%
  mutate(pos_to_end = pos_to_start + cumsum(flow),
         pos_to_start = pos_to_start + lag(cumsum(flow), default = 0) - 1)
  
  ## filtering makes the app quicker but messes up the transistions
  ## This is made much better however if you group by something more permanent when plotting (group_by(name_from, name_to) instead of (group_by(link)))
all_regions <- all_regions %>% filter(flow > 50000, orig %in% countries | dest %in% countries)

all_regions <- mutate(all_regions, join = paste(orig, dest, sep = "-"))


track_df <- create_track_df(track_radians, outer = 1, inner = 0.95) %>%
  inner_join(totals_out, by = c("group" = "orig")) %>%
  inner_join(totals_in, by = c("group" = "dest"))
  
ribbons <- fit_ribbons(all_regions$orig, all_regions$dest, all_regions$pos_from_start, all_regions$pos_from_end, 
                       all_regions$pos_to_start, all_regions$pos_to_end, seq_df, 0.95, 0.9, 0) %>%
  mutate(join = paste(name_from, name_to, sep = "-")) %>%
  inner_join(all_regions, by = "join")
  


ggvis() %>% 
  layer_rects(data = data.frame(x=-1.5,y=-1.5, x2 = 1.5, y2 = 1.5), ~x, ~y, x2 = ~x2, y2 = ~ y2, fill:="#000000") %>%
  layer_paths(data = track_df %>% group_by(group), ~sin(theta) * r, ~cos(theta) * r,
              interpolate := "linear-closed", fill = ~group, strokeOpacity := 0,
              strokeWidth := 0) %>%
  layer_paths(data = ribbons %>% group_by(name_from, name_to), ~sin(theta) * r, ~cos(theta) * r,
              interpolate := "basis", fill = ~name_to, strokeOpacity := 0,
              fillOpacity := 0.8, fillOpacity.hover := 1, strokeWidth := 0) %>%
  scale_numeric("x", domain = c(-1, 1), nice = FALSE, clamp = TRUE) %>%
  scale_numeric("y", domain = c(-1, 1), nice = FALSE, clamp = TRUE) %>%
  hide_legend("fill") %>%
  hide_axis("x") %>%
  hide_axis("y")



