library(dplyr)
library(ggvis)


population_flows <- read.csv("population_flows/Data on the global flow of people_Version March2014.csv")

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

total_flow_1990 <- region_totals_out$total_flow_1990 + region_totals_in$total_flow_1990

radians_1990 <- create_radians(names = region_totals_out$region_orig, lengths = total_flow_1990)
track_radians_1990 <- create_track_radians(radians_1990, names = region_totals_out$region_orig)
seq_df_1990 <- create_seq_df(names = region_totals_out$region_orig, lengths = total_flow_1990, radians = radians_1990)



link_one_region <- function(region) {
  
  region_flows %>% filter(region_orig == region) %>% select(1:3) %>% ungroup() %>%
  left_join(region_totals_out, by = c("region_dest" = "region_orig")) %>%
  mutate(pos_from_start = c(0, cumsum(regionflow_1990)[1:9]), pos_from_end = cumsum(regionflow_1990) - 1,
         pos_to_start = total_flow_1990, pos_to_end = total_flow_1990 + regionflow_1990) %>%
  select(region_orig, region_dest, regionflow_1990, pos_from_start, pos_from_end, pos_to_start, pos_to_end)
  
}



africa_out_1990 <- link_one_region("Africa")



ggvis() %>% add_track(track_radians_1990, outer = 1, inner = 0.95, fill = ~group) %>%
  add_ribbons(africa_out_1990$region_orig, africa_out_1990$region_dest, africa_out_1990$pos_from_start, africa_out_1990$pos_from_end,
              africa_out_1990$pos_to_start, africa_out_1990$pos_to_end, seq_df_1990, 0.95, 0.9,
              fill = ~name_to, strokeOpacity := 0, fillOpacity := 0.8, fillOpacity.hover := 1)



all_regions_1990 <- do.call(rbind, lapply(region_totals_out$region_orig, link_one_region))


## move all pos_to_starts and pos_to_ends along by the sums of the previous (alphabetic) ribbons

all_regions_1990 <- all_regions_1990 %>%
  group_by(region_dest) %>% 
  arrange(region_dest, region_orig) %>%
  mutate(pos_to_end = pos_to_start + cumsum(regionflow_1990),
         pos_to_start = pos_to_start + lag(cumsum(regionflow_1990), default = 0) - 1)


ggvis() %>% add_track(track_radians_1990, outer = 1, inner = 0.95, fill = ~group) %>%
  add_ribbons(all_regions_1990$region_orig, all_regions_1990$region_dest, all_regions_1990$pos_from_start, all_regions_1990$pos_from_end,
              all_regions_1990$pos_to_start, all_regions_1990$pos_to_end, seq_df_1990, 0.95, 0.9,
              fill = ~name_to, strokeOpacity := 0, fillOpacity := 0.8, fillOpacity.hover := 1) %>%
  hide_axis("x") %>%
  hide_axis("y")


