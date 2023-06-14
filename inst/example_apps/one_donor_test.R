
library(ggcircos)
library(dplyr)

data_path <- "example_apps/icgc_example/clean_data"

donors_data <- system.file(data_path, "donors_clean.csv", package = "ggcircos")
snps_data <- system.file(data_path, "snp_clean.csv", package = "ggcircos")
struct_data <- system.file(data_path, "struct_clean.csv", package = "ggcircos")
cnv_data <- system.file(data_path, "cnv_clean.csv", package = "ggcircos")
genes_data <- system.file(data_path, "genes.csv", package = "ggcircos")

donors <- read.csv(donors_data)

snp <- read.csv(snps_data)
struct <- read.csv(struct_data)
cnv <- read.csv(cnv_data)

genes <- read.csv(genes_data)


## find which donors have all types of data (42)
use_donors <- intersect(unique(donors$icgc_donor_id), unique(snp$icgc_donor_id))

## filter to just these donors
donors <- donors %>% filter(icgc_donor_id %in% use_donors)
snp <- snp %>% filter(icgc_donor_id %in% use_donors)
struct <- struct %>% filter(icgc_donor_id %in% use_donors)
cnv <- cnv %>% filter(icgc_donor_id %in% use_donors)



## for initial testing choose one donor (male)
one_donor <- donors$icgc_donor_id[1]


test_donors <- donors %>% filter(icgc_donor_id == one_donor)
test_snp <- snp %>% filter(icgc_donor_id == one_donor)
test_struct <- struct %>% filter(icgc_donor_id == one_donor)
test_cnv <- cnv %>% filter(icgc_donor_id == one_donor)

## cnv data from two different specimens
# unique(test_cnv$icgc_specimen_id)

## for the moment choose cnv data which comes from the same specimen as snp and struct
test_cnv <- test_cnv %>% filter(icgc_specimen_id == "SP77315")


chroms <- c(1:22, "X", "Y")
lengths = c(249250621,243199373,198022430,191154276,180915260,171115067,
            159138663,146364022,141213431,135534747,135006516,133851895,
            115169878,107349540,102531392,90354753,81195210,78077248,
            59128983,63025520,48129895,51304566,155270560,59373566)

radians <- create_radians(chroms, lengths)
track_radians <- create_track_radians(radians, points_per_track = rep(20, 24))
seq_df <- create_seq_df(radians)



## CNV
# get midpoints of cnvs for the moment
test_cnv <- test_cnv %>% mutate(pos = (chromosome_end + chromosome_start) / 2)

cnv_plot_data <- fit_to_seq(test_cnv$chromosome, test_cnv$pos, seq_df, metadata = test_cnv[, c("copy_number", "mutation_type")])


cnv_inner <- 0.8
cnv_outer <- 0.9
## this might become a new function (fit_ticks?)
cnv_plot_data <- data.frame(rbind(cnv_plot_data, cnv_plot_data),
                            r = c(rep(cnv_inner, nrow(cnv_plot_data)), rep(cnv_outer, nrow(cnv_plot_data))))


## TEXT
text_df <- data.frame(name = chroms, pos = lengths / 2)



## RNA


## STRUCT

struct_plot_data <- test_struct %>% filter(chr_from != chr_to | abs(chr_from_bkpt - chr_to_bkpt) > 10^6)

## SNP
snp_plot_data <- test_snp %>% group_by(chromosome, chromosome_start, gene_affected, mutation_type) %>% summarise(transcripts = n())

max_affected <- max(snp_plot_data$transcripts)

## crude way to limit number of axis lines
n_circles <- ceiling(max_affected / ceiling(max_affected / 10))




plot <- ggvis() %>%
  add_track(track_radians, 1, 0.9, fill = ~group, fillOpacity := 0.5, fillOpacity.hover := 1) %>%
  add_text(seq_df = seq_df, data = text_df, seq = name, position = pos, label = name, r = 1.05,
           align := "center", baseline := "middle") %>%
  add_links(seq_df = seq_df, data = struct_plot_data, name_from = chr_from, name_to = chr_to,
            pos_from = chr_from_bkpt, pos_to = chr_to_bkpt, 0.6, 0.6, 0, stroke = ~inter) %>%
  add_track(track_radians, 0.8, 0.6, strokeOpacity := 0.5) %>%
  add_points(seq_df = seq_df, data = snp_plot_data, seq = chromosome, position = chromosome_start, value = transcripts, 0.8, 0.6,
             metadata = data.frame(mutation_type = snp_plot_data$mutation_type), min_value = 0, max_value = max_affected + 1,
             size := 7, size.hover := 14, fill = ~mutation_type, stroke := "black", strokeWidth := 1) %>%
  add_circles(track_radians, r = seq(0.6, 0.8, length.out = n_circles + 2), opacity := 0.2) %>%
  layer_paths(data = cnv_plot_data %>% group_by(theta), ~sin(theta) * r, ~cos(theta) * r, stroke = ~factor(copy_number)) %>%
  hide_axis("x") %>%
  hide_axis("y") %>%
  hide_legend(c("fill", "stroke"))

print(plot)



