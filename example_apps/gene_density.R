library(biomaRt)
library(dplyr)



mart <- useMart(biomart="ensembl", dataset="hsapiens_gene_ensembl")

genes <- getBM(attributes = c("ensembl_gene_id", "chromosome_name", "start_position", "end_position", "hgnc_symbol"),
               filters = c("chromosome_name"),
               values = c(1:22, "X", "Y"),
               mart = mart)


chroms <- c(1:22, "X", "Y")
lengths <- c(249250621,243199373,198022430,191154276,180915260,171115067,
             159138663,146364022,141213431,135534747,135006516,133851895,
             115169878,107349540,102531392,90354753,81195210,78077248,
             59128983,63025520,48129895,51304566,155270560,59373566)


# remove genes which have positions greater than these lengths
w <- c()
for (i in 1:24) {
  
  w <- c(w, which(genes$chromosome_name == chroms[i] & ((genes$start_position + genes$end_position) / 2) >= lengths[[i]]))
  
}

genes <- genes[-w, ]


dummy_frame <- data.frame(ensembl_gene_id = NA,
                          chromosome_name = chroms, start_position = lengths,
                          end_position = lengths, hgnc_symbol = NA)

genes <- genes %>% rbind(dummy_frame)

get_gene_counts <- function(n = 5e+7) {
  
  bands_per_chrom <- ceiling(lengths / n)
  
  dummy_positions <- do.call(c, sapply(bands_per_chrom, function(x) seq(from = 1, by = n, length.out = x) + 1))
  
  genes %>%
    filter(chromosome_name %in% chroms) %>%
    transform(chromosome_name = factor(chromosome_name, levels = chroms)) %>%
    mutate(mid_pos = (start_position + end_position) / 2) %>%
    rbind(data.frame(ensembl_gene_id = NA,
                     chromosome_name = rep(chroms, bands_per_chrom), start_position = NA,
                     end_position = NA, hgnc_symbol = "NA",
                     mid_pos = dummy_positions)) %>%
    mutate(pos_bins = cut(mid_pos, seq(from = 1, by = n, length.out = 1 + ceiling((2.5e+8) / n)))) %>%
    group_by(chromosome_name, pos_bins) %>%
    summarise(n_genes = sum(!is.na(hgnc_symbol)) - 1)
  
}



get_lengths <- function(n = 5e+7) {
  
  do.call(c, sapply(lengths, function(x) c(rep(n, floor(x/(n))), x - ((n) * floor(x/(n))))))
  
}




ns <- c(5e+7, 4.5e+7, 4e+7, 3.5e+7, 3e+7, 2.5e+7, 2e+7, 1.8e+7, 1.6e+7, 1.4e+7, 1.2e+7, 1e+7,
        9e+6, 8e+6, 7e+6, 6e+6, 5e+6, 4e+6, 3e+6, 2e+6, 1e+6)
outers = rev(c(1, 0.96, 0.92, 0.88, 0.84, 0.8, 0.76, 0.72, 0.68, 0.64, 0.6,
               0.56, 0.52, 0.48, 0.44, 0.4, 0.36, 0.32, 0.28, 0.24, 0.2))
inners = rev(c(0.96, 0.92, 0.88, 0.84, 0.8, 0.76, 0.72, 0.68, 0.64, 0.6, 0.56,
               0.52, 0.48, 0.44, 0.4, 0.36, 0.32, 0.28, 0.24, 0.2, 0.16))

counts_list <- lapply(ns, get_gene_counts)
lengths_list <- lapply(ns, get_lengths)

radians_list <- lapply(1:length(ns), function(i) create_radians(paste0(letters[i], 1:length(lengths_list[[i]])), lengths_list[[i]], total_gap = 0))

track_radians_list <- lapply(radians_list, create_track_radians)


track_df_list <- lapply(1:length(ns), function(i) {
    df <- create_track_df(track_radians_list[[i]], outers[i], inners[i])
    df$n_genes <- rep(counts_list[[i]]$n_genes, each = nrow(df)/length(lengths_list[[i]]))
    df$density <- rep(1000 * counts_list[[i]]$n_genes / lengths_list[[i]], each = nrow(df)/length(lengths_list[[i]]))
    df
  }
)




p <- ggvis()



for (i in 1:length(ns)) {
  
  p <- p %>% layer_paths(data = track_df_list[[i]] %>% group_by(group), ~sin(theta) * r, ~cos(theta) * r, fill = ~density,
                         stroke = ~density)
  
}
p











