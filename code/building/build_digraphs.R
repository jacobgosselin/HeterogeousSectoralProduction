# loading libraries and data ----------------------------------------------

# load libraries
library(ggplot2)
library(igraph)
library(ggraph)
library(readxl)
library(zoo)
library(magick)

# clear environment
rm(list = ls())

# load Rdata
load("data/cleaned/A_Int_byyear_1997_2023.RData")
# load("data/cleaned/A_byyear_1947-1962.Rdata")
# load("data/cleaned/A_byyear_1963-1996.Rdata")
# load("data/cleaned/A_byyear_1997-2022.Rdata")
# load("data/cleaned/A_byyear_det.Rdata")
# load("data/cleaned/L_byyear_1947-1962.Rdata")
# load("data/cleaned/L_byyear_1963-1996.Rdata")
# load("data/cleaned/L_byyear_1997-2022.Rdata")
# load("data/cleaned/L_byyear_det.Rdata")
# load("data/cleaned/cross_walk_summary.Rdata")
# load("data/cleaned/cross_walk_detail.Rdata")
# load("data/cleaned/two_dig.Rdata")

# defining functions for digraph plots ------------------------------------

# define plot function
plot_mat <- function(mat, year){
  
  io_matrix <- as.matrix(mat[,3:(ncol(mat)-1)])
  
  # replace values <.01 with 0
  io_matrix[io_matrix < .01] <- 0

  # Extract total industry output
  total_output <- mat$total_int_output
  
  # # Recover 2-digit code 
  # if (ncol(mat) < 100) {
  #   cross_walk <- cross_walk_summary
  # } else {
  #   cross_walk <- cross_walk_detail
  # }
  # 
  # mat <- merge(mat, cross_walk, by = "Code", all.x = TRUE)
  # mat$two_dig[is.na(mat$two_dig)] <- mat$Code[is.na(mat$two_dig)]
  # mat <- merge(mat, two_dig, by = "two_dig", all.x = TRUE)
  # mat$two_dig_label[is.na(mat$two_dig_label)] <- mat$`Industry Description`[is.na(mat$two_dig_label)]
  # two_dig_label <- mat$two_dig_label
  
  # Plot a digraph of the matrix mat; spread out nodes
  g <- graph_from_adjacency_matrix(io_matrix, mode = "directed", weighted = TRUE)
  
  # Create a new attribute for the nodes that contains the internal weight of the node
  V(g)$tot_sales_down <- as.vector(total_output)
  
  # Create a new attribute for the nodes that contains the first 2 digits of the column names of mat
  # V(g)$color_code <- as.vector(two_dig_label)
  
  # Create a new attribute for the edges that contains the weight category
  E(g)$weight_category <- cut(E(g)$weight, breaks = 10, labels = FALSE)
  
  # Calculate percentiles for edge weights
  weights <- E(g)$weight
  weight_percentiles <- ecdf(weights)(weights)  # This calculates the percentile of each weight
  E(g)$weight_percentile <- weight_percentiles
  
  # simplifying plot...
  # , colour = color_code, scale_color_viridis(discrete = TRUE, option = 'B', alpha = .8, name = "2-digit industry") +
  
  # Plot the graph
  plot <- ggraph(g, layout = 'linear', circular = TRUE) +
    geom_edge_link(aes(edge_colour = weight), alpha = 0.5) +
    geom_node_point(aes(size = tot_sales_down), alpha = 0.5) +
    theme_void() +
    scale_edge_colour_gradient(low = "darkgrey", high = "black", name = "Weight Percentile")  +
    scale_size_continuous(name = "Total downstream sales") +
    coord_flip() +
    guides(colour = FALSE) +
    theme(legend.position = "none", text = element_text(family = "serif", size = 24)) +
    labs(caption = "Source: BEA, Make and Use Tables",
         title = year)
  
  return(plot)
}

# save plot wrapper for summary IO digraphs
save_plot <- function(mat, year, level, type) {
  # file_path = paste0("figures/digraphs", type, "/", level, "/", type, "_", level, "_", year, ".pdf") (use for all year, A+L)
  file_path = paste0("figures/digraphs/", type, "_", level, "_", year, ".pdf")
  plot <- plot_mat(mat, year)
  ggsave(file_path, plot = plot, device = "pdf", width = 9, height = 9) 
}

# only need 1997 plot for paper

save_plot(A_Int_byyear_1997_2023[['1997']], '1997', 'summary', 'A')



# Saving digraph plots A and L matrices --------------------------------------------------------------------
# 
# for each item in A_byyear summary (1963-1996 and 1997-2022), save_plot with year and A
# for (i in 1:length(A_byyear_1947_1962)) {
  # save_plot(A_byyear_1947_1962[[i]], names(A_byyear_1947_1962)[i], "summary", "A")
  # save_plot(L_byyear_1947_1962[[i]], names(L_byyear_1947_1962)[i], "summary", "L")
# }

# for (i in 1:length(A_byyear_1963_1996)) {
  # save_plot(A_byyear_1963_1996[[i]], names(A_byyear_1963_1996)[i], "summary", "A")
  # save_plot(L_byyear_1963_1996[[i]], names(L_byyear_1963_1996)[i], "summary", "L")
# }

# for (i in 1:length(A_byyear_1997_2022)) {
  # save_plot(A_byyear_1997_2022[[i]], names(A_byyear_1997_2022)[i], "summary", "A")
  # save_plot(L_byyear_1997_2022[[i]], names(L_byyear_1997_2022)[i], "summary", "L")
# }
 
# for (i in 1:length(A_byyear_det)) {
#   save_plot(A_byyear_det[[i]], names(A_byyear_det)[i], "detailed", "A")
#   save_plot(L_byyear_det[[i]], names(L_byyear_det)[i], "detailed", "L")
# }

# create gifs of plots over time --------------------------------------------------------------------
# 
# # summary A plots, 1947-2022
# pdf_files <- list.files(path = "figures/digraphs/A/summary", pattern = "A_.*\\.pdf$", full.names = TRUE)
# gif_image <- image_animate(image_read(pdf_files, density = 300), fps = 5)
# image_write(gif_image, path = "figures/digraphs/A/summary/A_summary_1947-2022.gif")
# 
# # detailed A plots, 2007-2017
# pdf_files <- list.files(path = "figures/digraphs/A/detailed", pattern = "A_.*\\.pdf$", full.names = TRUE)
# gif_image <- image_animate(image_read(pdf_files, density = 300), fps = 1)
# image_write(gif_image, path = "figures/digraphs/A/detailed/A_detailed_2007-2017.gif")
# 
# # summary L plots, 1947-2022
# pdf_files <- list.files(path = "figures/digraphs/L/summary", pattern = "L_.*\\.pdf$", full.names = TRUE)
# gif_image <- image_animate(image_read(pdf_files, density = 100), fps = 5)
# image_write(gif_image, path = "figures/digraphs/L/summary/L_summary_1947-2022.gif")
# 
# # detailed L plots, 2007-2017
# pdf_files <- list.files(path = "figures/digraphs/L/detailed", pattern = "L_.*\\.pdf$", full.names = TRUE)
# gif_image <- image_animate(image_read(pdf_files, density = 300), fps = 1)
# image_write(gif_image, path = "figures/digraphs/L/detailed/L_detailed_2007-2017.gif")