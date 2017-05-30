####################################################################
# BEFORE RUNNING FILE YOU HAVE TO UNZIP vuzopedia.zip in data folder
####################################################################


# Color contained in logos of Russian universities
# Logo source: https://vuzopedia.com

# Much of the code for color analysis was taken from here
# https://www.r-bloggers.com/color-quantization-in-r/


# Load libraries
library("png")
library("ggplot2")
library("grid")
library("gridExtra")
library("dplyr")

# Read picture ----
mglu <- readPNG("data/vuzopedia_data/Московский_государственный_лингвистический_университет.png")

# Convert to data.frame. Each row is pixel and columns contain proportions
mglu_df <- data.frame(
  red = c(mglu[, , 1]),
  green = c(mglu[, , 2]),
  blue = c(mglu[, , 3])
)

# Find color clusters. Try different number of clusters (different number of main colors)
set.seed(1610)
mglu_kms <- lapply(seq(5, 20, 5), function(x) kmeans(mglu_df, x))

# Retrieve cluster IDs and generate data.frame with them ----

# Cluster ID columns names
cl_ids <- paste0("cl", seq(5, 20, 5))

# Get cluster splits, name them and combine into dataframe
cluster_ids <- as.data.frame(setNames(lapply(1:length(mglu_kms), function(x) mglu_kms[[x]]$cluster),
                        cl_ids))

# Retrieve cluster centers for each type of cluster split (5, 10, 15, 20) ----
cluster_cents <- setNames(
  # List of dataframes with correct column names
  lapply(1:length(mglu_kms), function(x) { 
      data.frame(1:nrow(mglu_kms[[x]]$centers),mglu_kms[[x]]$centers)
  }), 
  # Second argument to setNames
  paste0("cl_cent", seq(5, 20, 5))
)

# Generate nice column names for this sets
all_combos <- expand.grid(c("cl", "red", "green", "blue"), seq(5, 20, 5))
pretty_cnames <- paste0(all_combos[, 1], all_combos[, 2])
pretty_cnames <- split(pretty_cnames, ceiling(seq_along(pretty_cnames)/4))
rm(all_combos)

# Use these column names
for (i in 1:length(cluster_cents)){
  colnames(cluster_cents[[i]]) <- pretty_cnames[[i]]
}

# Add cluster IDs to initial set ----
mglu_df <- cbind(cluster_ids, mglu_df)

# Merge initial set with center data ----
fin_mglu_df <- list(mglu_df)
fin_mglu_df[2:(length(cluster_cents) + 1)] <- cluster_cents[1:length(cluster_cents)]
fin_mglu_df <- Reduce(left_join, fin_mglu_df)

# Compute principal components ----
mglu_pc_compl <- prcomp(dplyr::select(mglu_df, red:blue), 
                        scale = TRUE, center = TRUE)

# 99% of variance is explained
# summary(mglu_pc_compl)

# Add principal omponents to data in order to plot in 2D projection
fin_mglu_df$pc1 <- mglu_pc_compl$x[, 1]
fin_mglu_df$pc2 <- mglu_pc_compl$x[, 2]


# Function for plotting PCs ----
plot_colors_pc <- function(data, colors) {
  # Function takes dataframe and VECTOR with names of columns that contain color values
  #
  #
  # Args: data - dataframe with data
  #       colors - vector with names of columns that contain colors
  #
  #
  # Returns: scatterplot of PCs coloured by supplied variables
  #
  #
  # Example call: plot_colors_pc(your_data, c("red", "green", "blue"))
  pl <- ggplot(data = data, 
               aes(x = pc1, 
                   y = pc2,
                   color = do.call("rgb", 
                                   lapply(colors, function(x) eval(as.name(x), data))
                                   )
                   )
              ) +
    geom_point(size = 2) +
    scale_color_identity() +
    theme_void()
  return(pl)
}

# Generate plots ----
mglu_plot <- plot_colors_pc(fin_mglu_df, c("red", "green", "blue"))
mglu_plot20 <- plot_colors_pc(fin_mglu_df, c("red20", "green20", "blue20"))
mglu_plot15 <- plot_colors_pc(fin_mglu_df, c("red15", "green15", "blue15"))
mglu_plot10 <- plot_colors_pc(fin_mglu_df, c("red10", "green10", "blue10"))
mglu_plot5 <- plot_colors_pc(fin_mglu_df, c("red5", "green5", "blue5"))

# Title font style: common parameter for each plot
gp <- gpar(fontfamily = "PT Sans", fontsize = 20)

# Save plots ----
png("plots/logos/mglu_colors.png", width = 900, height = 600)
grid.newpage()
grid.arrange(arrangeGrob(rasterGrob(mglu), top = textGrob("Логотип МГЛУ", gp = gp)), 
             arrangeGrob(mglu_plot, top = textGrob("Все цвета логотипа", gp = gp)),
             arrangeGrob(mglu_plot20, top = textGrob("20 основных цветов", gp = gp)),
             arrangeGrob(mglu_plot15, top = textGrob("15 основных цветов", gp = gp)),
             arrangeGrob(mglu_plot10, top = textGrob("10 основных цветов", gp = gp)),
             arrangeGrob(mglu_plot5, top = textGrob("5 основных цветов", gp = gp)),
             nrow = 2)
dev.off()
