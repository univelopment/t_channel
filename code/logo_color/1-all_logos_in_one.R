####################################################################
# BEFORE RUNNING FILE YOU HAVE TO UNZIP vuzopedia.zip in data folder
####################################################################


# Analysis of all logos as a big one

#### GIF DISCLAIMER####
#### GIF DISCLAIMER####
# Initially some of the logos were in gif format (about 10 of them)
# I chose to convert them to png manually
# Because of them some parts of code below are commented
#### GIF DISCLAIMER####
#### GIF DISCLAIMER####

# Libraries
library("ggplot2")
library("png")
library("jpeg")
library("EBImage")  # package from bioconductor for image manipulations

# List filenames ----
all_pics <- list.files("data/vuzopedia_data/")

# Remove subsidiaries
all_pics <- all_pics[!grepl("[Фф]илиал", all_pics)]

# Read files in ----
all_pics_data <- list()

for (i in 1:length(all_pics)) {
  all_pics_data[[i]] <- try(readPNG(paste0("vuzopedia_data/", all_pics[i])), silent = TRUE)
  if (inherits(all_pics_data[[i]], "try-error")) {
    all_pics_data[[i]] <- try(readJPEG(paste0("vuzopedia_data/", all_pics[i])), silent = TRUE)
    if (inherits(all_pics_data[[i]], "try-error")) {
      # Gifs were manually converted that is why lines below are commented now
      # all_pics_data[[i]] <- try(read.gif(paste0("vuzopedia_data/", all_pics[i])), silent = TRUE)
      # if (inherits(all_pics_data[[i]], "try-error")) { 
        all_pics_data[[i]] <- "unkwn_format"
      # }
    }
  }
}

# Now we get no errors when reading logos in (previous troubles listed below)
all_pics[which(sapply(all_pics_data, is.character))]

# Initially troublesome files (marked as png, but can't be read using it)
# Had to manually resave them
# Anyway because of them I caught 2 more subsidiaries
# Also I removed MAI's training centre and RUSDISTANT centre
# [1] "Государственная_академия_славянской_культуры.png"                              
# [2] "Донской_государственный_технический_университет.png"                           
# [3] "Лесосибирский_педагогический_институт_Сибирского_федерального_университета.png"
# [4] "Российский_государственный_университет_правосудия.png"                         
# [5] "Сибирский_федеральный_университет.png"                                         
# [6] "Хакасский_технический_институт_Сибирского_федерального_университета.png"   

# Analyse dimensions of data (e.g. all pictures are correct) ----
dms <- lapply(all_pics_data, dim)

# Their length (got two greyscale pics with two dimensions and detected gifs)
# Now no gifs are detected (see comments in the beginning of the script)
dms_len <- sapply(dms, length)
table(dms_len)

# At first check pics with 0 dimensions. They were lists,
# because gif is read to list. Converted them manually to png for easier analysis
# gif_dim <- all_pics_data[which(dms_len == 0)]

# Greyscale images
grsc_dim <- all_pics_data[which(dms_len == 2)]

# Convert them to RGB
all_pics_data[[84]] <- toRGB(all_pics_data[[84]])@.Data
all_pics_data[[604]] <- toRGB(all_pics_data[[604]])@.Data
all_pics_data[[619]] <- toRGB(all_pics_data[[619]])@.Data

# Analyse dimensions of files. Resize those that are above 40k pixels ----
pics_dims_before <- data.frame(t(sapply(all_pics_data, dim)))

pics_dims_before[, 4] <- pics_dims_before[, 1] * pics_dims_before[, 2]

# 1/4 to the size of images that are larger than 40k pixels
all_pics_data[which(pics_dims_before[, 4] > 40000)] <- lapply(which(pics_dims_before[, 4] > 40000), 
                                               function(x) resize(all_pics_data[[x]], 
                                                                  dim(all_pics_data[[x]])[1]/4))

# Make one very big picture even smaller
all_pics_data[[151]] <- resize(all_pics_data[[151]], dim(all_pics_data[[151]])[1]/2)

# Double check
pics_dims_after <- data.frame(t(sapply(all_pics_data, dim)))

pics_dims_after[, 4] <- pics_dims_after[, 1] * pics_dims_after[, 2]

# Convert arrays to data.frames ----
dfs <- list()

for (i in 1:length(all_pics_data)) {
  dfs[[i]] <- data.frame(
    red = c(all_pics_data[[i]][ , , 1]),
    green = c(all_pics_data[[i]][ , , 2]),
    blue = c(all_pics_data[[i]][ , , 3])
    )
}

# Check their dimensions
df_dims <- t(sapply(dfs, dim))

# Check distribution of pictures' definiton
# Not quite uniform. But random sampling of pixels will help 
# to overcome non-uniformity influence on final result
ggplot(data = data.frame(df_dims), aes(x = X1)) +
  geom_density() +
  theme_bw()

###################################################################
# Aggregate list to big dataframe ----
# CASE WITH NO SAMPLING. IGNORING DIFFERENCES IN DIMENSION BETWEEN PICS
# (NOT THE MOST ACCURATE WAY TO CHECK COLORS)
###################################################################

# dfs_ag <- Reduce(rbind, dfs)

# Compute PCs ----
# dfs_ag_pc <- prcomp(dfs_ag, scale = TRUE, center = TRUE)

# 98% of variance for first two components
# summary(dfs_ag_pc)

# Extract them
# dfs_ag$pc1 <- dfs_ag_pc$x[,1]
# dfs_ag$pc2 <- dfs_ag_pc$x[,2]

# Final plot ----
# pl <- ggplot(dfs_ag, aes(x = pc1, y = pc2, color = rgb(red, green, blue))) + 
#   geom_point(size = 2 ) +
#   scale_color_identity() +
#   theme_void()
# 
# ggsave("new_data.png", pl, width = 15, height = 15, units = "cm")


###################################################################
# SAMPLE 1000 pixels from each picture ----
# To overcome problem with non-uniform distribion of dimensions
###################################################################

# Function to sample random pixels
df_row_sampler <- function(df, n_pixels = 2500) {
  df <- df[sample(1:nrow(df), n_pixels), ]
  return(df)
}

# Make plots reproducable (because of sampling and kmeans)
set.seed(42)

# Sample pixels from dataframes
# Two sample sizes (1000 and 2500) // Choose smaller one

# dfs_sample <- lapply(dfs, df_row_sampler)
dfs_sample_sm <- lapply(dfs, df_row_sampler, 1000)

# Aggregate df
# dfs_sample_ag <- Reduce(rbind, dfs_sample)
dfs_sample_ag_sm <- Reduce(rbind, dfs_sample_sm)

# Retreive main colors
dfs_km10 <- kmeans(dfs_sample_ag_sm, centers = 10)
dfs_km7 <- kmeans(dfs_sample_ag_sm, centers = 7)

# Add cluster labels to dataframe
dfs_sample_ag_sm$cluster10 <- dfs_km10$cluster
dfs_sample_ag_sm$cluster7 <- dfs_km7$cluster

# Add cluster centers to dataframe
dfs_cent10 <- data.frame(cluster10 = 1:nrow(dfs_km10$centers), dfs_km10$centers)
colnames(dfs_cent10) <- c("cluster10", "red10", "green10", "blue10")

dfs_cent7 <- data.frame(cluster7 = 1:nrow(dfs_km7$centers), dfs_km7$centers)
colnames(dfs_cent7) <- c("cluster7", "red7", "green7", "blue7")

dfs_sample_ag_sm <- left_join(dfs_sample_ag_sm, dfs_cent10, by = "cluster10")
dfs_sample_ag_sm <- left_join(dfs_sample_ag_sm, dfs_cent7, by = "cluster7")

# Compute PCs ----
# dfs_sample_ag_pc <- prcomp(dfs_sample_ag, scale = TRUE, center = TRUE)
dfs_sample_ag_sm_pc <- prcomp(dfs_sample_ag_sm[, 1:3], scale = TRUE, center = TRUE)

# 98% of variance for first two components
summary(dfs_sample_ag_sm_pc)

# Extract them
# dfs_sample_ag$pc1 <- dfs_sample_ag_pc$x[,1]
# dfs_sample_ag$pc2 <- dfs_sample_ag_pc$x[,2]

dfs_sample_ag_sm$pc1 <- dfs_sample_ag_sm_pc$x[,1]
dfs_sample_ag_sm$pc2 <- dfs_sample_ag_sm_pc$x[,2]

#  Create final plot ----
# pl_sample <- ggplot(dfs_sample_ag, aes(x = pc1, y = pc2, color = rgb(red, green, blue))) + 
#   geom_point(size = 2 ) +
#   scale_color_identity() +
#   theme_void()

# All colors
pl_sample_sm <- ggplot(dfs_sample_ag_sm, aes(x = pc1, y = pc2, color = rgb(red, green, blue))) + 
  geom_point(size = 2 ) +
  scale_color_identity() +
  theme_void()

# 10 main colors
pl_sample_sm10 <- ggplot(dfs_sample_ag_sm, aes(x = pc1, y = pc2, color = rgb(red10, green10, blue10))) + 
  geom_point(size = 2 ) +
  scale_color_identity() +
  theme_void()

# 7 main colors
pl_sample_sm7 <- ggplot(dfs_sample_ag_sm, aes(x = pc1, y = pc2, color = rgb(red7, green7, blue7))) + 
  geom_point(size = 2 ) +
  scale_color_identity() +
  theme_void()

# Title font style: common parameter for each plot
gp <- gpar(fontfamily = "PT Sans", fontsize = 20)

# Save final plot ----
png("plots/logos/all_sample.png", width = 1100, height = 600)
grid.newpage()
grid.arrange(arrangeGrob(pl_sample_sm, top = textGrob("Все цвета логотипов", gp = gp)),
             arrangeGrob(pl_sample_sm10, top = textGrob("10 основных цветов", gp = gp)),
             arrangeGrob(pl_sample_sm7, top = textGrob("7 основных цветов", gp = gp)),
             nrow = 1)
dev.off()
