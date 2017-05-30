####################################################################
# BEFORE RUNNING FILE YOU HAVE TO UNZIP vuzopedia.zip in data folder
####################################################################


# Get five main colors of each logo and combine in one plot

# For reproducibility
set.seed(1610)

# Create empty object to store clustering results
km_each <- list()

#####
# dfs object is created in pca_logo.R
#####

# Some of the logos contain only one color (which means one cluster)
# For them we write an exception inside for loop
for (i in 1:length(dfs)) {
  km_each[[i]] <- try(data.frame(cl = 1:5, 
                                 kmeans(dfs[[i]], centers = 5)$centers, 
                                 size = kmeans(dfs[[i]], centers = 5)$size,
                                 pic_id = i))
  if (inherits(km_each[[i]], "try-error")) {
    km_each[[i]] <- data.frame(cl = 1:length(unique(dfs[[i]][, 1])),
                               kmeans(dfs[[i]], centers = length(unique(dfs[[i]][, 1])))$centers,
                               size = kmeans(dfs[[i]], centers = length(unique(dfs[[i]][, 1])))$size, 
                               pic_id = i)
  }
  print(i)
}

# Combine list into single dataframe
km_each_df <- Reduce(rbind, km_each)

# Plot results
rug_logo <- ggplot(km_each_df, aes(x = factor(pic_id), y = size, fill = rgb(red, green, blue))) +
  geom_bar(stat = "identity", position = position_fill(), width = 1) +
  labs(y = "Доля индивидуального цвета, %",
       x = "Каждая вертикальная полоска соответствует одному логотипу\n
       У каждого логотипа свои основные 5 цветов",
       title = "Распределение 5 основных цветов каждого логотипа") +
  scale_y_continuous(breaks = seq(0, 1, 0.25), labels = seq(0, 100, 25)) +
  scale_fill_identity() +
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_line(color = "grey30"),
        axis.text.x = element_blank(),
        axis.text.y = element_text(family = "Roboto",
                                   size = 10, 
                                   color = "grey30"),
        axis.title.x = element_text(family = "PT Sans",
                                    size = 12, 
                                    color = "grey30",
                                    margin = margin(t = -20, unit = "pt")),
        axis.title.y = element_text(family = "PT Sans",
                                    size = 12, 
                                    color = "grey30"),
        plot.title = element_text(family = "PT Sans",
                                  size = 16, 
                                  color = "grey30",
                                  margin = margin(b = -10, unit = "pt"), 
                                  hjust = 0.5))

# Save results
ggsave("plots/logos/each_logo_rug.png", rug_logo, width = 7, height = 7, units = "in")
