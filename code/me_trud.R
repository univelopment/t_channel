# Employment by federal districts
# Source: http://vo.graduate.edu.ru/booklet#/?year=2014&board=4

# Load libraries ----
library("reshape2")
library("ggplot2")
library("forcats")

# Prepare data ----
d <- read.csv("data/trud_district.csv", stringsAsFactors = FALSE)

# Get the unemployment fraction
d$unemployed <- 1 - d$employed

# Transform table to wide format (for now ignore the threshold variable)
dm <- melt(d[, -3], id = c("district"))

# For ordering colors and rows
dm$variable <- fct_rev(dm$variable)
dm$district <- fct_reorder(dm$district[1:10], dm$value[1:10])

# Stacked bar ----
distr_empl <- ggplot(data = dm, aes(x = district, y = value, fill = variable)) +
  geom_bar(stat = "identity", width = 0.7, color = "white", alpha = 0.7) +
  geom_text(aes(label = value * 100), 
            position = position_fill(vjust = 0.5),
            family = "Roboto",
            color = "white",
            size = 5) +
  scale_fill_manual(name = "", 
                    values = c("#7D9C9F", "#3D4935"),
                    labels = c("Не трудоустроено", "Трудоустроено")) +
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = c(0, 50, 100)) +
  coord_flip() +
  labs(title = "Доля трудоустроенных выпускников по округу вуза, %") +
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_text(family = "Roboto", size = 14, color = "grey30"),
        legend.text = element_text(family = "Roboto", size = 12, color = "grey30"),
        legend.position = "bottom",
        legend.box.margin = margin(t = -15, unit = "pt"),
        plot.title = element_text(family = "Roboto", size = 16, color = "grey30")) +
  guides(fill = guide_legend(reverse = TRUE))

distr_empl

# Save plot
ggsave("me_trud_after/stacked_bar.png", distr_empl, width = 12, height = 6, units = "in")

# Facet pie ----
distr_empl_pie <- ggplot(data = dm, aes(x = 1, y = value, fill = variable)) +
  geom_bar(stat = "identity", width = 1, color = "white", alpha = 0.7) +
  geom_text(aes(label = value * 100),
            position = position_fill(vjust = 0.5),
            family = "Roboto",
            color = "white",
            size = 5) +
  scale_fill_manual(name = "", 
                    values = c("#7D9C9F", "#3D4935"),
                    labels = c("Не трудоустроено", "Трудоустроено")) +
  facet_wrap(~fct_rev(district), nrow = 2, labeller = label_wrap_gen(width = 25, multi_line = TRUE)) +
  coord_polar(theta = "y", direction = -1) +
  labs(title = "Доля трудоустроенных выпускников по округу вуза, %") +
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.text = element_text(family = "Roboto", size = 12, color = "grey30"),
        legend.position = "bottom",
        legend.box.margin = margin(t = -15, unit = "pt"),
        strip.background = element_blank(),
        strip.text = element_text(family = "Roboto", size = 11.5, color = "grey30"),
        plot.title = element_text(family = "Roboto", size = 16, color = "grey30", hjust = 0.5)) +
  guides(fill = guide_legend(reverse = TRUE))

distr_empl_pie

# Save plot
ggsave("me_trud_after/pie.png", distr_empl_pie, width = 12, height = 6, units = "in")

# Facet dodge ----
distr_empl_dodge <- ggplot(data = dm, aes(x = fct_rev(variable), y = value, fill = variable)) +
  geom_bar(stat = "identity", width = 0.8, color = "white", alpha = 0.7, position = position_dodge()) +
  geom_text(aes(label = value * 100),
            position = position_dodge(width = 0.8),
            family = "Roboto",
            color = "white",
            size = 5,
            vjust = 1.3) +
  scale_fill_manual(name = "", 
                    values = c("#7D9C9F", "#3D4935"),
                    labels = c("Не трудоустроено", "Трудоустроено")) +
  facet_wrap(~fct_rev(district), nrow = 2, labeller = label_wrap_gen(width = 25, multi_line = TRUE)) +
  labs(title = "Доля трудоустроенных выпускников по округу вуза, %") +
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.text = element_text(family = "Roboto", size = 12, color = "grey30"),
        legend.position = "bottom",
        legend.box.margin = margin(t = -15, unit = "pt"),
        strip.background = element_blank(),
        strip.text = element_text(family = "Roboto", size = 12, color = "grey30"),
        plot.title = element_text(family = "Roboto", size = 16, color = "grey30", hjust = 0.5)) +
  guides(fill = guide_legend(reverse = TRUE))

distr_empl_dodge

# Save plot
ggsave("me_trud_after/bar_dodge.png", distr_empl_dodge, width = 12, height = 6, units = "in")