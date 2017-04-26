# Competition between BA applicants

# Load libraries ----
library("reshape2")
library("ggplot2")
library("forcats")
library("stringr")

# Colours ----
pal <- c("#C5E0DC", "#E08E79", "#F1D4AF", "#A8DBA8")

# Load data ----
d <- read.csv("data/ba_competition.csv", stringsAsFactors = FALSE)

# The BEFORE plot ----
ba_comp_before <- ggplot(data = d, aes(x = broader.spec, y = competition, fill = cat_fact)) +
  geom_bar(position = position_dodge(width = 0.85), color = "white", size = 0.8, stat = "identity") +
  geom_text(aes(label = round(competition, 2),
                vjust = 0.5, 
                hjust = -0.08), 
            position = position_dodge(width = 0.85), 
            color = "grey30",
            family = "Roboto",
            size = 5) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 30)) +
  scale_y_continuous(limits = c(0, 8)) +
  scale_fill_manual(name = "Форма обучения:",
                    values = pal) + 
  labs(x = "Направление подготовки",
       y = "Конкурс, человек/место",
       title = "Конкурс на укрупнённые направления подготовки, бакалавриат") +
  coord_flip() +
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        legend.box.margin = margin(b = -15),
        legend.position = "top",
        plot.title = element_text(color = "grey30", family = "PT Sans", size = 20, hjust = 0.5, margin = margin(b = 0)),
        plot.caption = element_text(color = "grey30", family = "PT Sans", size = 14),
        legend.text = element_text(color = "grey30", family = "PT Sans", size = 14),
        legend.title = element_text(color = "grey30", family = "PT Sans", size = 14),
        axis.text.x = element_text(color = "grey30", family = "PT Sans", size = 14),
        axis.text.y = element_text(color = "grey30", family = "PT Sans", size = 14),
        axis.title = element_text(color = "grey30", family = "PT Sans", size = 16),
        axis.line.x = element_line(color = "grey30"),
        axis.line.y = element_line(color = "grey30"),
        axis.ticks = element_blank())

ba_comp_before

# The AFTER plot ----

# For correct order of bars on plot
d$cat_fact <- fct_relevel(factor(d$cat_fact), "Очная", "Очно-заочная")

ba_comp_after <- ggplot(data = d, aes(x = fct_reorder(cat_fact, competition), y = competition, fill = cat_fact)) +
  geom_bar(stat = "identity", width = 0.85) +
  geom_text(aes(label = round(competition, 2)), 
            hjust = -0.15,
            color = "grey30",
            family = "Roboto",
            size = 5) +
  facet_wrap(~broader.spec, nrow = 2, labeller = label_wrap_gen(width = 25, multi_line = TRUE)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(limits = c(0, 8.2)) +
  scale_fill_manual(name = "",
                    values = pal) + 
  labs(x = "Форма обучения",
       y = "",
       title = "Конкурс на укрупнённые направлениям подготовки\nпри поступлении в бакалавриат, человек/место") +
  coord_flip() +
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        legend.position = "none",
        plot.title = element_text(color = "grey30", family = "PT Sans", size = 20, hjust = 0.5, margin = margin(b = 5)),
        plot.caption = element_text(color = "grey30", family = "PT Sans", size = 14),
        legend.text = element_text(color = "grey30", family = "PT Sans", size = 14),
        legend.title = element_text(color = "grey30", family = "PT Sans", size = 14),
        strip.background = element_blank(),
        strip.text = element_text(color = "grey30", family = "PT Sans", size = 15, face = "italic"),
        axis.text.x = element_blank(),
        axis.text.y = element_text(color = "grey30", family = "PT Sans", size = 14),
        axis.title = element_text(color = "grey30", family = "PT Sans", size = 14),
        axis.line.y = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks = element_blank())

ba_comp_after


ggsave("ba_comp_after/ba_comp_facet.png", ba_comp_after, width = 12, height = 8, units = "in")
