# Load data ----
d <- read.csv("data/ba_adm.csv", stringsAsFactors = FALSE)

# For correct order of bars
d$category <- fct_relevel(factor(d$category), "Заочная", "Очно-заочная")

# Special helper function to add spaces in large numbers: 10000 - > 10 000 ----
format_space <- function(vect) {
  vect <- format(vect, big.mark = " ", scientific = FALSE, trim = TRUE)
  return(vect)
}

# The BEFORE plot ----
ba_adm_before <- ggplot(data = d, aes(x = broader.spec, y = n_adm, fill = category)) +
  geom_bar(position = position_dodge(width = 0.9), color = "white", size = 0.8, stat = "identity") +
  geom_text(aes(label = formatSpace(n_adm),
                vjust = 0.5, 
                hjust = -0.08), 
            position = position_dodge(width = 0.9), 
            color = "grey30",
            family = "Roboto",
            size = 5) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 30)) +
  scale_y_continuous(breaks = seq(0, 250000, 50000), labels = formatSpace(seq(0, 250000, 50000)), limits = c(0, 255000)) +
  scale_fill_manual(name = "Форма обучения:",
                    values = pal) +
  labs(x = "Направление подготовки",
       y = "Принято студентов, человек",
       title = "Число принятых в бакалавриат студентов") +
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
        axis.ticks = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE))

ba_adm_before

# Save plot
ggsave("plots/ba_adm/ba_adm_before.png", ba_adm_before, width = 12, height = 8, units = "in")

# The AFTER plot ----
ba_adm_after <- ggplot(data = d, aes(x = category, y = n_adm, fill = category)) +
  geom_bar(stat = "identity", width = 0.85) +
  geom_text(aes(label = formatSpace(n_adm), 
                hjust = dplyr::if_else(n_adm > 200000, 1.1, -0.15)),
            color = "grey30",
            family = "Roboto",
            size = 5) +
  facet_wrap(~broader.spec, nrow = 2, labeller = label_wrap_gen(width = 25, multi_line = TRUE)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(limits = c(0, 255000)) +
  scale_fill_manual(name = "",
                    values = pal) + 
  scale_color_manual(name = "",
                    values = c("grey30", "grey84")) + 
  labs(x = "Форма обучения",
       y = "",
       title = "Число принятых в бакалариват студентов\nпо укрупнённым направлениям подготовки, человек") +
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

ba_adm_after

# Save plot
ggsave("plots/ba_adm/ba_adm_after.png", ba_adm_after, width = 12, height = 8, units = "in")
