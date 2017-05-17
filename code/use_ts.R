library("dplyr")
library("reshape2")
library("ggplot2")
library("grid")  # for plots arrangement

# Original plot and data source
# https://ege.hse.ru/stata

# Load data ----
d <- read.csv("data/use_dynam.csv", stringsAsFactors = FALSE)

# Special helper function to add spaces in large numbers: 10000 - > 10 000 ----
format_space <- function(vect) {
  vect <- format(vect, big.mark = " ", scientific = FALSE, trim = TRUE)
  return(vect)
}

# Create separate dataset for USE and transform it to long format ----
d_use <- d %>%
  select(year, ege_av_bud, ege_av_pr) %>%
  melt(id = "year")

# Create separate dataset for ADMISSION and transform it to long format ----
d_adm <- d %>%
  select(year, adm_bud, adm_pr) %>%
  melt(id = "year")

# USE plot ----
use_ts <- ggplot(data = d_use, aes(x = year, y = value, color = variable)) +
  geom_line() +
  geom_point() +
  geom_text(aes(label = value,
                vjust = ifelse(year %in% seq(2011, 2015, 2), -0.8, 1.4)),
            size = 4.5,
            family = "Roboto",
            show.legend = FALSE) +
  scale_y_continuous(breaks = seq(55, 70, 5), 
                     labels = seq(55, 70, 5),
                     limits = c(55, 70)) +
  scale_color_manual(name = "",
                     labels = c("Бюджетные места", "Платные места"),
                     values = c("#D95B43", "#53777A")) +
  labs(title = "Динамика среднего балла ЕГЭ, необходимого для поступления",
       y = "Балл ЕГЭ",
       x = "") +
  theme(axis.text = element_text(size = 14, family = "PT Sans", color = "grey30"),
        axis.title = element_text(size = 14, family = "PT Sans", color = "grey30"),
        axis.ticks = element_blank(),
        axis.line = element_line(color = "grey30", linetype = "dotted"),
        plot.title = element_text(size = 18, family = "PT Sans", color = "grey30", hjust = 0.5),
        legend.position = "top",
        legend.key = element_blank(),
        legend.text = element_text(size = 14, family = "PT Sans", color = "grey30"),
        legend.box.margin = margin(t = -5, b = -15, unit = "pt"),
        panel.background = element_blank())

use_ts

# Admission plot ----
adm_ts <- ggplot(data = d_adm, aes(x = year, y = value, color = variable)) +
  geom_line() +
  geom_point() +
  geom_text(aes(label = format_space(value),
                vjust = ifelse(!(year %in% seq(2011, 2015, 2)), -1.1, 1.9),
                hjust = 0.6),
            size = 4.5,
            family = "Roboto",
            show.legend = FALSE) +
  scale_y_continuous(breaks = seq(100000, 300000, 100000), 
                     limits = c(85000, 320000),
                     labels = format_space) +
  scale_color_manual(name = "",
                     labels = c("Бюджетные места", "Платные места"),
                     values = c("#D95B43", "#53777A")) +
  labs(title = "Динамика числа принятых абитуриентов",
       y = "Число принятых абитуриентов, человек",
       x = "") +
  theme(axis.text = element_text(size = 14, family = "PT Sans", color = "grey30"),
        axis.title = element_text(size = 14, family = "PT Sans", color = "grey30"),
        axis.ticks = element_blank(),
        axis.line = element_line(color = "grey30", linetype = "dotted"),
        plot.title = element_text(size = 18, family = "PT Sans", color = "grey30", hjust = 0.5),
        legend.position = "none",
        panel.background = element_blank())

adm_ts

# Align plots by left edge ----
# Details: http://stackoverflow.com/questions/13294952/left-align-two-graph-edges-ggplot

# Convert to grob object
grU <- ggplotGrob(use_ts)
grA <- ggplotGrob(adm_ts)

# Save to png. ggsave is powerless here ----
grid.newpage()
png(filename = "plots/use_ts/use_ts_after.png", width = 10, height = 10, units = "in", res = 70)
grid.draw(rbind(grU, grA))
dev.off()

# Admission structure ----
d_adm_str <- d_adm %>%
  mutate(year = factor(year)) %>%
  group_by(year) %>%
  mutate(fracts = value/sum(value))

# Total number of admitted students
d_tot_adm <- d_adm %>%
  mutate(year = factor(year)) %>%
  group_by(year) %>%
  summarise(tot_numb = sum(value))

# Correct plot
adm_str_pl_1 <- ggplot(data = d_adm_str, aes(x = year, y = fracts, fill = variable)) +
  geom_bar(stat = "identity", color = "white", width = 0.8, alpha = 0.55) +
  geom_text(aes(label = round(100 * fracts, 0)), 
            position = position_stack(vjust = 0.5),
            family = "Roboto",
            size = 6,
            color = "white") +
  geom_text(data = d_tot_adm, 
            aes(x = year, 
                y = 1, 
                fill = NULL, 
                label = paste0("Всего:\n", format_space(tot_numb)),
                vjust = -0.1),
            family = "Roboto",
            size = 6,
            color = "grey30") +
  scale_y_continuous(breaks = seq(0, 1, 0.25), 
                     labels = seq(0, 100, 25),
                     limits = c(0, 1.05)) +
  scale_fill_manual(name = "",
                     labels = c("Бюджетные места", "Платные места"),
                     values = c("#D95B43", "#53777A")) +
  labs(title = "Изменение структуры приёма",
       y = "Доля, %",
       x = "") +
  theme(axis.text = element_text(size = 16, family = "PT Sans", color = "grey30"),
        axis.title = element_text(size = 16, family = "PT Sans", color = "grey30"),
        axis.ticks = element_blank(),
        axis.line = element_line(color = "grey30", linetype = "dotted"),
        plot.title = element_text(size = 22, family = "PT Sans", color = "grey30", hjust = 0.5),
        legend.position = "bottom",
        legend.key = element_blank(),
        legend.text = element_text(size = 16, family = "PT Sans", color = "grey30"),
        legend.box.margin = margin(t = -15, unit = "pt"),
        panel.background = element_blank())

adm_str_pl_1

ggsave("plots/use_ts/use_struct_correct.png", adm_str_pl_1, width = 12, height = 8, units = "in")

# Wrong way
adm_str_pl_2 <- ggplot(data = d_adm_str, aes(x = year, y = value, fill = variable)) +
  geom_bar(stat = "identity", color = "white", width = 0.8, alpha = 0.55) +
  geom_text(aes(label = paste0(round(100 * fracts, 0), "%")),
            position = position_stack(vjust = 0.5),
            family = "Roboto",
            size = 6,
            color = "white") +
  geom_text(data = d_tot_adm,
            aes(x = year,
                y = tot_numb,
                fill = NULL,
                label = paste0("Всего:\n", format_space(tot_numb)),
                vjust = -0.1),
            family = "Roboto",
            size = 6,
            color = "grey30") +
  scale_y_continuous(breaks = seq(0, 450000, 150000),
                     labels = format_space,
                     limits = c(0, 480000)) +
  scale_fill_manual(name = "",
                    labels = c("Бюджетные места", "Платные места"),
                    values = c("#D95B43", "#53777A")) +
  labs(title = "ПЛОХОЙ СПОСОБ показать изменение структуры приёма",
       y = "Число принятых абитуриентов, человек",
       x = "") +
  theme(axis.text = element_text(size = 16, family = "PT Sans", color = "grey30"),
        axis.title = element_text(size = 16, family = "PT Sans", color = "grey30"),
        axis.ticks = element_blank(),
        axis.line = element_line(color = "grey30", linetype = "dotted"),
        plot.title = element_text(size = 22, family = "PT Sans", color = "grey30", hjust = 0.5),
        legend.position = "bottom",
        legend.key = element_blank(),
        legend.text = element_text(size = 16, family = "PT Sans", color = "grey30"),
        legend.box.margin = margin(t = -15, unit = "pt"),
        panel.background = element_blank())

adm_str_pl_2

ggsave("plots/use_ts/use_struct_wrong.png", adm_str_pl_2, width = 12, height = 8, units = "in")

# Correct but not so useful way
adm_str_pl_3 <- ggplot(data = d_adm_str, aes(x = year, y = value, fill = variable)) +
  geom_bar(stat = "identity", color = "white", width = 0.8, alpha = 0.55) +
  geom_text(aes(label = format_space(value)),
            position = position_stack(vjust = 0.5),
            family = "Roboto",
            size = 6,
            color = "white") +
  geom_text(data = d_tot_adm,
            aes(x = year,
                y = tot_numb,
                fill = NULL,
                label = paste0("Всего:\n", format_space(tot_numb)),
                vjust = -0.1),
            family = "Roboto",
            size = 6,
            color = "grey30") +
  scale_y_continuous(breaks = seq(0, 450000, 150000),
                     labels = format_space,
                     limits = c(0, 480000)) +
  scale_fill_manual(name = "",
                    labels = c("Бюджетные места", "Платные места"),
                    values = c("#D95B43", "#53777A")) +
  labs(title = "Изменение структуры приёма",
       y = "Число принятых абитуриентов, человек",
       x = "") +
  theme(axis.text = element_text(size = 16, family = "PT Sans", color = "grey30"),
        axis.title = element_text(size = 16, family = "PT Sans", color = "grey30"),
        axis.ticks = element_blank(),
        axis.line = element_line(color = "grey30", linetype = "dotted"),
        plot.title = element_text(size = 22, family = "PT Sans", color = "grey30", hjust = 0.5),
        legend.position = "bottom",
        legend.key = element_blank(),
        legend.text = element_text(size = 16, family = "PT Sans", color = "grey30"),
        legend.box.margin = margin(t = -15, unit = "pt"),
        panel.background = element_blank())

adm_str_pl_3

ggsave("plots/use_ts/use_struct_not_quite_useful.png", adm_str_pl_3, width = 12, height = 8, units = "in")
