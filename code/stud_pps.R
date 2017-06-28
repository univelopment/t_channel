# Region list with dots instead of bars

# Load libraries
library("dplyr")
library("forcats")
library("ggplot2")
library("extrafont")

# Read data in
d <- readr::read_csv("data/stud_pps_gos_ochn.csv")

# Add a couple of variables and manipulate factors
d <- d %>% 
  mutate(pps_stud_round = round(pps_stud, 0),
         # 2 functions are applied to achive alphabetic order with coord_flip:
         region = fct_reorder(fct_rev(region), pps_stud_round),
         # For coloring margin dots the other way:
         min_max_ind = ifelse(pps_stud_round < 4 | pps_stud_round > 9, "a", "b"))


# Bar chart ----
bars <- ggplot(data = d, aes(x = region, y = pps_stud_round)) +
  geom_segment(x = -Inf, xend = 84, y = 10, yend = 10, color = "grey60", linetype = "dotted", size = 0.3) +
  geom_segment(x = -Inf, xend = 4, y = 5, yend = 5, color = "grey60", linetype = "dotted", size = 0.3) +
  geom_segment(x = -Inf, xend = 16, y = 7, yend = 7, color = "grey60", linetype = "dotted", size = 0.3) +
  geom_col(color = "white", fill = "grey80", size = 0.1, width = 0.9) +
  scale_y_continuous(limits = c(0, 10.05), breaks = c(0, 3, 5, 7, 10), expand = c(0, 0)) +
  coord_flip() +
  labs(x = "",
       y = "Число студентов на 1 ППС",
       title = "Число студентов, которое приходится на 1 человека из\nпрофессорско-преподавательского состава",
       subtitle = "Информация относится к государственным вузам; совместители и сотрудники\nработающие по договорам ГПХ учтены в расчётах",
       caption = "Источник: формы ВПО-1 2016") + 
  theme(axis.ticks = element_blank(),
        axis.line.x = element_line(color = "grey85"),
        axis.text.x = element_text(family = "Roboto Condensed", color = "grey18", size = 12),
        axis.text.y = element_text(family = "PT Sans", color = "grey18", size = 9),
        axis.title.x = element_text(family = "Roboto", color = "grey18", size = 11),
        plot.subtitle = element_text(family = "Roboto", color = "grey18", size = 10),
        plot.title = element_text(family = "Roboto", color = "grey18", size = 15),
        plot.caption = element_text(family = "Roboto", color = "grey18", size = 9),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(color = "grey96"),
        panel.grid.major.x = element_blank())

ggsave("plots/stud_pps/stud_pps_bar.png", bars, width = 8 , height = 13, units = "in") 


# Dot plot ----
dots <- ggplot(data = d) +
  geom_segment(x = 1, xend = 84, y = 3, yend = 3, color = "grey60", linetype = "dotted", size = 0.3) +
  geom_segment(x = 1, xend = 16.5, y = 7, yend = 7, color = "grey60", linetype = "dotted", size = 0.3) +
  geom_segment(x = 43.5, xend = 84, y = 7, yend = 7, color = "grey60", linetype = "dotted", size = 0.3) +
  geom_segment(x = 1, xend = 78.5, y = 10, yend = 10, color = "grey60", linetype = "dotted", size = 0.3) +
  geom_point(data = d,
             aes(x = region, 
                 y = pps_stud_round,
                 shape = min_max_ind,
                 color = min_max_ind),
             size = 2,
             show.legend = FALSE) +
  scale_y_continuous(limits = c(0, 10.1), breaks = c(0, 3, 5, 7, 10), expand = c(0, 0)) +
  scale_shape_manual(values = c(19, 1)) +
  scale_color_manual(values = c("grey35", "grey18")) +
  coord_flip() +
  labs(x = "",
       y = "Число студентов на 1 ППС",
       title = "Число студентов, которое приходится на 1 человека из\nпрофессорско-преподавательского состава",
       subtitle = "Информация относится к государственным вузам; совместители и сотрудники\nработающие по договорам ГПХ учтены в расчётах",
       caption = "Источник: формы ВПО-1 2016") + 
  theme(axis.ticks = element_blank(),
        axis.line.x = element_line(color = "grey85"),
        axis.text.x = element_text(family = "Roboto Condensed", color = "grey18", size = 12),
        axis.text.y = element_text(family = "PT Sans", color = "grey18", size = 9),
        axis.title.x = element_text(family = "Roboto", color = "grey18", size = 11),
        plot.subtitle = element_text(family = "Roboto", color = "grey18", size = 10),
        plot.title = element_text(family = "Roboto", color = "grey18", size = 15),
        plot.caption = element_text(family = "Roboto", color = "grey18", size = 9),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(color = "grey96"),
        panel.grid.major.x = element_blank())

ggsave("plots/stud_pps/stud_pps_dots.png", dots, width = 8, height = 13, units = "in")


# Experimental plot
dots_exp <- ggplot(data = d) +
  geom_segment(x = 1, xend = 84, y = 3, yend = 3, color = "grey60", linetype = "dotted", size = 0.3) +
  geom_segment(x = 1, xend = 16.5, y = 7, yend = 7, color = "grey60", linetype = "dotted", size = 0.3) +
  geom_segment(x = 43.5, xend = 84, y = 7, yend = 7, color = "grey60", linetype = "dotted", size = 0.3) +
  geom_segment(x = 1, xend = 78.5, y = 10, yend = 10, color = "grey60", linetype = "dotted", size = 0.3) +
  geom_point(data = d,
             aes(x = region, 
                 y = pps_stud_round,
                 shape = min_max_ind,
                 color = min_max_ind),
             size = 2,
             show.legend = FALSE) +
  scale_y_continuous(limits = c(0, 10.1), breaks = c(0, 3, 5, 7, 10), expand = c(0, 0)) +
  scale_shape_manual(values = c(19, 1)) +
  scale_color_manual(values = c("grey35", "grey18")) +
  coord_flip() +
  labs(x = "",
       y = "Число студентов на 1 ППС",
       title = "Число студентов, которое приходится на 1 человека из\nпрофессорско-преподавательского состава",
       subtitle = "Информация относится к государственным вузам; совместители и сотрудники\nработающие по договорам ГПХ учтены в расчётах",
       caption = "Источник: формы ВПО-1 2016") + 
  theme(axis.ticks = element_blank(),
        axis.line.x = element_line(color = "grey85"),
        axis.text.x = element_text(family = "Roboto Condensed", color = "grey18", size = 12),
        axis.text.y = element_text(family = "PT Sans", color = "grey18", size = 9),
        axis.title.x = element_text(family = "Roboto", color = "grey18", size = 11),
        plot.subtitle = element_text(family = "Roboto", color = "grey18", size = 10),
        plot.title = element_text(family = "Roboto", color = "grey18", size = 15),
        plot.caption = element_text(family = "Roboto", color = "grey18", size = 9),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(color = "grey96"),
        panel.grid.major.x = element_blank())