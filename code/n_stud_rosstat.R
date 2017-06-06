# Student admsision plot redesign

library("dplyr")
library("forcats")
library("grid")
library("gtable")
library("ggplot2")
library("stringr")

# Read data in
adm_gks <- read.csv("data/adm_gks.csv", stringsAsFactors = FALSE)

# Manipulate factor levels
adm_gks <- adm_gks %>%
  mutate(level = fct_relevel(level, "Среднее образование"),
         finance = fct_relevel(finance, "Федеральный бюджет", "Бюджет субъектов РФ", "Местный бюджет"))

# Add total sum as another variable and use it in facets
adm_gks_sum <- adm_gks %>%
  group_by(level) %>%
  summarise(n_stud_sm = sum(n_stud)) %>%
  right_join(adm_gks, by = "level") %>%
  mutate(n_stud_sm = paste0("Всего: ", n_stud_sm))

# V1: ignore proportions ----
adm_gks_no_prop <- ggplot(adm_gks_sum, aes(x = finance, y = n_stud, fill = level)) +
  geom_bar(stat = "identity", width = 0.9, alpha = 0.9) +
  geom_text(aes(label = n_stud, vjust = -0.6),
            family = "Roboto Condensed",
            size = 4.5,
            color = "grey30") +
  facet_wrap(~level + n_stud_sm, nrow = 1) +
  scale_x_discrete(label = function(x) str_wrap(x, 10)) +
  scale_fill_manual(values = c("#40866A", "#BCB77B")) +
  labs(x = "Источник финансирования",
       y = "Число студентов, тыс. человек",
       title = "Приём на программы среднего и высшего образования\nв государственные и муниципальные образовательные организации") +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        strip.background = element_rect(fill = "gray96"),
        strip.text = element_text(family = "PT Sans", face = "italic", color = "grey30", size = 14), 
        plot.title = element_text(family = "PT Sans", size = 16, color = "grey30", hjust = 0.5),
        axis.title = element_text(family = "PT Sans", size = 14, color = "grey30"),
        axis.text.x = element_text(family = "PT Sans", size = 13,color = "grey30", margin = margin(t = -10, b = 5, unit = "pt")),
        axis.text.y = element_text(family = "Roboto Condensed", size = 13, color = "grey30"))

adm_gks_no_prop

# Save plot
ggsave("plots/adm_gks/adm_gks_v1.png", adm_gks_no_prop, width = 3, height = 3, units = "in", scale = 3)


# V2: now focus on them! ----

# Add frequency to data, cause we can't simply retreive it using position_fill inside geom_bar
adm_gks_sum <- adm_gks_sum %>%
  group_by(level) %>%
  mutate(freq = n_stud/sum(n_stud))

# Totals for top of the plot
adm_gks_totals <- adm_gks_sum %>%
  group_by(level) %>%
  summarise(n_stud_level = sum(n_stud)) %>%
  mutate(n_stud_level = gsub("\\.", ",", n_stud_level))

# Plot itself
adm_gks_prop <- ggplot(adm_gks_sum, aes(x = level, y = freq, fill = finance)) +
  geom_bar(stat = "identity", 
           color = "white",
           width = 0.5, 
           size = 0.3,
           alpha = 0.9) +
  geom_text(aes(label = ifelse(freq > 0.02, round(freq * 100, 0), "")),
            position = position_fill(vjust = 0.5),
            color = "white",
            family = "Roboto Condensed",
            size = 5) +
  geom_text(data = adm_gks_totals,
            aes(x = level, 
                y = 1, 
                label = paste0("Всего: ", n_stud_level, " тыс."), 
                fill = NULL),
            vjust = -0.4,
            color = "grey30",
            family = "PT Sans",
            size = 4.5) +
  scale_y_continuous(labels = seq(0, 100, 25)) +
  scale_fill_manual(name = "", 
                    values = c( "#40866A", "#BCB77B", "#FD8603", "#FBB829")) +
  labs(x = "",
       y = "Доля, %",
       title = "Приём на программы среднего и высшего образования в государственные и\nмуниципальные образовательные организации по источникам финансирования") +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "right",
        legend.justification = "top",
        legend.key.height = unit(0.6, "cm"),
        legend.key.width = unit(0.6, "cm"),
        legend.text = element_text(family = "PT Sans", color = "grey30", size = 12),
        strip.background = element_rect(fill = "gray96"),
        strip.text = element_text(family = "PT Sans", face = "italic", color = "grey30", size = 14), 
        plot.title = element_text(family = "PT Sans", size = 16, color = "grey30"),
        axis.title = element_text(family = "PT Sans", size = 14, color = "grey30"),
        axis.text.x = element_text(family = "PT Sans", size = 13,color = "grey30", margin = margin(t = -10, b = 5, unit = "pt")),
        axis.text.y = element_text(family = "Roboto Condensed", size = 13, color = "grey30"))


# Now add some explaining text blow legend ----
# Epic solution for adding text below legend by Sandy Muspratt ----
# https://stackoverflow.com/questions/32506444/ggplot-function-to-add-text-just-below-legend

# Create graphical object and extract legend
g <- ggplotGrob(adm_gks_prop)
leg <- g$grobs[[which(g$layout$name == "guide-box")]]

# Text to include
explaining_text <- "*Доля бюджета субъектов РФ\nв финансировании ВО составляет 1,6%.\n
**Для местного бюджета доля\nне превышает 0,5% в обоих случаях."

# Construct the label grob 
xpos <- 5
textgrob <- textGrob(x = unit(xpos, "points"), 
                     explaining_text, 
                     gp = gpar(fontfamily = "PT Sans",
                                      col = "grey30",
                                      fontsize = 12),
                     just = "left")

width <- unit(1, "grobwidth", textgrob) + unit(2 * xpos, "points")
height <- unit(1, "grobheight", textgrob) + unit(2 * xpos, "points")

pos <- subset(leg$layout, grepl("guides", name), t:r)

# Add new text as new row in the legend
leg <- gtable_add_rows(leg, height, pos = pos$t+1)
leg <- gtable_add_grob(leg, textgrob, t = pos$t+2, l = pos$l)

# Adjust the middle width of the legend to be the maximum of the original width 
# or the width of the grob
leg$widths[pos$l] <- max(width, leg$widths[pos$l])

# Return the modified legend to the origial plot
g$grobs[[which(g$layout$name == "guide-box")]] <- leg

# Adjust the width of the column containing the legend to be the maximum 
# of the original width or the width of the label
g$widths[g$layout[grepl("guide-box", g$layout$name), "l"]] <- max(width, sum(leg$widths))

# Draw and save the plot
png("plots/adm_gks/adm_gks_v2.png", width = 9, height = 9, units = "in", res = 70)
grid.newpage()
grid.draw(g)
dev.off()

