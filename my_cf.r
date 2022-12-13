# import data set ----
my_cf <- read.csv("my_cf.csv")

# library packages
library(tidyverse)
library(rcartocolor)
library(ggtext)
library(cowplot)
library(patchwork)
library(showtext)

# add font----
font_add_google("Fira Sans", "Fira Sans")
font_add_google("Playfair Display", "Playfair Display")

# ggplot theme ----
theme_set(theme_minimal(base_family = "Fira Sans"))

theme_update(plot.background = element_rect(fill = "transparent",
                                            color = "transparent"),
             panel.background = element_rect(fill = "transparent", color=NA),
             panel.grid = element_blank(),
             axis.ticks = element_blank(),
             axis.text = element_blank(),
             axis.title = element_blank(),
             plot.title = element_markdown(family = "Playfair Display",
                                           size = 90,
                                           color = "grey10",
                                           face = "bold",
                                           hjust = .5,
                                           margin = margin(80, 0, 20, 0)),
             plot.subtitle = element_markdown(size = 40,
                                              color = "grey30",
                                              face = "plain",
                                              hjust = .5,
                                              lineheight = 1.2,
                                              margin = margin(20, 0, -15, 0)),
             plot.caption = element_text(family = "Playfair Display",
                                         size = 30,
                                         color = "grey70",
                                         face = "bold",
                                         hjust = .5,
                                         margin = margin(0, 0, 30, 0)),
             plot.title.position = "plot",
             plot.caption.position = "plot",
             plot.margin = margin(10, 0, 10, 0))


# select some columns
my_cf = my_cf[,c(1,2,3,11)]
my_cf$SCOPE = as.factor(my_cf$SCOPE)
my_cf$percent = my_cf$cf*100/sum(my_cf$cf)
my_cf$SCOPE = as.factor(my_cf$SCOPE)

my_cf$percent = round(my_cf$percent, digits = 1)

# add some angle and position for text label
new_cf <-
  my_cf %>%
  arrange(SCOPE, percent) %>%
  mutate(
    pos = cumsum(percent),
    pos = if_else(!is.na(lag(pos)), pos - ((pos - lag(pos)) / 2), pos / 2),
    angle = 90 - 360 * (pos - 0.5),
    hjust = if_else(angle > 90, 0, 1),
    angle = ifelse(angle > 90, angle + 180, angle),
    label_big = main,
    label_small = desc,
  )%>%
  group_by(SCOPE) %>%
  arrange(percent) %>%
  mutate(
    alpha = percent / max(percent),
    pos_cont = min(pos) + (max(pos) - min(pos)) / 2,
    angle_cont = 90 - 360 * (pos_cont - 0.5),# / n(),
    hjust_cont = if_else(angle_cont > 90, 0, 1),
    angle_cont = ifelse(angle_cont > 90, angle_cont + 180, angle_cont)
  )

# ploting
  
p = new_cf %>%
  ggplot(aes(1, percent)) +
  geom_col(
    aes(
      fill = SCOPE,
      alpha = alpha,
      color = after_scale(colorspace::darken(fill, .2)),
      fill = after_scale(colorspace::lighten(fill, .3))
    ),
    color = "white",
    size = .5
  ) +
  geom_col(
    aes(fill = SCOPE,
        color = SCOPE),
    width = .4,
    size = .8
  ) +
  geom_rect(
    xmin = -Inf, xmax = .8,
    ymin = -Inf, ymax = Inf,
    fill = "#7e9bba"
  ) +
  coord_polar(theta = "y") +
  scale_x_continuous(limits = c(0, 1.9)) +
  scale_color_carto_d(palette = "TealGrn", direction = -1, guide = F) +
  scale_fill_carto_d(palette = "TealGrn", direction = -1, guide = F) +
  scale_alpha(range = c(.3, 1), guide = F) +
  scale_size(range = c(.7, 14), guide = F)

# save the plot ----
#ggsave('myplot.png', p, dpi=300, width = 23, height = 23, units = 'in')

library(echarts4r)
library(echarts4r.assets)

tree = read.csv('cf-tree.csv')

# add custom svg icon
tree_icon = c(path = "M8.416.223a.5.5 0 0 0-.832 0l-3 4.5A.5.5 0 0 0 5 5.5h.098L3.076 8.735A.5.5 0 0 0 3.5 9.5h.191l-1.638 3.276a.5.5 0 0 0 .447.724H7V16h2v-2.5h4.5a.5.5 0 0 0 .447-.724L12.31 9.5h.191a.5.5 0 0 0 .424-.765L10.902 5.5H11a.5.5 0 0 0 .416-.777l-3-4.5zM6.437 4.758A.5.5 0 0 0 6 4.5h-.066L8 1.401 10.066 4.5H10a.5.5 0 0 0-.424.765L11.598 8.5H11.5a.5.5 0 0 0-.447.724L12.69 12.5H3.309l1.638-3.276A.5.5 0 0 0 4.5 8.5h-.098l2.022-3.235a.5.5 0 0 0 .013-.507z",
              name = "tree")
icons = rbind(icons, tree_icon)


t =tree %>% 
  arrange(desc(year)) %>%
  mutate(tree1 = sapling/10) %>%
  e_charts(scenario) %>% 
  e_pictorial(sapling, symbol = ea_icons("tree"), 
              symbolRepeat = TRUE, z = -1,
              symbolSize = c(10,20)
              ) %>% 
  e_theme("wef") %>%
  e_theme_custom('{"color":["#90EE90","#ffaf51"]}')%>%
  e_title("") %>% 
  e_flip_coords() %>%
  # Hide Legend
  e_legend(show = FALSE) %>%
  # Remove Gridlines
  e_x_axis(splitLine=list(show = FALSE)) %>%
  e_y_axis(splitLine=list(show = FALSE)) %>%
  # Format Label
  e_labels(fontSize = 16, fontWeight ='bold', 
           position = "right", offset=c(10, -1), color='yellow') %>%
  e_tooltip(
    backgroundColor = "rgba(255,255,255,1)"
  )

