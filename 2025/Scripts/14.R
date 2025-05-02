install.packages("PakPC2023", dependencies = TRUE)
library(PakPC2023)
library(ggplot2)
library(ggplot2library(dplyr)
library(showtext)
library(ggtext)


df <- TABLE_01 %>%
  filter(REGION == "OVERALL") %>%       # Keep only 'OVERALL' region
  filter(!PROVINCE == "ISLAMABAD") %>%       # Keep only 'OVERALL' region
  select(PROVINCE, TEHSIL,AVG_HH_SIZE)|>
  group_by(TEHSIL, PROVINCE)|>
  rename(HOUSEHOLD_SIZE = AVG_HH_SIZE)


font_add_google("Outfit", "title_font")
font_add_google("Cabin", "body_font")
showtext_auto()

title_font <- "title_font"
body_font <- "body_font"


title = "How Many Under One Roof? A Tehsil-Wise View of Household Size"
subtitle = "Each data point represents a Tehsil, with labels indicating the highest and lowest household size in their respective provinces"
caption = "Source: Pakistan Bureau of Statistics | Vistales"


my_pal <- rcartocolor::carto_pal( n=12,name = "Bold")


ggplot(df, aes(x = PROVINCE, y = HOUSEHOLD_SIZE, color = PROVINCE, fill = PROVINCE))+
  geom_point(
    position = position_jitter(width = .2, seed = 0),
    size = 4, alpha = .5) +
  geom_point(
    position = position_jitter(width = .2, seed = 0),
    size = 4.5, stroke = .4, shape = 1, color = "black")  +
  geom_text(data = df |> 
              group_by(PROVINCE) |>
              slice_min(order_by=HOUSEHOLD_SIZE, n=1),
            aes(label=TEHSIL),
            size=4.5,
            family = body_font,
            nudge_y = -0.25,
            nudge_x = 0.15)+
  geom_text(data = df |> 
                             group_by(PROVINCE) |>
                             slice_max(order_by=HOUSEHOLD_SIZE, n=1),
                           aes(label=TEHSIL),
                           hjust=0.5, size=4.5, 
                           family=body_font,
                           force = 0.5, nudge_y = 0.25, 
                           min.segment.length = 4,
  )+
  ggdist::stat_gradientinterval(
    width = .23, color = "black",
    position = position_nudge(x = 0.350))+
  scale_y_continuous()+
  scale_color_manual(values = my_pal, guide = "none") +
  scale_fill_manual(values = my_pal, guide = "none")+
  theme_minimal()+
  labs(title = title,
       subtitle = subtitle,
       caption = caption)+
  theme(
    axis.title.x  = element_blank(),
    axis.title.y  = element_blank(),
    axis.text.x = element_text(family = body_font, 
                               face = "bold",
                               size=16),
    axis.text.y = element_text(family = body_font, 
                               face = "bold",
                               size=16),
    
    # Legend
    legend.position = "none",
    
    # TITLE
    plot.title.position = "plot",
    plot.title = element_textbox(margin = margin(20, 0, 10, 0),
                                 size = 32,
                                 family = title_font,
                                 face = "bold",
                                 width = unit(85, "lines")),
    # SUB-TITLE
    plot.subtitle = element_textbox(margin = margin(10, 0, 30, 0),
                                    size = 18,
                                    color = "grey30",
                                    family = body_font,
                                    width = unit(70, "lines")),
    
    # Caption
    plot.caption = element_text(family=body_font,
                                face="plain",
                                size=14, 
                                color="grey40",
                                hjust=.5,
                                margin=margin(30,40,0,0)),
    
    plot.background = element_rect(color="white", fill="white"),
    plot.margin = margin(50, 50, 20, 50)
  )



showtext_opts(dpi = 320)

ggsave("kinship.png",dpi=320,
       width = 18, height = 12)
showtext_auto(FALSE)














ggplot(df, aes(x = PROVINCE, y = HOUSEHOLD_SIZE, color = PROVINCE, fill = PROVINCE))+
  geom_point(
    position = position_jitter(width = .2, seed = 0),
    size = 4, alpha = .5) +
  geom_point(
    position = position_jitter(width = .2, seed = 0),
    size = 5, stroke = .4, shape = 1, color = "black")  +
  geom_text(data = df |> 
              group_by(PROVINCE) |>
              slice_min(order_by=HOUSEHOLD_SIZE, n=1),
            aes(label=TEHSIL),
            size=4.5,
            family = body_font,
            nudge_y = -0.25,
            nudge_x = 0.15)+
  geom_text(data = df |> 
              group_by(PROVINCE) |>
              slice_max(order_by=HOUSEHOLD_SIZE, n=1),
            aes(label=TEHSIL),
            hjust=0.5, size=4.5, 
            family=body_font,
            force = 0.5, nudge_y = 0.25, 
            min.segment.length = 4,
  )+
  ggdist::stat_gradientinterval(
    width = .3, color = "black",
    position = position_nudge(x = .10))+
  scale_y_continuous()+
  scale_color_manual(values = my_pal, guide = "none") +
  scale_fill_manual(values = my_pal, guide = "none")
