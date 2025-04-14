library(showtext)
library(ggtext)
library(tidyverse)
library(ggplot2)


font_add_google("Fira sans", "title_font")
font_add_google("Roboto", "body_font")


showtext_auto()

title_font <- "title_font"
body_font <- "body_font"


title_text <- "Healthcare Workforce Growth: Doctors, Nurses & Dentists"
subtitle_text <- "Registered with Pakistan Medical & Dental Council and Pakistan Nursing Council"
caption_text <- "Vistales      |     Data: Pakistan Bureau of Statistics"


data <- read.csv("C:/Users/CHD/Downloads/medics.csv")
# Convert to long format
df <- data %>%
  pivot_longer(cols = -Year, names_to = "Category", values_to = "Count")



# Identify the first and last year
first_last <- range(df$Year)

# Add labels only for first and last year
df <- df %>%
  mutate(label = if_else(Year %in% first_last, as.character(Count), NA_character_))

# Plot
ggplot(df, aes(x = factor(Year), y = Count, fill = Category)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(start = 0) +
  scale_fill_brewer(palette = "Pastel1") +
  theme_void() +
  labs(title = title_text,
       subtitle = subtitle_text,
       caption = caption_text,
       y= "Rank")+
  
  theme(
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    axis.text.x = element_text(face = "bold"),
    # Legend
    legend.position = "bottom",
    
    # TITLE
    plot.title.position = "plot",
    plot.title = element_textbox(margin = margin(30, 0, 18, 5),
                                 size = 30,
                                 hjust = 0.5,
                                 family = title_font,
                                 face = "bold",
                                 width = unit(60, "lines")),
    plot.subtitle = element_textbox(margin = margin(5, 0, 18, 5),
                                    size = 16,
                                    hjust = 0.5,
                                    family = title_font,
                                    face = "bold",
                                    width = unit(60, "lines")),
    
    # Caption
    plot.caption = element_text(family=body_font,
                                
                                size=13, 
                                color="black",
                                hjust=0.5,
                                margin=margin(30,0,0,0)),
    
    plot.background = element_rect(color="#f0f7dc", fill="#f0f7dc"),
    plot.margin = margin(15, 5, 25, 5)
  )+
  labs(x = "Year", y = "Rank")

# ------ Save Plot ------ 

showtext_opts(dpi = 320)

ggsave("medics.png",dpi=320,
       width = 14, height = 10)
showtext_auto(TRUE)

