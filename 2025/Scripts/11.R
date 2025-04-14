
library(showtext)
library(ggtext)
library(tidyverse)
library(MetBrewer)
library(patchwork)
library(ggplot2)
library(readxl)
library(scales)


data <- read_xlsx("Table_4_national.xlsx")  # Replace with actual path

df<-data %>%
  select(Age, MALE, FEMALE) %>%
  pivot_longer(cols = c(MALE, FEMALE), names_to = "Gender", values_to = "Population") %>%
  mutate(
    Age = as.numeric(Age), # ensure Age is numeric
    Age_Bin = cut(Age, breaks = seq(0, 75, by = 1), include.lowest = TRUE, right = FALSE)
  )%>%drop_na()



# Plot
ggplot(df) +
  geom_tile(aes(x = Age, y = 0, fill = Population)) +
  MetBrewer::scale_fill_met_c("VanGogh3") +
  scale_x_continuous(breaks = seq(0, 75, by = 5),expand = c(0.02, 0)) +
  #scale_y_continuous(expand = c(0, 0)) +
  labs(
    title = "Pakistan’s Population Spectrum: Age in Gradient",
    subtitle = str_wrap("Each stripe represents a single age cohort, with color intensity revealing population size. From the deep hues of youth to the fading bands of older generations, 
                        this is a nation’s demographic signature—written in gradients.",130),
    caption = "Data: Pakistan Bureau of Statistics (Population Census 2023)   | Graphic:Vistales",
    fill = "Population"
  ) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(
    plot.title = element_text(family = body_font,size = 22, face = "bold", margin = margin(20,0,15,15)),
    plot.subtitle = element_text(family = body_font,size = 14, margin =  margin(5,0,15,20)),
    plot.caption = element_text(size = 9, color = "grey20", margin = margin(t = 10), hjust = 0),
    legend.position = "bottom",
    legend.title.position = "top",
    legend.title = element_text(hjust = 0.5),
    legend.key.width = unit(2.5, "lines"),
    legend.key.height = unit(0.6, "lines"),
    legend.margin = margin(b = 10),
    plot.background = element_rect(fill = "grey99", color = NA),
    axis.text.x = element_text(color = "black"),
    strip.text = element_text(size = 10, face = "bold"),
    plot.margin = margin(10, 10, 10, 10)
  )



# ------ Save Plot ------ 

showtext_opts(dpi = 320)

ggsave("stripes.png",dpi=320,
       width = 14, height = 10)
showtext_auto(TRUE)
