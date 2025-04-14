library(showtext)
library(ggtext)
library(tidyverse)
library(patchwork)
library(ggplot2)
library(readxl)
library(scales)


font_add_google("Fraunces", "title_font")
font_add_google("Montserrat", "body_font")
showtext_auto()

body_font<- "body_font"
title_font<- "title_font"

df <- read_xlsx("Table_4_national.xlsx")  # Replace with actual path


df$Age<- as.numeric(df$Age)
df<- df %>%
  select(Age, MALE, FEMALE) 

fem<-ggplot(df, aes(x = Age, y = FEMALE)) +
  geom_hline(yintercept = c(1e6, 2e6,3e6,4e6),   color = "gray", size = 0.5)+
  #geom_bar(stat = "identity", width = 0.8) +
  geom_area()+
  coord_flip() +
  #geom_bar(stat = "identity", fill = "#EA498C",width = 0.8) +
  coord_flip() +
  scale_y_continuous(labels = unit_format(unit = " M", scale = 1e-6)) +
  scale_x_continuous(breaks = seq(0, 75, by = 5))+
  theme_void()+
  theme(axis.title.x = element_text(family= body_font, size=  16, margin = margin(10, 0, 0, 0)), 
        axis.title.y = element_blank(), 
        axis.text.y = element_text(family= body_font, size=  14), 
        axis.text.x = element_text(),
        axis.ticks.x = element_line(size = 0.5, color = 'gray'),
        axis.ticks.length = unit(.12, "cm"),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "#E8F6FF" , color = "#E8F6FF"),
        plot.background = element_rect(fill = "#E8F6FF", color = "#E8F6FF" ))+
  labs(y="Females")
fem
male<-ggplot(df, aes(x = Age, y = -MALE)) +
  geom_hline(yintercept = c(-1e6, -2e6,-3e6,-4e6),   color = "gray", size = 0.5)+
  geom_bar(stat = "identity", fill = "#6784AA",width = 0.8) +
  coord_flip() +
  scale_y_continuous(labels = function(x) scales::label_number(scale = 1e-6, suffix = " M")(abs(x)))+
  
  #scale_y_continuous(labels = function(x) scales::unit_format(unit = "M", scale = 1e-6)(abs(x)))) +
  #scale_y_continuous(labels = unit_format(unit = "M", scale::comma (abs(1e-6))))+
  #scale_y_continuous(labels = function(x) scales::comma(abs(x)))+
  theme_void()+
  theme(legend.position = 'none',
        #axis.line.y = element_blank(),
        #panel.grid = element_blank(),
        panel.background = element_rect( color = "#E8F6FF",fill = "#E8F6FF" ),
        plot.background = element_rect(fill = "#E8F6FF", color ="#E8F6FF"),
        axis.title.x = element_text(family= body_font, size=  18,  margin = margin(10, 0, 0, 0)), 
        axis.text.x = element_text(),
        axis.ticks.x = element_line(size = 0.5),
        axis.ticks.length = unit(.12, "cm")  ) +
  labs(y= "Males")


g<-male + fem +  plot_layout(ncol =2, widths = c(0.5, 0.5))

g_plus<- g + 
  labs(title  = "Pakistan’s Growing Population: Age and Gender Dynamics", 
       subtitle = "Age in Years",
       caption = "Vistales | Data: PBS - Population Census 2023")+
  theme(
    plot.background = element_rect(fill = "#E8F6FF", colour = "#E8F6FF"),
    plot.title = element_text(family = title_font, face = 'bold',
                              size = 30,color = "black", margin = margin(28, 0, 0, -440)),
    plot.subtitle = element_text(family = body_font, 
                                 size = 15,color = "black", margin = margin(10, -50, 0, -60)),
    plot.caption = element_text(family = body_font,
                                size = 10,color = "black", margin = margin(10, 0, 0, 0))
  )


showtext_opts(dpi = 320)
ggsave("pop.png", g_plus, dpi=320, width = 14, height = 10)
showtext_auto(TRUE)

