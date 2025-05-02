library(tidyverse)
library(ggtext)
library(showtext)


font_add_google("Fraunces", "title_font")
font_add_google("Montserrat", "body_font")
showtext_auto()

title_font<- "title_font"

body_font<- "body_font"


raw=tibble( 
  labels=c(
    "Balochistan",
    "KPK",
    "Punjab",
    "Sindh"),
  Male=c(56.13,54.47,53.43,54.13),
  Female=c(43.87, 45.53,46.57,45.87)
)

df=raw %>% # raw is just the generated data
  
  # compute the gap
  mutate(gap=Female-Male) %>% 
  
  # find the maximum value by label
  group_by(labels) %>% 
  mutate(max=max(Male, Female)) %>% 
  ungroup() %>% 
  
  # sort the labels by gap value
  # note that its absolute value of gap
  mutate(labels=forcats::fct_reorder(labels, abs(gap)))  

# make into long format for easier plotting  
df_long=df %>% 
  pivot_longer(
    c(Male,Female)
  )

df_long %>% head()

# set a custom nudge value
nudge_value=0.8

p_main=
  df_long %>% 
  
  # the following 3 lines of code are the same
  ggplot(aes(x=value,y=labels)) +
  geom_line(aes(group=labels), color="#B8B8B8", linewidth=3.5) +
  geom_point(aes(color=name), size=3) +
  
  # but we want geom_text for the data callouts and the legend
  
  # data callout
  geom_text(aes(label=value, color=name),
            size=11.25,
            nudge_x=if_else(
              df_long$value==df_long$max, # if it's the larger value...
              nudge_value,   # move it to the right of the point
              -nudge_value), # otherwise, move it to the left of the point
            hjust=if_else(
              df_long$value==df_long$max, #if it's the larger value
              0, # left justify
              1),# otherwise, right justify      
  )+
  
  # legend
  geom_text(aes(label=name, color=name), 
            data=. %>% filter(gap==max(gap)),
            nudge_y =3.2,family = body_font, 
            fontface="bold",
            size=11.25)+  
  
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.y = element_text(family = body_font,color="black", size = 25, face = "bold"),
        axis.text.x = element_text(family = body_font,color="#6F6F6F",size = 20),
        axis.title = element_blank(),
        panel.grid = element_blank()
  ) +
  labs(x="%",y=NULL) +
  scale_color_manual(values=c("#BF2F24","#436685")) +
  
  #extend the y-axis otherwise the legend is cut off
  coord_cartesian(xlim=c(39, 60)) +
  
  #display percentages with % appended
  scale_x_continuous(labels = scales::percent_format(scale = 1)) +
  theme(plot.margin = margin(20,20,20,20),
        plot.title = element_text(family = body_font, face = 'bold',
                                  size = 37,color = "black",margin = margin(10, 20, 10, -40)),
        plot.subtitle = element_text(family = body_font, face = 'bold',
                                  size = 27,color = "black",margin = margin(5, 0, 10, -40)),
        plot.caption = element_text(family = body_font,
                                    size = 23,color = "black",margin = margin(5, -10, -10, 0)),
        plot.background = element_rect(fill = "#F7F7F7", colour = "#F7F7F7"),
        panel.background = element_rect(fill = "#F7F7F7", colour = "#F7F7F7"),
        legend.background =  element_rect(fill = "#F7F7F7", colour = "#F7F7F7")) +
  labs(title ="Ballots and Barriers: Mapping Gender Disparity in Voter Turnout",
       subtitle = "Punjab leads in gender parity while Balochistan trails with a 12-percent gap",
       caption = "Vistales | Data: Election Comission of Pakistan")


ggsave("inc.png", p_main, dpi=320, width = 6, height = 6)

