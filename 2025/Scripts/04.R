# Load required libraries
library(ggplot2)
library(treemapify)
library(showtext)
library(ggtext)
library(dplyr)
# Create the data frame
population_data <- data.frame(
  Province = c("KPK", "Punjab", "Sindh", "Balochistan", "Islamabad"),
  Total = c(40856097,
            127688922,
            55696147,
            14894402,
            2363863
  )
)


# Calculate Percentage of Total Population
population_data <- population_data %>%
  mutate(Percentage = round((Total / sum(Total)) * 100, 1),  # Convert to percentage
         Label = paste0(Province, "\n", Percentage, "%"))  # Create label with province name + percentage

population_data$Color <- ifelse(population_data$Province == "Punjab", "#34BE82", "#F4A442")



font_add_google("Outfit", "title_font")
font_add_google("Cabin", "body_font")


font_add_google("Roboto", "roboto")
font_add_google("Roboto Slab", "roboto_slab")

showtext_auto()

title_font <- "title_font"
body_font <- "body_font"



title_text <- "Pakistan’s Population Divide: Punjab vs. All Other Provinces"
subtitle_text <- ""
caption_text <- "Vistales | Data: Pakistan Bureau of Statistics"

# Plot Treemap
ggplot(population_data, aes(area = Total, fill = Color, label = Label)) +
  geom_treemap(color = "black") +  # Black borders for clarity
  geom_treemap_text(family = body_font, fontface = "bold", colour = "white", place = "centre", size = 20) +  # Labels inside
  scale_fill_identity() +  # Use defined colors
  theme_void() +
  labs(title = title_text,
       caption = caption_text)+
  theme(
    axis.title.x  = element_blank(),
    axis.title.y  = element_blank(),
    axis.text.x = element_text(family = body_font, face = "bold",
                               size=14),
    axis.text.y = element_blank(),
    
    # Legend
    legend.position = "none",
    
    # TITLE
    plot.title.position = "plot",
    plot.title = element_textbox(margin = margin(2, 0, 30, 0),
                                 size = 30,
                                 hjust = 0.5,
                                 family = title_font,
                                 face = "bold",
                                 width = unit(60, "lines")),
    
    # Caption
    plot.caption = element_text(family=body_font,
                                
                                size=15, 
                                color="black",
                                hjust=0.5,
                                margin=margin(20,50,0,1)),
    
    plot.background = element_rect(color="white", fill="white"),
    plot.margin = margin(50, 50, 50, 50)
  )




# ------ Save Plot ------ 

showtext_opts(dpi = 320)
ggsave("pop.png",dpi=320,
       width = 14, height = 10)
showtext_auto(TRUE)
