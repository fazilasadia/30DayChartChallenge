library(ggplot2)
library(tidyverse)
library(ggthemes)




font_add_google("Lato", "title_font")
font_add_google("Roboto", "body_font")
showtext_auto()

title_font<- "title_font"

body_font<- "body_font"


data <- read.csv("C:/Users/CHD/Downloads/dataset (1).csv")

# Remittances: Pakistan's Economic Lifeline 

# Create a function to determine fiscal year based on given formula
get_fiscal_year <- function(date) {
  year <- as.integer(format(date, "%Y"))
  ifelse(month(date) >= 7, year + 1, year)
}

# Calculate fiscal year for each observation in the data frame
data$fiscal_year <- get_fiscal_year(as.Date(data$Observation.Date, format = "%d-%b-%Y"))

# Group by country and fiscal year, then calculate sum of remit values
df <- data %>%
  group_by(Series.Display.Name, Observation.Date) %>%
  summarise(remit = sum(Observation.Value)) %>%
  rename(country = Series.Display.Name,
         date = Observation.Date)%>%
  mutate(country = sub(".*?\\.?\\s+(\\b\\w+\\b)", "\\1", country))%>%
  filter(!str_detect(country, "Cash Flow|Other GCC Countries|Total|EU Countries|Encashment|Dubai|Sharjah|Abu Dhabi|Others|Other countries"))%>%
  arrange(country)%>%
  mutate(remit = round(remit/1000 , 2))%>%
  group_by()



df$date <- dmy(df$date)

monthly_data <- df %>%
  group_by(month_year = floor_date(date, "month")) %>%
  summarize(total_value = sum(remit, na.rm = TRUE))


annual_data <- df %>%
  mutate(fiscal_year = get_fiscal_year(date)) %>%
  group_by(fiscal_year) %>%
  summarize(total_value = sum(remit, na.rm = TRUE))


title = "Remittance Boom Props Up Pakistan’s Struggling Economy "
subtitle = "Smoothed annual growth reflects diaspora's resilience amid economic crises"
x = NULL  # Remove axis title for cleaner look
y = "Total Remittances (USD)"
caption = "Source: State Bank of Pakistan | Vistales"



ggplot(data = annual_data,aes(x= fiscal_year, y= total_value))+
  #geom_line()+
  geom_line(color = "#3C3C3C", size = 1.2) +
  #geom_smooth(color = "grey", alpha = 0.7)+
  geom_smooth(method = "loess", se = FALSE, color = "#0072B2", size = 2)+# Axis formatting
  scale_y_continuous(position = "right",
  labels = label_number(scale = 1, suffix = " B$ ", accuracy = 1),  # Formats as "10B"
  expand = expansion(mult = c(0.01, 0.1))  # Adds padding for annotations
) +
  #xlim(1975,2025)
  scale_x_continuous(breaks = seq(1975, 2025, by = 5)) +
  
  #scale_y_continuous(sec.axis = dup_axis()) +
  theme_economist() +
  theme(
    axis.ticks.length.x=unit(c(7,7), "pt"),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 18),
    # TITLE
    plot.title.position = "plot",
    plot.title = element_text(margin = margin(20, 0, 18, -5),
                                 size = 35,
                                 hjust = 0.5,
                              family = title_font,
                                 face = "bold",),
    plot.subtitle = element_text(margin = margin(5, 0, 14, -55),
                                    size = 25,
                                    hjust = 0.5),
    
    # Caption
    plot.caption = element_text(family=body_font,
                                
                                size=13, 
                                color="black",
                                hjust=0.5,
                                margin=margin(30,0,0,0)),
    
    #plot.background = element_rect(color="#f0f7dc", fill="#f0f7dc"),
    plot.margin = margin(15, 45, 25, 45)
  )+
  labs(title= title,
       subtitle = subtitle,
       caption = caption,
       x = x, y = NULL)


# ------ Save Plot ------ 

showtext_opts(dpi = 320)

ggsave("remit-w.png",dpi=320,
       width = 14, height = 10)
showtext_auto(TRUE)


