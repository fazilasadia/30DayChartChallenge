install.packages(c("ggimage", "ggbump", "countrycode","devtools", "tidyverse", "ggrepel", "showtext","ggetext"))
devtools::install_github('rensa/ggflags') 

library(ggplot2)
library(ggbump)
library(tidyverse)
library(showtext)
library(countrycode)
library(ggflags)
#library(ggrepel)  # For better label placement



data <- read.csv("dataset (1).csv")

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
  mutate(remit = round(remit/1000 , 2))



df$date <- dmy(df$date)


# Your data processing
remit_data_clean <- df %>%
  filter(year(date) >= 2020) %>%
  filter(year(date) <= 2024 ) %>%
  mutate(year = year(date)) %>%
  filter(!str_detect(country, "Other Countries"))%>%
  group_by(country, year) %>%
  summarise(total_remit = sum(remit, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    iso2c = tolower(countrycode(country, origin = "country.name", destination = "iso2c"))
  )

# Rank countries per year
ranked_data <- remit_data_clean %>%
  group_by(year) %>%
  mutate(rank = rank(-total_remit)) %>%
  filter(rank <= 10)

# Create data for end flags and labels
end_labels <- ranked_data %>%
  group_by(country) %>%
  filter(year == 2024) %>%
  ungroup() %>%
  mutate(
    # Add small offset to x-position for flags and labels
    flag_x = year + 0.2,  # Flags will appear 0.3 units right of last point
    label_x = year + 0.4  # Labels will appear 0.5 units right of last point
  )


start_labels <- ranked_data %>%
  group_by(country) %>%
  filter(year == 2020) %>%
  ungroup() %>%
  mutate(
    # Add small offset to x-position for flags and labels
    flag_x = year ,  # Flags will appear 0.3 units right of last point
    label_x = year - 0.4  # Labels will appear 0.5 units right of last point
  )



font_add_google("Outfit", "title_font")
font_add_google("Cabin", "body_font")


font_add_google("Roboto", "roboto")
font_add_google("Roboto Slab", "roboto_slab")

showtext_auto()

title_font <- "title_font"
body_font <- "body_font"


title_text <- "Remittance Inflows to Pakistan, 2020 - 2024"
subtitle_text <- "Saudi Arabia and UAE Stand Out as Top Contributors to Pakistan's Economy"
caption_text <- "Vistales      |     Data: State Bank of Pakistan"

end_labels <- ranked_data %>%
  filter(year == 2024)  # No need for group_by if you're just filtering

start_labels <- ranked_data %>%
  filter(year == 2020)


# Create the bump chart
ggplot(ranked_data, aes(x = year, y = rank, group = country, color = country)) +
  geom_bump(size = 1.2, smooth = 8) +
  geom_point(size = 3) +
  # Add flags with offset
  geom_flag(
    data = end_labels,
    aes(x = 2024.3, country = iso2c),  # Fixed position right of 2024
    size = 14  # Slightly reduced size
  ) +
  geom_flag(
    data = start_labels,
    aes(x = 2020, country = iso2c),  # Exact position at 2020
    size = 14
  )+
  
  geom_text(
    data = end_labels,
    aes(x = 2024, y = rank, label = paste0(scales::comma(total_remit), "$ B")),
    hjust = 0.55,
    vjust = -1,
    size = 6.5,
    fontface = "bold",
    family = body_font
  )+
  scale_y_reverse(
    breaks = 1:10,  # Explicitly set breaks at each rank
    labels = 1:10,  # Label each rank
    name = "Rank"   # Add y-axis title
  ) +
  scale_x_continuous(
    limits = c(min(ranked_data$year), 2024.5),  # Hard set upper limit to 2024
    breaks = seq(2020, 2024.5, by = 1),        # Explicit breaks every year
    expand = expansion(add = c(0.2, 0.5))    # Reduced right padding
  ) +
  theme_void() +
  labs(title = title_text,
       subtitle = subtitle_text,
       caption = caption_text,
       y= "Rank")+
  coord_cartesian(clip = "off")  + # Allows elements to extend beyond plot area
  
  theme(
    axis.title.x  = element_blank(),
    axis.title.y = element_text(  # Style for y-axis title
      family = body_font,
      face = "bold",
      size = 16,
      angle = 90,
      margin = margin(r = 10)
    ),
    axis.text.y = element_text(  # Style for rank numbers
      family = body_font,
      face = "bold",
      size = 14,
      margin = margin(r = 5)
    ),
    axis.text.x = element_text(family = body_font, face = "bold",
                               size=14),
    
    # Legend
    legend.position = "none",
    
    # TITLE
    plot.title.position = "plot",
    plot.title = element_text(margin = margin(0, 0, 18, -70),
                                 size = 28,
                                 hjust = 0.5,
                                 family = title_font,
                                 face = "bold"),
    plot.subtitle = element_text(margin = margin(0, 0, 15, -65),
                                    size = 18,
                                    hjust = 0.5,
                                    family = title_font,
                                    face = "bold"),
    
    # Caption
    plot.caption = element_text(family=body_font,
                                
                                size=13, 
                                color="black",
                                hjust=0.5,
                                margin=margin(30,90,0,0)),
    
    plot.background = element_rect(color="#f0f7dc", fill="#f0f7dc"),
    plot.margin = margin(35, -25, 25, 50)
  )+
  labs(x = "Year", y = "Rank")





# ------ Save Plot ------ 

showtext_opts(dpi = 320)

ggsave("remit.png",dpi=320,
       width = 14, height = 10)
showtext_auto(TRUE)
