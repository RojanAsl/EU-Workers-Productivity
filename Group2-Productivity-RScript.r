# VPDA - MECD - FEUP

# Evolution of Productivity in European Countries from 1995 to 2021
# Group 2 : Luis Henriques, Farzam Salimi, Rojan Aslani.
# October 2022

# Please replace the following characters the first time running the script:
# - Line 87: ??? to €
###############################################################################

library(dplyr)
library(lubridate)
library(lattice)
library(readxl)
library(tidyverse)
library(viridis)
library(ggplot2)
library(tidyr)
library(tibble)
library(hrbrthemes)
library(ggthemes)
library(scales)


# Upload data
ds <-  read_excel("productivity.xlsx")
colnames(ds) <- c('year','Germany','Austria','Belgium','Bulgary','Cyprus','Croatia',
                  'Denmark','Slovakia','Slovenia','Spain','Estonia','Finland','France',
                  'Greece','Hungary','Ireland','Italy','Latvia','Lithuania','Luxembourg',
                  'Malta','Netherlands','Poland','Portugal','Czech Republic','Romania',
                  'Sweden','United Kingdom')


ds_tall <- pivot_longer(ds, cols=2:29, names_to = "country",
                        values_to = "productivity", values_drop_na = FALSE)

ds_tall$country <- as.factor(x = ds_tall$country)


# Replacing the NA values
ds_tall[ ds_tall == 0] <- NA
ds_tall <- data.frame( ds_tall, is.na (ds_tall) )
ds_tall <- VIM::kNN( ds_tall , variable = c( "productivity" ) , dist_var = c('country','year'), k = 2 )

ds_tall <- ds_tall %>% select(year, country, productivity)


# Data Exploration example
# other tools used for exploration: tableau, excel

#ds_tall %>%
#  ggplot(aes(x = productivity)) +
#  geom_histogram(fill= "blue", alpha = 0.5, color = "black", bins = 20) +
#  theme_minimal() +
#  labs (x= "Productivity",
#        y = "Frequency",
#        title = "Distribution of Productivity in European Countries (1995-2021)")


# Ordering and plotting the data
xax <- c(2000,2010,2020)

heatmap <- ds_tall %>%
  mutate(country = fct_reorder(country, productivity, .fun='max')) %>% # Sorting the data
  ggplot( aes(x = year, y = country)) +
  geom_tile(aes(fill = productivity)) +
  scale_fill_viridis(name = NULL, alpha = 1, begin = 0.1, end = 1, direction = -1,
                     discrete = FALSE, option = "G") +
  guides(fill = guide_colourbar(barwidth = 30, barheight = 0.8),
         size = guide_legend(order = 3)) +
  theme_economist() + 
  theme(legend.position ='top',
        legend.justification = 'top',
        legend.text = element_text(size = 9, family="Merriweather"),
        plot.background = element_rect(fill = '#C3D6DF'),
        plot.title = element_text(size = rel(1.5), family="Merriweather",hjust = 0.5, margin=margin(0,0,15,0)),
        plot.subtitle = element_text( size = rel(1.0) , hjust = 0.5, family="Merriweather"),
        plot.caption = element_text(size = rel(0.8), hjust = 1, family="Merriweather"),
        aspect.ratio = 1/1.61803398875,
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.title.x = element_text(size = rel (1.2), hjust = 0.5, vjust = 0.5),
        axis.text.x = element_text(vjust = 0.5, family="Merriweather"), #angle = 45,
        axis.text.y = element_text(hjust= 1, vjust = 0.5, family="Merriweather")) +
  scale_x_continuous("", labels = as.character(xax), breaks = xax) +
  labs ( x = NULL, y = NULL,
         title = 'Productivity of European Countries From 1995 to 2021',
         subtitle = 'Productivity - value produced per working hour (€/hour)',
         caption=paste('Authors: Luis Henriques, Rojan Aslani, Farzam Salimi. Created in', Sys.Date()))


ggsave("projectVisualization.png", plot = heatmap , width = 20 * 1.61803398875,
       height = 20, units = "cm", dpi = 1000)
         