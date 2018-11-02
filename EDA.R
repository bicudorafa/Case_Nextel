### EDA

## Abertura do df
houses <- read.csv('house_sales.csv')

# Pacotes necessários
library(dplyr)
library(tidyr)
library(ggplot2)
theme_set(theme_classic())


## Basicas
summary(houses)
sapply(houses, function(x) length(unique(x)))
glimpse(houses)

## Visualizacao interativa
library(leaflet)

leaflet(data = houses) %>% addTiles() %>% 
  addMarkers(~longitude, ~latitude, popup = ~as.character(price), 
             clusterOptions = markerClusterOptions()
  )

# visualizacao das variaveis continuas
target <- 'price'
categorical <- c('num_bed', 'num_bath', 'num_floors', 'is_waterfront', 'condition', 'renovation_date')
numeric <- setdiff(names(houses), c(target, categorical))

plots_num <- list()
for (var in numeric) {
  plots_num[[var]] <- ggplot(houses, aes_string(houses[[var]], houses[['price']])) + 
    geom_point() +
    geom_smooth(se = F) + 
    ggtitle(paste(var, 'x price')) +
    theme_minimal()
  print(plots_num[[var]])
}
plots_num = list()
# features que mereceriam clusterização ou pca: year_built, (lat x long), zip. Demais possuem relações n lineares

# visualizacao das variaveis categoricas

plots_cat <- list()
for (var in categorical) {
  plots_cat[[var]] <- ggplot(houses, aes_string(x = as.factor(houses[[var]]))) + 
    geom_bar(stat = 'count') +
    ggtitle(paste('Total Observations in ',var)) +
    theme_minimal()
  print(plots_cat[[var]])
}
plots_cat = list()
# num_bed: pouca concentração a partir de 6, e uma casa com 33
#houses %>% filter(num_bed == 33)
