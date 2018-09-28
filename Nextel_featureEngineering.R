## Featrue Engineering

# Criacao de id
#id <- 1:nrow(houses)
#houses_id <- cbind(id, houses)

## Features Geográficas
geo <- houses %>% select(price, latitude, longitude)

# Initialize total within sum of squares error: wss
wss <- 0

# For 1 to 15 cluster centers
for (i in 1:15) {
  km.out <- kmeans(geo %>% select(-c(price)), centers = i, nstart = 50)
  # Save total within sum of squares to wss variable
  wss[i] <- km.out$tot.withinss
}

# Plot total within sum of squares vs. number of clusters
plot(1:15, wss, type = "b", 
     xlab = "Number of Clusters", 
     ylab = "Within groups sum of squares")

# Set k equal to the number of clusters corresponding to the elbow location
k_latLon <- 3
km_geo <- kmeans(geo %>% select(-price), centers = k_latLon, nstart = 50)
ggplot(geo, aes(latitude, longitude, col = as.factor(km_geo$cluster))) +
  geom_point()
ggplot(geo, aes(x = as.factor(km_geo$cluster), price)) +
  geom_boxplot()

latLon_cluster <- km_geo$cluster

# zip
ggplot(houses, aes(x = as.factor(zip), fil = price)) +
  geom_bar()
# representatividade bem baixa de algumas features. Testar com e sem e ver

## categoricas - coercao em classes mais representativas

# num_bed
categoricalCoerc <- houses %>% 
  select(c(num_bed, num_bath, num_floors, condition, renovation_date)) %>% 
  mutate(num_bed_c = ifelse(num_bed < 3, 
                            2, ifelse(num_bed > 4, 5, num_bed)),
         num_bath_c = ifelse(num_bath < 1.5,
                             1.25, ifelse(num_bath > 3.5, 3.75, num_bath)),
         num_floors_c = ifelse(num_floors > 2, 2.5, num_floors),
         condition_c = ifelse(condition < 3, 3, condition), 
         renovation_date_c = ifelse(renovation_date == 0, 0, 1)) %>% 
  select(-c(num_bed, num_bath, num_floors, condition, renovation_date))

# is_waterfront - baixíssima variância e impacto muito razoável no preço
summary(factor(houses$is_waterfront))

# Sumarização das novas Features
houses_nf <- cbind(houses, latLon_cluster, categoricalCoerc) %>% 
  select(-c(latitude, longitude, num_bed, num_bath, num_floors, condition, renovation_date, is_waterfront))

## One hot Encoding

# factorização
houses_fact <- houses_nf %>%
  mutate_at(
    .vars = vars('zip', 'latLon_cluster','num_bed_c', 'num_bath_c', 'num_floors_c', 'condition_c', 
                 'renovation_date_c'),
    .funs = funs(as.factor(.))
  )

# Sumarizando as colunas
summarizeColumns(houses_fact) %>%
  knitr::kable(digits = 2) # gerador de tabelas muito prático do knitr

# Normalização
houses_norm <- normalizeFeatures(houses_fact, target = "price")

# One hot encoding
houses_ohe <- createDummyFeatures(
  houses_norm, target = "price",
  cols = c(
    'zip', 'latLon_cluster','num_bed_c', 'num_bath_c', 'num_floors_c', 'condition_c', 
    'renovation_date_c'
  )
)


