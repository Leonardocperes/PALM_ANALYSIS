library(sf)
library(dplyr)
library(ggplot2)
library(viridis)

remove(list=ls())

# Carregar o shapefile do mapa do mundo
mapa_mundo <- st_read("C:/Users/Windows/Downloads/doi_10.5061_dryad.ts45225__v2/tdwg_level3_shp")

# Carregar o arquivo CSV com as informações das espécies de palmeiras
dados_especies <- read.csv("C:/Users/Windows/Downloads/doi_10.5061_dryad.ts45225__v2/data/palms_in_tdwg3.csv")

# Calcular o número de espécies por país
num_especies_por_pais <- dados_especies %>%
  group_by(Area_code_L3) %>%
  summarise(SpecName = n())

# Juntar os dados do shapefile com os dados das espécies por país
mapa_mundo_especies <- left_join(mapa_mundo, num_especies_por_pais, by = c("LEVEL3_COD" = "Area_code_L3"))

# Definir as cores do mapa de calor usando a escala "viridis"
cores <- viridis(2)

# Plotar o mapa de calor
ggplot() +
  geom_sf(data = mapa_mundo_especies, aes(fill = SpecName)) +
  scale_fill_gradientn(colors = cores, na.value = "lightgrey") +
  theme_void() +
  labs(title = "Riqueza de Espécies de Palmeiras por País", fill = "N° de Espécies")

# Ordenar os países por número de espécies em ordem decrescente e selecionar os 15 primeiros
top_15_paises <- num_especies_por_pais %>%
  arrange(desc(SpecName)) %>%
  head(15)

# Adicionando coluna com o nome dos países
top_15_paises <- left_join(top_15_paises, mapa_mundo[, c("LEVEL3_COD", "LEVEL3_NAM")], by = c("Area_code_L3" = "LEVEL3_COD"))

# Plotar o gráfico de barras com os 15 países com maior número de espécies
ggplot(top_15_paises, aes(x = reorder(LEVEL3_NAM, - SpecName), y = SpecName)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title = "Número de Espécies de Palmeiras por País (Top 15)", x = "País", y = "Número de Espécies") +
  theme(legend.position = "none") +
  geom_text(aes(label = SpecName), vjust = -0.5, color = "black", size = 3)

num_esp_distintas <- dados_especies %>% 
  distinct(SpecName) %>% 
  n_distinct()

top_15_paises$porcentagem <- round((top_15_paises$SpecName/num_esp_distintas)*100,2)

ggplot(top_15_paises, aes(x = reorder(LEVEL3_NAM, - porcentagem), y = porcentagem)) +
  geom_bar(stat = "identity", fill = "darkred") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title = "% de Espécies de Palmeiras por País (Top 15)", x = "País", y = "% de Espécies") +
  theme(legend.position = "none") +
  geom_text(aes(label = porcentagem), vjust = -0.5, color = "black", size = 3)
