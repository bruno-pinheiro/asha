# CARREGAR PACOTES --------------------------------------------------------
library(dplyr)
library(sf)
library(asha)

load("data-raw/setores_censitarios-raw.rda")
data("ubs_pontos")


# PONTOS DE ORIGEM (CENTROIDES DOS SETORES) -----------------------------------
centroides <- st_centroid(setores) %>% select(cd_geocodi)

# OD EUCLIDIANO ---------------------------------------------------------------
# Isto sigfica identificar as "n" UBS mais proximas de cada setor censitario.
# No caso estamos trabalhando com as 5 UBS mais proximas.
od_euclidiano <- asha_nn(ubs_pontos, centroides, "cnes", "cd_geocodi", 5) %>%
  as_tibble()

# LEVANTAR ROTAS --------------------------------------------------------------
# O levantamento de dados de rotas consiste na consulta à Google Distance
# Matrix API para obtencao de dados de rotas (distancia e tempo de viagem)
# entre as 94765 conexões de origem e destino criadas em `od_euclidiano`.

## Preparar base de zonas ---------------
zonas <- asha_zones(centroides, ubs_pontos, "cd_geocodi", "cnes")

## Levantar dados de rotas ---------------
# Esta etapa demora muito. Mais de um dia.
od_viagens <- asha_dists(od_euclidiano, zonas)

## Renomear as varíávies
od_viagens <- od_viagens %>%
  rename(cd_geocodi = code_o,
         cnes = code_d,
         de = from_addresses,
         para = to_addresses,
         distancias = distances,
         tempo = duration,
         ox = fx, oy = fy,
         dx = tx, dy = ty) %>%
  select(-currency, -fare)

# SALVAR ----------------------------------------------------------------------
usethis::use_data(od_viagens)
