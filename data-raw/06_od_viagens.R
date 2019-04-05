# CARREGAR PACOTES --------------------------------------------------------
library(dplyr)
library(sf)
library(asha)

data("setores")
data("ubs_pontos")

# PONTOS DE ORIGEM (CENTROIDES DOS SETORES) -----------------------------------
centroides <- st_centroid(setores) %>% select(cd_geocodi)

# OD EUCLIDIANO ---------------------------------------------------------------
# Isto sigfica identificar as "n" UBS mais proximas de cada setor censitario.
# No caso estamos trabalhando com as 5 UBS mais proximas.
od_euclidiano <- asha_nn(ubs_pontos, centroides, "cnes", "cd_geocodi", 5) %>%
  as_tibble()

# odprox_euc <- list(
#   setor = od_euclidiano %>% filter(proximidade == 1) %>%
#     arrange(cd_geocodi) %>% select(cd_geocodi) %>%
#     left_join(as_tibble(centroides), by = "cd_geocodi") %>%
#     st_sf() %>% st_transform(4326),
#   ubs = od_euclidiano %>% filter(proximidade == 1) %>%
#     arrange(cd_geocodi) %>% select(cnes) %>%
#     left_join(as_tibble(ubs_pontos), by = "cnes") %>%
#     st_sf() %>% st_transform(4326)
#   )

# setor <- as.matrix(st_coordinates(st_transform(odprox_na$setor[1:3, ], 4326)))
# ubs <- as.matrix(st_coordinates(st_transform(odprox_na$ubs[1:3, ], 4326)))

asha_dist <- function(from, to, mode, api) {
  if ("sf" %in% class(from)) {
    from = as.matrix(st_coordinates(st_transform(from, 4326)))
    to = as.matrix(st_coordinates(st_transform(to, 4326)))
  }

  dist <- lapply(1:nrow(from), function(i) {
    from = from[i, ]
    to = to[i, ]
    stplanr::dist_google(from = from, to = to, mode = mode, google_api = api)
    print(i)
  })
  dist <- do.call(rbind, dist)
  return(dist)
}

odvig_na <- list(
  setor = od_indicadores %>% filter(is.na(distancia), malha == "vigente") %>%
    arrange(cd_geocodi) %>% select(cd_geocodi, cnes) %>%
    left_join(as_tibble(centroides), by = "cd_geocodi") %>%
    st_sf() %>% st_transform(4326),
  ubs = od_indicadores %>%  filter(is.na(distancia), malha == "vigente") %>%
    arrange(cd_geocodi) %>% select(cnes) %>%
    left_join(as_tibble(ubs_pontos), by = "cnes") %>%
    st_sf() %>% st_transform(4326)
  )

zonasvig <- asha_zones(odvig_na$setor, distinct(odvig_na$ubs, cnes), "cd_geocodi", "cnes")
od_viagens <- asha_dists(odvig_na$setor, zonasvig, modal = "walking")

od_viagens_na <- as_tibble(odvig_na$setor) %>%
  select(-geometry) %>%
  cbind(od_viagens)

# LEVANTAR ROTAS --------------------------------------------------------------
# O levantamento de dados de rotas consiste na consulta à Google Distance
# Matrix API para obtencao de dados de rotas (distancia e tempo de viagem)
# entre as 94765 conexões de origem e destino criadas em `od_euclidiano`.

## Preparar base de zonas ---------------
zonas <- asha_zones(centroides, ubs_pontos, "cd_geocodi", "cnes")

## Levantar dados de rotas ---------------
# Esta etapa demora muito. Mais de um dia.
od_viagens <- asha_dists(od_euclidiano, zonas, modal = "walking")





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
