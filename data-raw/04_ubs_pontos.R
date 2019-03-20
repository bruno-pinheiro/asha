# BAIXAR OS DADOS -------------------------------------------------------------
data_comp <- osfr::osf_retrieve_node("bzk34") %>%
  osfr::osf_ls_nodes("Data")

files <- osfr::osf_ls_files(data_comp) %>%
  filter(stringr::str_detect(name, "ubs_pontos"))

asha::clean_tmp()
tmp <- tempfile(fileext = ".rda")
osf_download(files, path = tmp)

# IMPORTAR OS DADOS -----------------------------------------------------------
file <- list_tmp("rda")
load(file)
data("ubs_malha")

# LIMPAR OS DADOS -------------------------------------------------------------
ubs_pontos <- ubs_pontos %>%
  dplyr::select(cnes = eq_cnes) %>%
  sf::st_set_crs(31983)

## Limpar UBS fechadas da base de pontos
diferencas <- list(
  novas = dplyr::filter(ubs_malha, !(cnes %in% ubs_pontos$cnes)) %>%
    dplyr::select(cnes, endereco),
  fechadas = dplyr::filter(ubs_pontos, !(cnes %in% ubs_malha$cnes))
)

## Extrair as que não foram fechadas
diferencas$antigas <- filter(ubs_pontos, !(cnes %in% diferencas$fechadas$cnes))

## GEOCOLOCALIZAR NOVAS UBS ---------------------------------------------------
diferencas$novas_geocode <- ggmap::geocode(diferencas$novas$endereco)

diferencas$novas <- diferencas$novas_geocode %>%
  st_as_sf(coords = c("lon", "lat")) %>%
  st_set_crs(4326) %>%
  st_transform(31983) %>%
  bind_cols(diferencas$novas %>% as_tibble()) %>%
  mutate(cnes = as.character(cnes)) %>%
  select(cnes)

## UNIR PONTOS DE UBS NOVAS E ANTIGAS -----------------------------------------
ubs_pontos <- rbind(diferencas$antigas, diferencas$novas)

## CORRIGIR UBS COM ERRO DE LOCALIZAÇÃO ---------------------------------------
erros <- list(
  cnes = as.character(
    c(2787954, 2788306, 2788608, 2815273, 3219771, 3561186, 3708411,
      3762831, 4050223, 5731143, 6018912, 6194990, 6329527, 6332420, 6332471)
  ),
  endereco =
    c("R. Lajedo, 123 - Cidade Tiradentes, São Paulo - SP",
      "Av. Edu Chaves, 1197 - Parque Edu Chaves, São Paulo - SP",
      "R. Frei Fidelis Mota, 1001 – Parque Cruzeiro do Sul, São Paulo - SP",
      "R. Humberto de Almeida, 279, Chacara Santana, São Paulo - SP",
      "Tv. Somos Todos Iguais, 915 - Jardim da Conquista, São Paulo - SP",
      "UBS Vera Poty, São Paulo",
      "R. Rolando Curti, 701 - Vila Clara, São Paulo - SP, 04413-070",
      "UBS Vila Calu",
      "UBS Parque do Largo - Estrada da Baronesa. 1000, Parque do Lago, São Paulo - SP",
      "Rua Catarina Gabrielli, 236 - Americanópolis, São Paulo - SP",
      "R. Benedito Schunck, 8 - Emburá, São Paulo - SP, 04893-050",
      "AMA CHÁCARA CRUZEIRO DO SUL - Rua Mercedes Lopes - Vila Santana, São Paulo - SP",
      "R. Francisco de Sáles, 10 - Jardim das Palmas, São Paulo - SP, 05749-280",
      "UBS Barragem, São Paulo - SP",
      "UBS MARSILAC - Estrada Engenheiro Marsilac - Emburá, São Paulo - SP")
)


erros$geocode <- ggmap::geocode(erros$endereco)
erros$corrigidos <- erros$geocode %>%
  sf::st_as_sf(coords = c("lon", "lat")) %>%
  sf::st_set_crs(4326) %>%
  sf::st_transform(31983) %>%
  dplyr::bind_cols(cnes = erros$cnes) %>%
  dplyr::mutate(cnes = as.character(cnes)) %>%
  dplyr::select(cnes)
erros$check <- dplyr::filter(ubs_pontos, cnes %in% erros$cnes)

# u <- filter(ubs_malha, cnes %in% erros$corrigidos$cnes)
# leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE)) %>%
#   leaflet::addTiles() %>%
#   leaflet::addPolygons(data = st_transform(u, 4326),
#                        label = ~ htmltools::htmlEscape(cnes)) %>%
#   leaflet::addCircleMarkers(data = st_transform(erros$check, 4326),
#                             color = "red", label = ~ htmltools::htmlEscape(cnes)) %>%
#   leaflet::addCircleMarkers(data = st_transform(erros$corrigidos, 4326),
#                             color = "green", label = ~ htmltools::htmlEscape(cnes))

ubs_pontos <- ubs_pontos %>%
  dplyr::filter(!(cnes %in% erros$corrigidos$cnes)) %>%
  rbind(erros$corrigidos)

# EXPORTAR OS DADOS -----------------------------------------------------------
usethis::use_data(ubs_pontos)

rm(list = ls())
