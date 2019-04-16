library(sf)
library(dplyr)

# IMPORTAR OS DADOS -----------------------------------------------------------
data("setores")
data("ubs_malha")
data("ubs_pontos")
data("populacao")
data("prof_saude")
data("od_viagens")
# load("~/Documentos/Dev/asha/inst/extdata/od_viagens_na.rda")
load("inst/extdata/dists_erros_setores.rda")

### Computar centroides
centroides <- sf::st_centroid(setores)

# Ranquear distâncias euclidianas
od_viagens <- od_viagens %>%
  arrange(cd_geocodi, distancias) %>%
  group_by(cd_geocodi) %>%
  mutate(rank = rank(tempo, ties.method = "first")) %>%
  ungroup()

## Criar ODs das malhas
od <- list(vigente = asha::asha_intersect(ubs_malha, centroides, "cnes", "cd_geocodi") %>%
             as_tibble() %>%
             mutate(malha = "Vigente") %>%
             arrange(cd_geocodi, cnes, malha))

od$proximidade <- od_viagens %>%
  filter(rank == 1) %>%
  mutate(malha = "Proximidade") %>%
  select(cd_geocodi, cnes, malha, distancias, tempo) %>%
  arrange(cd_geocodi, cnes)

#' O resultado tem 18948 setores, de 18953. Os cinco nao incluidos
#' nao estao dentro de nenhuma area de ubs, por diferencas na geometria.
#' Abaixo incluo diretamente as linhas com os setores ausentes. Busquei
#' no mapa as UBS correspondentes.

## Resolver setores faltantes da malha vigente
cd_setores <- setores$cd_geocodi[!(setores$cd_geocodi %in% od$vigente$cd_geocodi)]
cd_ubs <- c("3121135", "4049934", "2788039", "2788217", "2788500")

od$vigente <- od$vigente %>%
  rbind(tibble::tibble(cd_geocodi = cd_setores, cnes = cd_ubs, malha = "Vigente")) %>%
  left_join(od_viagens %>% select(cd_geocodi, cnes, distancias, tempo),
            by = c("cd_geocodi", "cnes"))

## Verificar NAs
sapply(od, function(x) table(is.na(x$tempo)))
## Restaram 701 setores sem identificação de tempo e distância na malha vigente
## Restaram 48 setores na malha de proximidade (estes precisa revisar qual a UBS mais próxima)

## Extrais observações com distâncias NA
od_na <- lapply(od, function(x) filter(x, is.na(tempo)))


#############################################################################################
# Obter distâncias e tempos de viagem para os casos sem dados de tempo da malha vigente

# Modelo vigente
centroides_vig_na <- setores %>%
  filter(cd_geocodi %in% od_na$vigente$cd_geocodi) %>%
  st_centroid()

ubs_vig_na <- ubs_pontos %>%
  filter(cnes %in% od_na$vigente$cnes) %>%
  distinct(cnes)

zonas_na <- asha_zones(centroides_vig_na, ubs_vig_na, "cd_geocodi", "cnes")

dists_vig_na <- asha_dists(od_na$vigente, zonas_na)

# Modelo proximidade
centroides_na_prox <- setores %>%
  filter(cd_geocodi %in% od_na$proximidade$cd_geocodi) %>%
  st_centroid()

ubs_prox_na <- ubs_pontos %>%
  filter(cnes %in% od_na$proximidade$cnes) %>%
  distinct(cnes)

zonas_na <- asha_zones(centroides_na_prox, ubs_prox_na, "cd_geocodi", "cnes")

dists_prox_na <- asha_dists(od_na$proximidade, zonas_na)

dists_na <- list(vigente = as_tibble(dists_vig_na),
                 proximidade = as_tibble(dists_prox_na))

## Unir colunas com nova rodada de busca de distâncias e tempos
od_na$vigente <- od_na$vigente %>%
  select(-distancias, -tempo) %>%
  cbind(select(dists_na$vigente, distancias, tempo)) %>%
  as_tibble()

od_na$proximidade <- od_na$proximidade %>%
  select(-distancias, -tempo) %>%
  cbind(select(dists_na$proximidade, distancias, tempo)) %>%
  as_tibble()


################ consertar setores com ubs erradas
erros <- od_indicadores %>%
  filter(malha == "experimento", cd_geocodi %in% dists_erros_setores$cd_geocodi) %>%
  select(cd_geocodi) %>%
  left_join(select(filter(od_viagens, rank == 1), cd_geocodi, cnes, distancia = distancias, tempo),
            by = "cd_geocodi") %>%
  mutate(erro = "erro", malha = "experimento") %>%
  select(cd_geocodi, cnes, malha, distancia, tempo, erro)

od_indicadores <- od_indicadores %>%
  select(cd_geocodi, cnes, malha, distancia, tempo) %>%
  mutate(erro = "não") %>%
  rbind(erros)
limpar <- which(od_indicadores$malha == "experimento" & od_indicadores$erro == "não" & od_indicadores$cd_geocodi %in% erros$cd_geocodi)
od_indicadores <- od_indicadores[-limpar, -6]

# UNIR BASES od_indicadores ---------------------------------------------------------------
od_indicadores <- do.call(rbind, od) %>%
  filter(!is.na(tempo)) %>%
  bind_rows(od_na) %>%
  dplyr::mutate(malha = factor(malha, levels = c("Vigente", "Proximidade")))

od_indicadores <- od_indicadores %>%
  dplyr::mutate(malha = factor(malha, levels = c("vigente", "experimento")))


od_indicadores <- od_indicadores %>%
  dplyr::left_join(populacao, by = "cd_geocodi") %>%
  dplyr::left_join(prof_saude, by = "cnes") %>%
  dplyr::mutate(oportunidades = enfermeiros + medicos,
         habitantes = tidyr::replace_na(habitantes, 0)) %>%
  dplyr::group_by(cnes) %>%
  dplyr::mutate(demanda = sum(habitantes, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  # merge(od_viagens[, c("cnes", "cd_geocodi", "distancias", "tempo")] %>% dplyr::rename(distancia = distancias),
  #           by = c("cd_geocodi", "cnes"), all.x = TRUE) %>%
  as_tibble()

# CALCULAR INDICADORES --------------------------------------------------------

## Acessibilidade viável -------------
od_indicadores <- asha::asha_av(od_indicadores, cnes, tempo, habitantes, malha, 15)

## Tratar distâncias NA
vars <- names(od_indicadores)

# distna <- filter(od_indicadores, is.na(av)) %>%
#   select(-distancia, -tempo, -minutos) %>%
#   left_join(select(od_viagens_na, cd_geocodi, cnes, distancia = distancias, tempo),
#             by = c("cd_geocodi", "cnes")) %>%
#   mutate(minutos = tempo / 60) %>%
#   select(vars)

# od_indicadores <- od_indicadores %>%
#   filter(!is.na(av)) %>%
#   rbind(distna)

## Acessibilidade competitiva -------------
od_indicadores <- asha::asha_ac(od_indicadores, habitantes, cnes, malha, 1000)
od_indicadores2 <- od_indicadores
# SALVAR ----------------------------------------------------------------------

usethis::use_data(od_indicadores, overwrite = TRUE)

rm(list = ls())



od_indicadores
