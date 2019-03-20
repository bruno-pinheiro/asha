library(magrittr)

# IMPORTAR OS DADOS -----------------------------------------------------------
data("setores")
data("ubs_malha")
data("ubs_pontos")
data("populacao")
data("prof_saude")
data("od_viagens")

# MODELAR A ACESSIBILIDADE ESPACIAL -------------------------------------------

centroides <- sf::st_centroid(setores)

## od_indicadores da malha vigente -------------------------------------------------------------
od_indicadores <- list(vigente = asha::asha_intersect(ubs_malha, centroides, "cnes", "cd_geocodi"))

#' O resultado tem 18948 setores, de 18953. Os cinco nao incluidos
#' nao estao dentro de nenhuma area de ubs, por diferencas na geometria.
#' Abaixo incluo diretamente as linhas com os setores ausentes. Busquei
#' no mapa as UBS correspondentes.

cd_setores <- setores$cd_geocodi[!(setores$cd_geocodi %in% od_indicadores$vigente$cd_geocodi)]
cd_ubs <- c("3121135", "4049934", "2788039", "2788217", "2788500")

od_indicadores$vigente <- od_indicadores$vigente %>%
  rbind(tibble::tibble(cd_geocodi = cd_setores, cnes = cd_ubs)) %>%
  dplyr::mutate(malha = "vigente") %>%
  tibble::as_tibble()

## od_indicadores da malha experimental --------------------------------------------------------
od_indicadores$prox <- od_viagens %>%
  dplyr::group_by(cd_geocodi) %>%
  # ha menor tempo com distancia igual
  dplyr::filter(tempo == min(tempo)) %>%
  dplyr::filter(distancias == min(distancias)) %>%
  dplyr::ungroup() %>%
  dplyr::select(cd_geocodi, cnes) %>%
  dplyr::mutate(malha = "experimento") %>%
  tibble::as_tibble()

# Extrair setores ausentes
cd_setores <- setores$cd_geocodi[!(setores$cd_geocodi %in% od_indicadores$prox$cd_geocodi)]
od_euclidiano <- asha::asha_nn(ubs_pontos, centroides, "cnes", "cd_geocodi", 5)

# terminar od_indicadores da malha experimental
od_indicadores$prox <- od_indicadores$prox %>%
  rbind(od_euclidiano %>%
          dplyr::filter(proximidade == 1, cd_geocodi %in% cd_setores) %>%
          dplyr::select(cd_geocodi, cnes) %>%
          dplyr::mutate(malha = "experimento"))


# UNIR BASES od_indicadores ---------------------------------------------------------------
od_indicadores <- do.call(rbind, od_indicadores) %>%
  dplyr::mutate(malha = factor(malha, levels = c("vigente", "experimento")))

od_indicadores <- od_indicadores %>%
  dplyr::left_join(populacao, by = "cd_geocodi") %>%
  dplyr::left_join(prof_saude, by = "cnes") %>%
  dplyr::mutate(oportunidades = enfermeiros + medicos,
         habitantes = tidyr::replace_na(habitantes, 0)) %>%
  dplyr::group_by(cnes) %>%
  dplyr::mutate(demanda = sum(habitantes, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(od_viagens[, c("cnes", "cd_geocodi", "distancias", "tempo")],
            by = c("cd_geocodi", "cnes")) %>%
  dplyr::rename(distancia = distancias)

# CALCULAR INDICADORES --------------------------------------------------------

## Acessibilidade viável -------------
od_indicadores <- asha::asha_av(od_indicadores, cnes, tempo, habitantes, malha, 15)

## Acessibilidade competitiva -------------
od_indicadores <- asha::asha_ac(od_indicadores, habitantes, cnes, malha, 1000)

# SALVAR ----------------------------------------------------------------------
usethis::use_data(od_indicadores)

rm(list = ls())
