library(dplyr)

#### FUNCAO asha_dists FUNCIONANDO COM UM ERRO

api01 <- "AIzaSyBRPrAjSE_pRMWSq_XlO4BFwGD63j_gB4U"
api02 <- "AIzaSyCMQ5yJQ6UmiHCOT8M-S5mwENpAa2BZcMs"
# api03 <- AIzaSyBBKLBM_tckdQQxURAaGI5YRFKD0vVkX9U
apis <- c("AIzaSyBRPrAjSE_pRMWSq_XlO4BFwGD63j_gB4U",
          "AIzaSyCMQ5yJQ6UmiHCOT8M-S5mwENpAa2BZcMs",
          "AIzaSyBBKLBM_tckdQQxURAaGI5YRFKD0vVkX9U")

dist_vigente <- asha_dists(modelo_vigente, base_saude_setores, api02)
dist_prox <- asha_dists(modelo_proximidade[1:20, ], base_saude_setores, api02)

load("data-raw/rotas.rda")

ubs_sp_mobilidade <-
  rotas %>%
  rename(cd_geocodi = code_o,
         cnes = code_d,
         de = from_addresses,
         para = to_addresses,
         distancias = distances,
         tempo = duration,
         ox = fx, oy = fy,
         dx = tx, dy = ty) %>%
  select(-currency, -fare)

str(ubs_sp_mobilidade)

load("data-raw/rotas_faltas.rda")
rotas_faltas <-
  rotas_faltas %>%
  select(-currency, -fare, - MINUTOS) %>%
  rename(cd_geocodi = CD_GEOCODI,
         cnes = CNES,
         de = from_addresses,
         para = to_addresses,
         distancias = DISTANCIAS,
         tempo = SEGUNDOS,
         ox = fx, oy = fy,
         dx = tx, dy = ty)
names(rotas_faltas)

devtools::use_data(ubs_sp_mobilidade, overwrite = TRUE)

devtools::use_data(rotas_faltas, overwrite = TRUE)
