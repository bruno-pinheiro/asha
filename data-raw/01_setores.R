# IMPORTAR OS DADOS -----------------------------------------------------------
load(system.file("extdata", "setores_censitarios-raw.rda", package = "asha"))

# LIMPAR OS DADOS -------------------------------------------------------------
setores <- setores %>%
  dplyr::rename_all(tolower) %>%
  dplyr::filter(nm_municip == "SÃO PAULO") %>%
  dplyr::select(cd_geocodi, tipo, cd_geocodd, nm_distrit) %>%
  sf::st_transform(31983) %>%
  lwgeom::st_make_valid()

# EXPORTAR OS DADOS -----------------------------------------------------------
usethis::use_data(setores, overwrite = TRUE)

rm(list = ls())
