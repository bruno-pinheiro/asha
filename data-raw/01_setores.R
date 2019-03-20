# BAIXAR OS DADOS -------------------------------------------------------------
data_comp <- osfr::osf_retrieve_node("bzk34") %>%
  osfr::osf_ls_nodes("Data")

files <- osfr::osf_ls_files(data_comp) %>%
  filter(stringr::str_detect(name, "^set"))

asha::clean_tmp()
tmp <- tempfile(fileext = ".rda")
osf_download(files, path = tmp)

# IMPORTAR OS DADOS -----------------------------------------------------------
file <- list_tmp("rda")
load(file)

# LIMPAR OS DADOS -------------------------------------------------------------
setores <- setores %>%
  dplyr::rename_all(tolower) %>%
  dplyr::filter(nm_municip == "SÃO PAULO") %>%
  dplyr::select(cd_geocodi, tipo, cd_geocodd, nm_distrit) %>%
  sf::st_transform(31983) %>%
  lwgeom::st_make_valid()

# EXPORTAR OS DADOS -----------------------------------------------------------
usethis::use_data(setores)

rm(list = ls())
