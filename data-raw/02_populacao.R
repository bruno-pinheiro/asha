# BAIXAR OS DADOS -------------------------------------------------------------
data_comp <- osfr::osf_retrieve_node("bzk34") %>%
  osfr::osf_ls_nodes("Data")

file <- osfr::osf_ls_files(data_comp) %>%
  filter(stringr::str_detect(name, "pop"))

asha::clean_tmp()
tmp <- tempfile(fileext = ".rda")
osf_download(file, path = tmp)

# IMPORTAR OS DADOS -----------------------------------------------------------
file <- list_tmp("rda")
load(file)

# LIMPAR OS DADOS -------------------------------------------------------------
populacao <- populacao %>%
  dplyr::rename(cd_geocodi = Cod_setor) %>%
  # V001 = populacao moradoras em domicílios particulares e coletivos
  dplyr::select(cd_geocodi, habitantes = V001)

########## RESOLUÇÃO TEMPORÁRIA DE PROBLEMA EM CD_GEOCODI #####################
load(system.file("extdata", "pessoas_sp.rda", package = "asha"))

populacao <- populacao[, -1] %>%
  cbind(cd_geocodi = pessoas_sp[, 1]) %>%
  mutate(cd_geocodi = as.character(cd_geocodi)) %>%
  dplyr::select(cd_geocodi, habitantes)%>%
  tibble::as_tibble()

# EXPORTAR OS DADOS -----------------------------------------------------------
usethis::use_data(populacao)

rm(list = ls())
