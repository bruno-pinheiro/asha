# BAIXAR OS DADOS -------------------------------------------------------------
data_comp <- osfr::osf_retrieve_node("bzk34") %>%
  osfr::osf_ls_nodes("Data")

files <- osfr::osf_ls_files(data_comp) %>%
  dplyr::filter(stringr::str_detect(name, "ubs_malha"))

asha::clean_tmp()
tmp <- tempfile(fileext = ".rda")
osf_download(files, path = tmp)

# IMPORTAR OS DADOS -----------------------------------------------------------
file <- list_tmp("rda")
load(file)

# LIMPAR OS DADOS -------------------------------------------------------------
ubs_malha <- ubs_malha %>%
  dplyr::rename_all(tolower) %>%
  dplyr::select(cnes, nomeubs, sts, crs, logradouro, numero, cep) %>%
  sf::st_transform(31983) %>%
  lwgeom::st_make_valid()

# UNIR GEOMETRIAS DE UBS FECHADA ----------------------------------------------

### unir 2027291 a 2819856
ubs_malha  <- ubs_malha %>%
  dplyr::filter(cnes == "2819856" | cnes == "2027291") %>%
  sf::st_combine() %>%
  sf::st_union(by_feature = TRUE) %>%
  sf::st_set_crs(st_crs(31983)) %>%
  tibble::as_tibble() %>%
  sf::st_as_sf() %>%
  dplyr::mutate(cnes = "2819856") %>%
  dplyr::left_join(
    ubs_malha %>%
      tibble::as_tibble() %>%
      dplyr::select(-geometry),
    by = "cnes", all.x = TRUE
    ) %>%
  rbind(ubs_malha  %>% dplyr::filter(cnes != "2819856", cnes != "2027291"))

ubs_malha <- ubs_malha %>%
  mutate(cep = paste("CEP", cep)) %>%
  tidyr::unite("endereco", logradouro, numero, cep, sep = ", ")

# EXPORTAR OS DADOS -----------------------------------------------------------
usethis::use_data(ubs_malha)

rm(list = ls())
