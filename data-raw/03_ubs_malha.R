# IMPORTAR OS DADOS -----------------------------------------------------------
file <- system.file("extdata/ubs_sp_areas",
                    "AA_UBS_MSP_2015_2016_UTM_SIRGAS2000_fuso23S.shp",
                    package = "asha")

ubs_malha <- sf::read_sf(file)

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
