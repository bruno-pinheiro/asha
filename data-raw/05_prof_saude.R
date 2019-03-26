# BAIXAR OS DADOS -------------------------------------------------------------
enf <- fread(system.file("extdata", "enfermeiros.csv", package = "asha"))
med <- fread(system.file("extdata", "medicos.csv", package = "asha"))

asha::clean_tmp()
tmp <- tempfile(fileext = "csv")
osf_download(files[1, ], path = tmp)
tmp <- tempfile(fileext = "csv")
osf_download(files[2, ], path = tmp)

# IMPORTAR OS DADOS -----------------------------------------------------------
files <- list_tmp("csv")
medicos <- data.table::fread(files[1]) %>% as_tibble()
enfermeiros <- data.table::fread(files[2]) %>% as_tibble()

# LIMPAR OS DADOS -------------------------------------------------------------
prof_saude <- enfermeiros %>%
  dplyr::left_join(medicos, by = "ESTAB_SA") %>%
  dplyr::rename_all(tolower) %>%
  dplyr::mutate(cnes = stringr::str_extract(estab_sa, "[:digit:]+")) %>%
  dplyr::filter(cnes %in% ubs_malha$cnes) %>%
  dplyr::select(cnes, enfermeiros = total_enf, medicos = total_med)


# EXPORTAR OS DADOS -----------------------------------------------------------
usethis::use_data(prof_saude)

rm(list = ls())
