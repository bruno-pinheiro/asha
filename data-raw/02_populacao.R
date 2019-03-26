# IMPORTAR OS DADOS -----------------------------------------------------------
load(system.file("extdata", "populacao_sp_capital-raw.rda", package = "asha"))
load(system.file("extdata", "renda_sp_capital-raw.rda", package = "asha"))

library(magrittr)

# LIMPAR OS DADOS -------------------------------------------------------------
renda <-   dplyr::mutate(renda, Cod_setor = as.character(Cod_setor))
populacao <- populacao %>%
  # V002 = renda dos domicílios particulares
  dplyr::bind_cols(select(renda, cd_geocodi = Cod_setor, renda_domicilios = V002)) %>%
  # V001 = populacao moradora em domicílios particulares e coletivos
  # V045 = homens moradores em domicílios particulares e coletivos
  # V089 = mulheres moradoras em domicílios particulares e coletivos
  dplyr::select(cd_geocodi,
                habitantes = V001, homens = V045, mulheres = V089,
                renda_domicilios)

# EXPORTAR OS DADOS -----------------------------------------------------------
usethis::use_data(populacao)

rm(list = ls())
