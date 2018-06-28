#### CARREGAR PACOTES ###################
library(dplyr)
library(spdplyr)
library(sf)
library(lwgeom)
library(rgdal)


###### Dados de enfermeiros ------------

# Baixar dados
download.file("https://raw.githubusercontent.com/bruno-pinheiro/Mobilidade_Desigualdades/master/data/data_raw/quant_enfermeiros_set_2017.csv", "inst/extdata/enfermeiros.csv", quiet = TRUE)

# Organizar dados
enfermeiros <-
  read.csv(system.file("extdata", "enfermeiros.csv", package = "asha"),
           sep = ";", stringsAsFactors = FALSE) %>%
  mutate(cnes = substring(ESTAB_SA, first=1, last=7)) %>%
  rename(total_enf = TOTAL_ENF) %>%
  select(-ESTAB_SA)

################ Quantidade de médicos por UBS ------------------

# Baixar dados
download.file("https://raw.githubusercontent.com/bruno-pinheiro/Mobilidade_Desigualdades/master/data/data_raw/quant_medicos_set_2017.csv", "inst/extdata/medicos.csv", quiet = TRUE)

# Orgnizar dados
medicos <-
  read.csv(system.file("extdata", "medicos.csv", package = "asha"),
           sep=";", stringsAsFactors = FALSE) %>%
  mutate(cnes = substring(ESTAB_SA, first=1, last=7)) %>%
  rename(total_med = TOTAL_MED) %>%
  select(-ESTAB_SA)

###### Shape de areas de cobertura das UBS ------------------

######## Baixar dados

# guardar url do arquivo
if (!file.exists("inst/extdata/areas_ubs")) {
  tmp <- tempfile(fileext = ".zip")
  download.file('https://dataverse.harvard.edu/api/access/datafile/3092976', tmp, quiet = TRUE)
  unzip(tmp, exdir = "inst/extdata/areas_ubs")
  unlink(tmp)
}

# Importar arquivo no R
areas_ubs <-
  read_sf(dsn="inst/extdata/areas_ubs", layer="AA_UBS_MSP_2015_2016_UTM_SIRGAS2000_fuso23S",
          stringsAsFactors = FALSE) %>%
  st_transform(31983) %>%
  st_make_valid() %>%
  select(CNES, NOMEUBS, STS, CRS, SUBPREF) %>%
  rename_all(tolower) %>%
  merge(medicos, by = "cnes", all.x = TRUE) %>%
  merge(enfermeiros, by = "cnes", all.x = TRUE)


# Guardar para uso do pacote
devtools::use_data(areas_ubs)

rm(list=ls())
