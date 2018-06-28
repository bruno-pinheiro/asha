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
if (!file.exists("inst/extdata/ubs_sp_areas")) {
  tmp <- tempfile(fileext = ".zip")
  download.file('https://dataverse.harvard.edu/api/access/datafile/3092976', tmp, quiet = TRUE)
  unzip(tmp, exdir = "inst/extdata/ubs_sp_areas")
  unlink(tmp)
}

# Importar arquivo no R
ubs_sp_areas <-
  read_sf(dsn="inst/extdata/ubs_sp_areas", layer="AA_UBS_MSP_2015_2016_UTM_SIRGAS2000_fuso23S",
          stringsAsFactors = FALSE) %>%
  st_transform(31983) %>%
  st_make_valid() %>%
  select(CNES, NOMEUBS, STS, CRS, SUBPREF) %>%
  rename_all(tolower) %>%
  merge(medicos, by = "cnes", all.x = TRUE) %>%
  merge(enfermeiros, by = "cnes", all.x = TRUE)


# Guardar para uso do pacote
devtools::use_data(ubs_sp_areas)

rm(list=ls())


###### Dados de UBS ------------

load("data-raw/base_ubs.rda")

ubs_sp <-
  base_ubs %>%
  st_as_sf() %>%
  st_transform(31983) %>%
  st_make_valid() %>%
  select(-.id) %>%
  rename(cnes = CNES) %>%
  merge(select(as.data.frame(areas_ubs), -geometry), by = "cnes") %>%
  mutate(sts = as.factor(sts),
         crs = as.factor(crs))

str(ubs_sp)


setores_areas_ubs <-
  ubs_sp_areas %>% select(cnes) %>%
  st_join(select(centroides_sp, cd_geocodi), join = st_intersects) %>%
  as.data.frame() %>%
  select(cd_geocodi, cnes)

setores <- setores_sp$cd_geocodi[!(setores_sp$cd_geocodi %in% setores_areas_ubs$cd_geocodi)]
cnes <- c("3121135", "4049934", "2788039", "2788217", "2788500") # levantado manualmente

setores_areas_ubs <-
  setores_areas_ubs %>%
  bind_rows(data.frame(CNES=cnes,
                       CD_GEOCODI=setores))




devtools::use_data(ubs_sp)

rm(list=ls())


