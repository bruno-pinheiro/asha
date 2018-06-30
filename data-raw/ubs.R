#### CARREGAR PACOTES ###################
library(dplyr)
library(spdplyr)
library(reshape2)
library(sf)
library(lwgeom)
library(rgdal)


#### BAIXAR DADOS -------------

# De enfermeiros
if (!file.exists(system.file("extdata", "enfermeiros.csv", package = "asha"))) {
  download.file("https://raw.githubusercontent.com/bruno-pinheiro/Mobilidade_Desigualdades/master/data/data_raw/quant_enfermeiros_set_2017.csv",
                "inst/extdata/enfermeiros.csv")
}

# De medicos
if(!file.exists(system.file("extdata", "medicos.csv", package = "asha"))) {
download.file("https://raw.githubusercontent.com/bruno-pinheiro/Mobilidade_Desigualdades/master/data/data_raw/quant_medicos_set_2017.csv",
              "inst/extdata/medicos.csv")
}

# De areas de UBS
if (!file.exists(system.file("extdata", "ubs_sp_areas/", package = "asha"))) {
  tmp <- tempfile(fileext = ".zip")
  download.file('https://dataverse.harvard.edu/api/access/datafile/3092976', tmp)
  unzip(tmp, exdir = "inst/extdata/ubs_sp_areas")
  unlink(tmp)
}

#### IMPORTAR DADOS -------------

enfermeiros <-
  read.csv(system.file("extdata", "enfermeiros.csv", package = "asha"),
           sep=";", stringsAsFactors = FALSE) %>%
  mutate(cnes = substring(ESTAB_SA, first=1, last=7)) %>%
  rename(total_enf = TOTAL_ENF) %>%
  select(-ESTAB_SA)

medicos <-
  read.csv(system.file("extdata", "medicos.csv", package = "asha"),
           sep=";", stringsAsFactors = FALSE) %>%
  mutate(cnes = substring(ESTAB_SA, first=1, last=7)) %>%
  rename(total_med = TOTAL_MED) %>%
  select(-ESTAB_SA)

ubs_sp_areas <-
  read_sf(dsn="inst/extdata/ubs_sp_areas", layer="AA_UBS_MSP_2015_2016_UTM_SIRGAS2000_fuso23S",
          stringsAsFactors = FALSE) %>%
  st_transform(31983) %>%
  st_make_valid() %>%
  select(CNES, NOMEUBS, STS, CRS, SUBPREF) %>%
  rename_all(tolower) %>%
  merge(medicos, by = "cnes", all.x = TRUE) %>% # adicionar medicos
  merge(enfermeiros, by = "cnes", all.x = TRUE) %>% # adicionar enfermeiros
  mutate(sts = as.factor(sts),
         crs = as.factor(crs))

load("data-raw/base_ubs.rda")
ubs_sp <-
  base_ubs %>%
  st_as_sf() %>%
  st_transform(31983) %>%
  st_make_valid() %>%
  select(-.id) %>%
  rename(cnes = CNES) %>%
  st_join(select(ubs_sp_areas, -cnes))

#### IDENTIFICAR SETORES NAS AREAS DE COBERTURA -----------

modelo_vigente <-
  asha_intersect(ubs_sp_areas, centroides_sp, "cnes", "cd_geocodi")

#' O resultado tem 18948 setores, de 18953. Os cinco nao incluidos
#' nao estao dentro de nenhuma area de ubs, por diferencas na geometria.
#' Abaixo identico os setores ausentes e busco no mapa as UBS correspondentes
#' para depois incluir no dataset

setores <- setores_sp$cd_geocodi[!(setores_sp$cd_geocodi %in% modelo_vigente$cd_geocodi)]
cnes <- c("3121135", "4049934", "2788039", "2788217", "2788500") # levantado manualmente

modelo_vigente <-
  modelo_vigente %>%
  rbind(data.frame(cd_geocodi = setores,
                   cnes = cnes))

#### IDENTIFICAR AS n UBS MAIS PROXIMAS DOS SETORES -----------

modelo_proximidade <-
  asha_nn(ubs_sp, centroides_sp, "cnes", "cd_geocodi", 5)

#### INCLUIR VARIAVEIS DE UBS DOS MODELOS NO SETORES CENSITARIOS -----------

setores_sp <-
  setores_sp %>%
  merge(modelo_vigente, by = "cd_geocodi") %>%
  rename(ubs_vigente = cnes) %>%
  merge(select(filter(modelo_proximidade, proximidade == 1), cnes, cd_geocodi),
               by="cd_geocodi") %>%
  rename(ubs_prox = cnes)

rm(modelo_vigente, modelo_proximidade,
   medicos, enfermeiros)

#### BASE UNICA DE PONTOS DE UBS E CENTROIDES DE SETORES -------------

# criar base simples de ubs
ubs_ids <-
  ubs_sp %>%
  select(cnes) %>%
  rename(id = cnes) %>%
  mutate(tipo = "UBS")

# criar base simples de centroides
centroides_ids <-
  centroides_sp %>%
  select(cd_geocodi) %>%
  rename(id = cd_geocodi) %>%
  mutate(tipo = "centroide")

# Unir os pontos de centroides e ubs num so objeto
base_saude_setores <-
  rbind(centroides_ids, ubs_ids)

rm(centroides_ids, ubs_ids)

]]
# Guardar para uso no pacote
devtools::use_data(ubs_sp_areas, overwrite = T)
devtools::use_data(ubs_sp, overwrite = T)
devtools::use_data(setores_sp, overwrite = T)
devtools::use_data(base_saude_setores)

rm(list=ls())

