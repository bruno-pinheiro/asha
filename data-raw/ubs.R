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

ubs_sp_profissionais <-
  merge(read.csv(system.file("extdata", "enfermeiros.csv", package = "asha"),
                 sep=";", stringsAsFactors = FALSE),
        read.csv(system.file("extdata", "medicos.csv", package = "asha"),
                 sep=";", stringsAsFactors = FALSE),
        by = "ESTAB_SA") %>%
  mutate(cnes = substring(ESTAB_SA, first=1, last=7)) %>%
  rename_all(tolower) %>%
  select(sort(names(.)), -estab_sa) %>%
  filter(cnes %in% ubs_sp$cnes)

ubs_sp_profissionais <-
  ubs_sp_profissionais %>%
  bind_rows(data.frame(cnes = ubs_sp$cnes[!(ubs_sp$cnes %in% ubs_sp_profissionais$cnes)],
                   total_enf = NA, total_med = NA)) %>%
  arrange(cnes)

ubs_sp_areas <-
  read_sf(dsn="inst/extdata/ubs_sp_areas", layer="AA_UBS_MSP_2015_2016_UTM_SIRGAS2000_fuso23S",
          stringsAsFactors = FALSE) %>%
  st_transform(31983) %>%
  st_make_valid() %>%
  select(CNES, NOMEUBS, STS, CRS, SUBPREF) %>%
  rename_all(tolower) %>%
  mutate(sts = as.factor(sts),
         crs = as.factor(crs))

load("data-raw/base_ubs.rda")
ubs_sp <-
  base_ubs %>%
  st_as_sf() %>%
  st_transform(31983) %>%
  st_make_valid() %>%
  select(-.id) %>%
  rename(cnes = CNES)

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

rm(modelo_vigente, modelo_proximidade)

#### BASE UNICA DE PONTOS DE UBS E CENTROIDES DE SETORES -------------

zonas <- asha_zones(centroides_sp, ubs_sp, "cd_geocodi", "cnes")

# Guardar para uso no pacote
devtools::use_data(ubs_sp_areas, overwrite = T)
devtools::use_data(ubs_sp, overwrite = T)
devtools::use_data(setores_sp, overwrite = T)
devtools::use_data(zonas, overwrite = T)
devtools::use_data(ubs_sp_profissionais, overwrite = T)

rm(list=ls())



asha_zones <- function(sf1, sf2, id1, id2) {
  id = NULL
  tipo = NULL
  rbind(
    sf1 %>%
      select(!!id1) %>%
      rename(id = !!id1) %>%
      mutate(tipo = !!id1),
    sf2 %>%
      select(!!id2) %>%
      rename(id = !!id2) %>%
      mutate(tipo = !!id2)
  )
}


rm(centroides_ids, ubs_ids)
