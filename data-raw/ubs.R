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
  st_join(select(ubs_sp_areas, -cnes)) %>%
  mutate(sts = as.factor(sts),
         crs = as.factor(crs))


##### Identificar relacoes entre UBS e setores censitarios

###### UBSs associadas aos setores no modelo vigente


modelo_vigente <-
  ubs_sp_areas %>% select(cnes) %>%
  st_join(select(centroides_sp, cd_geocodi), join = st_intersects) %>%
  as.data.frame() %>%
  select(cd_geocodi, cnes)

setores <- setores_sp$cd_geocodi[!(setores_sp$cd_geocodi %in% modelo_vigente$cd_geocodi)]
cnes <- c("3121135", "4049934", "2788039", "2788217", "2788500") # levantado manualmente

modelo_vigente <-
  modelo_vigente %>%
  bind_rows(data.frame(cnes = cnes,
                       cd_geocodi = setores))


modelo_vigente <-
  ubs_sp_areas %>% select(cnes) %>%
  st_join(select(centroides_sp, cd_geocodi), join = st_intersects) %>%
  as.data.frame() %>%
  select(cd_geocodi, cnes)

setores <- setores_sp$cd_geocodi[!(setores_sp$cd_geocodi %in% setores_areas_ubs$cd_geocodi)]
cnes <- c("3121135", "4049934", "2788039", "2788217", "2788500") # levantado manualmente

modelo_vigente <-
  modelo_vigente %>%
  bind_rows(data.frame(cnes = cnes,
                       cd_geocodi = setores))

###### UBSs associadas aos setores no modelo de proximidade

ubs_proxima_setores <-
  st_distance(centroides_sp, base_ubs, longlat=F)

ubs_prox5 <-
  data.frame(t(apply(ubs_proxima_setores, 1, order)[ 1:10, ])) %>%
  bind_cols(data.frame(t(apply(ubs_proxima_setores, 1, sort)[ 1:10, ]))) %>%
  bind_cols(CD_GEOCODI=centroides_capital$CD_GEOCODI)

ubs_prox5 <-
  melt(ubs_prox5[, c(1:10, 21)], id="CD_GEOCODI") %>%
  bind_cols(melt(ubs_prox5[, c(11:21)], id="CD_GEOCODI")) %>%
  rename(Proximidade=variable, Distancia=value1) %>%
  mutate(CNES=base_ubs$CNES[value],
         Metros=round(Distancia)) %>%
  select(CD_GEOCODI, CNES, Metros, Proximidade)

ubs_prox5$Proximidade  <- gsub("X1", "1", ubs_prox5$Proximidade)
ubs_prox5$Proximidade  <- gsub("X2", "2", ubs_prox5$Proximidade)
ubs_prox5$Proximidade  <- gsub("X3", "3", ubs_prox5$Proximidade)
ubs_prox5$Proximidade  <- gsub("X4", "4", ubs_prox5$Proximidade)
ubs_prox5$Proximidade  <- gsub("X5", "5", ubs_prox5$Proximidade)
ubs_prox5$Proximidade  <- gsub("X6", "6", ubs_prox5$Proximidade)
ubs_prox5$Proximidade  <- gsub("X7", "7", ubs_prox5$Proximidade)
ubs_prox5$Proximidade  <- gsub("X8", "8", ubs_prox5$Proximidade)
ubs_prox5$Proximidade  <- gsub("X9", "9", ubs_prox5$Proximidade)
ubs_prox5$Proximidade  <- gsub("X10", "10", ubs_prox5$Proximidade)

glimpse(setores_areas_ubs)


devtools::use_data(ubs_sp)

rm(list=ls())

