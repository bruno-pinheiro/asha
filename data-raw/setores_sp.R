#### CARREGAR PACOTES ###################
library(dplyr)
library(spdplyr)
library(rgdal)
library(sf)
library(lwgeom)
library(data.table)

#### BAIXAR E PREPARAR BASES BRUTAS ################

options(scipen = 999)

###### Dados do Censo de 2010 agregados por setor censitario ------------

######## Baixar dados -------------

if (!file.exists("inst/extdata/sp_resultados_universo")) {
  tmp <- tempfile(fileext = ".zip")
  download.file("ftp://ftp.ibge.gov.br/Censos/Censo_Demografico_2010/Resultados_do_Universo/Agregados_por_Setores_Censitarios/SP_sp_20180416.zip", tmp, quiet = TRUE)
  unzip(tmp, exdir = "inst/extdata/sp_resultados_universo")
  unlink(tmp)
}

#' ANTES DE CONTINUAR EU MOVI MANUALMENTE OS ARQUIVOS
#' Domicilio02_SP1.csv e DomicilioRenda_SP1.csv PARA
#' O DIRETORIO data-raw/sp_resultados_universo/

####### Importar no R --------

# Importar Domicilio02_SP1.csv
## V001 Moradores em domicilios particulares e coletivos
pessoas_setor <-
  fread(system.file("extdata", "Domicilio02_SP1.csv", package = "asha"),
        sep = ";", dec = ",", encoding = "Latin-1", fill = T,
        stringsAsFactors = FALSE) %>%
  mutate(cd_geocodi=as.character(Cod_setor)) %>%
  select(cd_geocodi, V001) %>%
  rename(pessoas_setor = V001)

###### Shape de setores censitários do estado de Sao Paulo ------------

######## Baixar dados -------------

if (!file.exists("inst/extdata/sp_setores_censitarios")) {
  tmp <- tempfile(fileext = ".zip")
  download.file("ftp://geoftp.ibge.gov.br/organizacao_do_territorio/malhas_territoriais/malhas_de_setores_censitarios__divisoes_intramunicipais/censo_2010/setores_censitarios_shp/sp/sp_setores_censitarios.zip", tmp, quiet = TRUE)
  unzip(tmp, exdir = "inst/extdata/sp_setores_censitarios")
  unlink(tmp)
}

#### PREPARAR A BASE DE DADOS ############

setores_sp <-
  readOGR(dsn="inst/extdata/sp_setores_censitarios", layer="35SEE250GC_SIR",
          stringsAsFactors = F, encoding="windows-1252") %>%
  st_as_sf() %>%
  st_make_valid() %>%
  st_transform(31983) %>%
  filter(CD_GEOCODM == "3550308") %>%
  rename_all(tolower) %>%
  select(cd_geocodi, cd_geocodd, nm_distrit, tipo) %>%
  merge(pessoas_setor, by = "cd_geocodi", all.x=T) %>%
  mutate(tipo = as.factor(tipo),
         area = as.numeric(st_area(.) / 1000000),
         dens_demografica = pessoas_setor / area,
         nm_distrit = iconv(nm_distrit, from = "UTF-8", to = "ASCII//TRANSLIT"))

#### Criar centroides

centroides_sp <-
  setores_sp %>%
  st_centroid() %>%
  select(cd_geocodi)

# Guardar para uso no pacote
devtools::use_data(setores_sp)
devtools::use_data(centroides_sp)

rm(list=ls())
