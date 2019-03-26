library(asha)
library(magrittr)

# BAIXAR BASES BRUTAS ---------------------------------------------------------

## Dados do Censo 2010 agregados por setor censitario -------------------------
get_censo("spcapital")

## Malha espacial de setores censitários --------------------------------------
get_censo("sp", "geo")

## UBS geolocalizadas (Prefeitura de São Paulo \ GEOSAMPA) --------------------
get_zip("http://geosampa.prefeitura.sp.gov.br/PaginasPublicas/downloadArquivoOL.aspx?orig=DownloadCamadas&arq=03_Equipamentos%5C%5CSa%FAde%5C%5CShapefile%5C%5CEQUIPAMENTOS_SHP_TEMA_SAUDE&arqTipo=Shapefile")

# IMPORTAR BASES BRUTAS -------------------------------------------------------

file <- list_tmp("Dom*.*02b*.*csv")
populacao <- data.table::fread(file) %>% tibble::as_tibble()

file <- list_tmp("35b*.*shp")
setores <- sf::read_sf(file, options = "ENCODING=Latin1")

file <- list_tmp("SIRGAS*.*UBS*.*shp")
ubs_pontos <- sf::read_sf(file)

file <- list_tmp("DomicilioRenda_SP1*.*csv")
renda <- data.table::fread(file) %>% tibble::as_tibble()

# SALVAR DADOS RAW
save(setores, file = "inst/extdata/setores_censitarios-raw.rda")
save(populacao, file = "inst/extdata/populacao_sp_capital-raw.rda")
save(ubs_pontos, file = "inst/extdata/ubs_pontos-raw.rda")
save(renda, file = "inst/extdata/renda_sp_capital-raw.rda")

###############################################################################
# Os dados da malha de UBS foram obtidos por pedido de acesso à informação
# e os dados com os totais de enfermeiros e médicos nas unidades de saúde
# foram obtidos por meio de consulta manual. Estes dados foram subidos
# manualmente ao repositório do OSF.
###############################################################################
