# BAIXAR BASES BRUTAS -----------------------------------------------------

## Dados do Censo de 2010 agregados por setor censitario ---------------

#' FIXME Tem algo errado com a URL de download abaixo

if (!file.exists("inst/extdata/sp_resultados_universo")) {
  tmp <- tempfile(fileext = ".zip")
  download.file("ftp://ftp.ibge.gov.br/Censos/Censo_Demografico_2010/Resultados_do_Universo/Agregados_por_Setores_Censitarios/SP_sp_20180416.zip", tmp, quiet = TRUE)
  unzip(tmp, exdir = "inst/extdata/sp_resultados_universo")
  unlink(tmp)
}

#' Antes de continuar eu movi manualmente os arquivos Domicilio02_SP1.csv
#' e DomicilioRenda_SP1.csv para o diretorio data-raw/sp_resultados_universo/

## Shape de setores censitários do estado de Sao Paulo ---------------
if (!file.exists("inst/extdata/sp_setores_censitarios")) {
  tmp <- tempfile(fileext = ".zip")
  download.file("ftp://geoftp.ibge.gov.br/organizacao_do_territorio/malhas_territoriais/malhas_de_setores_censitarios__divisoes_intramunicipais/censo_2010/setores_censitarios_shp/sp/sp_setores_censitarios.zip", tmp, quiet = TRUE)
  unzip(tmp, exdir = "inst/extdata/sp_setores_censitarios")
  unlink(tmp)
}

## Quantidade de enfermeiros nas UBS ---------------
if (!file.exists(system.file("extdata", "enfermeiros.csv", package = "asha"))) {
  download.file("https://raw.githubusercontent.com/bruno-pinheiro/Mobilidade_Desigualdades/master/data/data_raw/quant_enfermeiros_set_2017.csv",
                "inst/extdata/enfermeiros.csv")
}

## Quantidade de medicos nas UBS ---------------
if(!file.exists(system.file("extdata", "medicos.csv", package = "asha"))) {
  download.file("https://raw.githubusercontent.com/bruno-pinheiro/Mobilidade_Desigualdades/master/data/data_raw/quant_medicos_set_2017.csv",
                "inst/extdata/medicos.csv")
}

## Base com a malha de areas de UBS ---------------
if (!file.exists(system.file("extdata", "ubs_sp_areas/", package = "asha"))) {
  tmp <- tempfile(fileext = ".zip")
  download.file('https://dataverse.harvard.edu/api/access/datafile/3092976', tmp)
  unzip(tmp, exdir = "inst/extdata/ubs_sp_areas")
  unlink(tmp)
}

## Base com os pontos de UBS ---------------
if (!file.exists(system.file("extdata", "ubs_sp/", package = "asha"))) {
  tmp <- tempfile(fileext = ".zip")
  download.file("http://geosampa.prefeitura.sp.gov.br/PaginasPublicas/downloadArquivoOL.aspx?orig=DownloadCamadas&arq=03_Equipamentos%5C%5CSa%FAde%5C%5CShapefile%5C%5CEQUIPAMENTOS_SHP_TEMA_SAUDE&arqTipo=Shapefile",
                tmp)
  unzip(tmp, exdir = "inst/extdata/ubs_sp")
  unlink(tmp)
  system("cp inst/extdata/ubs_sp/EQUIPAMENTOS_SHP_TEMA_SAUDE/SIRGAS_SHP_TEMA_-_SAUDE_UBS-POSTO_DE_SAUDE-CENTRO_DE_SAUDE.* inst/extdata/ubs_sp")
}

#' Aqui acabou a etapa de download dos dados. Agora e preciso rodar o script
#' "preparar_dados_brutos.R", que vai preparar as bases de dados para
#' manipulacao e geracao de indicadores e salva-los em data-raw/
