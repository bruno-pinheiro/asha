#' @title Get tabular data from IBGE
#' @name get_zip
#'
#' @description Download and extract tabular data from ftp://ftp.ibge.gov.br/
#'
#' @param url a string defining url of zipped shapfile to get
#' @param savedir a string defining the directory to save the data. It's not
#'        mandatory. See details.
#' @param junkpaths \code{unzip} TRUE/FALSE option to extract all files
#'        directily and ignore folders. It's usefull for IBGE data, which
#'        ussually have many junkpaths.
#'
#' @details \code{get_zip} will download and extract files
#'
#' @return extrated files
#'
#' @author Bruno Pinheiro
#'
#' @examples
#' # get_zip()
#'
#' @export
get_zip <- function(url, savedir = NULL, junkpaths = TRUE) {
  # baixar arquivo
  if (is.null(savedir)){
    if (!file.exists(tempdir())) { dir.create(tempdir()) }
    tmp <- tempfile(fileext = ".zip")
    utils::download.file(url, destfile = tmp)
    utils::unzip(tmp, exdir = tempdir(), junkpaths = junkpaths)
  } else if (is.character(savedir)){
    tmp <- tempfile(fileext = ".zip")
    utils::download.file(url, destfile = tmp)
    utils::unzip(tmp, exdir = savedir, junkpaths = junkpaths)
  } else {
    message("savedir must be a character vector indicating some folder path")
  }
}

#' @title Get tabular data from IBGE Census 2010
#' @name get_censo
#'
#' @description Download and extract census data from IBGE
#'
#' @param state a string defining the the federation unitie to download data
#' @param datatype a string defining the kind of data to download: "tabular"
#'        (default) or "geo"
#' @param mesh a string defining the mesh level to download: "setores"
#'        (default) or "municipios"
#' @inheritParams get_zip
#'
#' @details \code{get_censo} will download and extract IBGE data, keeping files
#'          in directory informed on \code{savedir} argument. If save dir is not
#'          informed, will store them in the temporary folder
#'
#' @return IBGE census tabular or geo data
#'
#' @author Bruno Pinheiro
#'
#' @examples
#' get_censo("rr")
#' clean_tmp()
#'
#' @export
get_censo <- function(state,
                      datatype = "tabular", mesh = "setores",
                      savedir = NULL){

  if (datatype == "tabular"){

    # construir URL para dados tabulares
    base_url <- "ftp://ftp.ibge.gov.br/Censos"
    diryear <- paste0("Censo_Demografico_2010")
    dirdata <- file.path("Resultados_do_Universo/Agregados_por_Setores_Censitarios")
    url <- file.path(base_url, diryear, dirdata) # precisa melhorar pelo /
    state <- toupper(state)

  } else if (datatype == "geo") {

    # construir URL para dados geo (meshs)
    base_url <- "ftp://geoftp.ibge.gov.br/organizacao_do_territorio/malhas_territoriais"
    dirmeshs <- paste0("malhas_de_setores_censitarios__divisoes_intramunicipais")
    diryear <- paste0("censo_2010")
    dirshp <- "setores_censitarios_shp"
    url <- file.path(base_url, dirmeshs, diryear, dirshp, tolower(state))
    state <- tolower(state)

  } else {
    message("Just 'tabular' or 'geo' are accepted as dataype arguments")
  }

  # raspar nomes dos arquivos na página
  all_files <- RCurl::getURL(paste0(url, "/"), dirlistonly = TRUE)
  all_files <- unlist(strsplit(all_files, "\n"))

  if (datatype == "tabular"){
    if (tolower(state) == "spcapital") {
      state <- "SP_Capital_"
    } else if(tolower(state) == "sp") {
      state <- "SP_Exceto_a_Capital_"
    }
  }

  # definir url do arquivo para baixar
  if (datatype == "tabular"){
    down_file <- file.path(url, all_files[grep(paste0("^", state), all_files)])
  } else if (datatype == "geo") {
    down_file <- file.path(url, all_files[grep(paste0("^", state, "_", mesh), all_files)])
  }

  get_zip(url = down_file, savedir = savedir)
}
