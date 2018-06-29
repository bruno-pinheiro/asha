# Interseccao de pontos de origem com areas de saude ###########################

#' @title Interseccao de pontos de origem em areas de saude
#' @name asha_intersect
#'
#' @description A funcao pega os pontos de origem e realiza interseccao
#'              com poligonos que demarcam territorios de saude
#'
#' @param sf1 Um objeto sf com geometria de poligonos
#' @param sf2 Um objeto sf com geometria de pontos representando a origem da demanda
#' @param id1 Codigo de identificacao dos poligonos
#' @param id2 Codigo de identificao dos pontos
#'
#' @details A funcao identifica os pontos localizados dentro de cada poligono.
#'          Usa a funcao sf::st_join para mesclar as bases com
#'          sf::st_intersect para realizar a intersecao.
#'
#' @return Um data frame com \code{id1} (origem) e \code{id2} (area de cobertura) como
#'         colunas.
#'
#' @author Bruno Pinheiro
#'
#' @seealso \code{\link[sf]{st_intersects}},
#'          \code{\link[sf]{st_join}}
#'
#' @examples
#' data("ubs_sp_areas")
#' data("centroides_sp")
#' asha_intersect(ubs_sp_areas, centroides_sp, "cnes", "cd_geocodi")
#'
#' @import dplyr sf
#'
#' @export
asha_intersect <- function(sf1, sf2, id1, id2) {
  sf1 %>%
    select(id1) %>%
    st_join(select(sf2, id2), join = st_intersects) %>%
    as.data.frame %>%
    select(id1, id2)
}

# Funcao asha_nearest ###########################

#' @title Encontrar os n pontos mais proximos
#' @name asha_nearest
#'
#' @description A funcao busca e encontra o n pontos mais proximos
#'              entre dois datasets sf.
#'
#' @param sf1 Um objeto sf com geometria de pontos representando o destino
#' @param sf2 Um objeto sf com geometria de pontos representando a origem
#' @param id1 Codigo de identificacao do ponto de destino
#' @param id2 Codigo de identificao da ponto de origem
#' @param n Numero de pontos mais proximo
#'
#' @details A funcao relaciona dois conjuntos de pontos espaciais e identifica os \code{n}
#'          pontos de \code{sf1} mais proximos de \code{sf2}. Usa a funcao nabor::knn
#'          para construir a matriz de distancia, filtra os \code{n} pontos e atribui
#'          codigos de identificacao.
#'
#' @return Retorna um data frame com as colunas \code{id_1} (destino) e \code{id_2} (origem),
#'         alem de uma coluna para cada \code{n} distancia.
#'
#' @author Bruno Pinheiro
#'
#' @seealso \code{\link[nabor]{knn}}
#'
#' @examples
#' data("ubs_sp")
#' data("centroides_sp")
#' asha_nearest(ubs_sp, centroides_sp, "cnes", "cd_geocodi", 3)
#'
#' @import sf dplyr
#'
#' @export
asha_nearest <- function(sf1, sf2, id1, id2, n) {
  id1 <- rlang::sym(id1)
  id2 <- rlang::sym(id2)
  id_1 <- NULL
  id_2 <- NULL
  value <- NULL
  variable <- NULL
  . <- NULL

  df <- list(coords1 = st_coordinates(sf1), coords2 = st_coordinates(sf2))
  df <- nabor::knn(data = df[[1]], query = df[[2]], k = n)
  df <-
    data.frame(id_2 = pull(select(as.data.frame(sf2), !!id2)),
                   as.data.frame(df[[1]]),
                   as.data.frame(df[2])) %>%
    reshape2::melt(id.vars = c("id_2", colnames(.[(n + 2):ncol(.)]))) %>%
    rename(proximidade = variable, id_1 = value) %>%
    mutate(id_1 = pull(as.data.frame(sf1), !!id1)[id_1])
  return(df)
}
