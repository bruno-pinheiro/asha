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
