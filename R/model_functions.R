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

# Funcao asha_nn ###########################

#' @title Encontrar os n pontos mais proximos
#' @name asha_nn
#'
#' @description A funcao busca e encontra o n pontos mais proximos
#'              entre dois datasets sf.
#'
#' @param sf1 Um objeto sf com geometria de pontos representando o destino
#' @param sf2 Um objeto sf com geometria de pontos representando a origem
#' @param id1 Codigo de identificacao do ponto de destino
#' @param id2 Codigo de identificao do ponto de origem
#' @param n Numero de pontos mais proximo
#'
#' @details A funcao relaciona dois conjuntos de pontos espaciais e identifica os \code{n}
#'          pontos de \code{sf1} mais proximos de \code{sf2}. Usa a funcao nabor::knn
#'          para construir a matriz de distancia, filtra os \code{n} pontos e atribui os
#'          codigos de identificacao do destino e da origem.
#'
#' @return Retorna um data frame com as colunas \code{de} (origem) e \code{para} (destino),
#'         \code{proximidade} (distancia $n_i$) e \code{distancia}.
#'
#' @author Bruno Pinheiro
#'
#' @seealso \code{\link[nabor]{knn}}
#'
#' @examples
#' data("ubs_sp")
#' data("centroides_sp")
#' asha_nn(ubs_sp, centroides_sp, "cnes", "cd_geocodi", 3)
#'
#' @import sf dplyr
#'
#' @export
asha_nn <- function(sf1, sf2, id1, id2, n) {
  id1 <- rlang::sym(id1)
  id2 <- rlang::sym(id2)
  nn.dists.Var1 <- NULL
  nn.dists.Var2 <- NULL
  nn.idx.value <- NULL
  nn.dists.value <- NULL
  de <- NULL
  para <- NULL
  proximidade <- NULL
  distancia <- NULL


  df <- list(coords1 = st_coordinates(sf1), coords2 = st_coordinates(sf2))
  df <- nabor::knn(data = df[[1]], query = df[[2]], k = n)
  df <- reshape2::melt(df)
  df <-
    as.data.frame(split(df, df$L1)) %>%
    rename(de = nn.dists.Var1,
           para = nn.idx.value,
           proximidade = nn.dists.Var2,
           distancia = nn.dists.value) %>%
    mutate(de = pull(as.data.frame(sf2), !!id2)[de],
           para = pull(as.data.frame(sf1), !!id1)[para]) %>%
    select(de, para, proximidade, distancia)
  return(df)
}
