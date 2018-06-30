# Funcao asha_intersect ###########################

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
    select(id2, id1)
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
#' @return Retorna um data frame com as colunas \code{id2} (codigo de origem),
#'         \code{id1} (codigo de destino), \code{proximidade} e \code{distancia}.
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
  # id1 <- rlang::sym(id1)
  # id2 <- rlang::sym(id2)
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
    rename(!!id2 := nn.dists.Var1,
           !!id1 := nn.idx.value,
           proximidade = nn.dists.Var2,
           distancia = nn.dists.value) %>%
    mutate(!!id2 := pull(as.data.frame(sf2), !!id2)[!!id2],
           !!id1 := pull(as.data.frame(sf1), !!id1)[!!id1]) %>%
    select(!!id2, !!id1, proximidade, distancia)
  return(df)
}

# Funcao asha_dists ###########################

#' @title Encontrar os n pontos mais proximos
#' @name asha_dists
#'
#' @description A funcao busca e encontra o n pontos mais proximos
#'              entre dois datasets sf.
#'
#' @param fluxo Uma matriz ou data frame de duas colunas representando a
#'              latitude e a longitude das origens.
#' @param zonas Uma matriz ou data frame de duas colunas representando a
#'              latitude e a longitude dos destinos.
#' @param api API do Google Distance Matrix API
#'
#' @details A funcao relaciona dois conjuntos de pontos espaciais e identifica os \code{n}
#'          pontos de \code{sf1} mais proximos de \code{sf2}. Usa a funcao nabor::knn
#'          para construir a matriz de distancia, filtra os \code{n} pontos e atribui os
#'          codigos de identificacao do destino e da origem.
#'
#' @return Retorna um data frame com as colunas \code{id2} (codigo de origem),
#'         \code{id1} (codigo de destino), \code{proximidade} e \code{distancia}.
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
#' @import sf stplanr
#' @importFrom methods as
#'
#' @export
asha_dists <- function(fluxo, zonas, api) {
  zonas <- as(st_transform(zonas, 4326), "Spatial")

  od <- stplanr::od2odf(flow = fluxo, zones = zonas)
  uma_linha <- data.frame(from_addresses=NA, to_addresses=NA, distances=NA,
                      duration=NA, currency=NA, fare=NA)
  output <- data.frame()

  for (linha in 1:nrow(od)) {
    o <- od[linha, 3:4]
    d  <- od[linha, 5:6]
    output <- tryCatch(
      {
        rbind(output, stplanr::dist_google(from = o, to = d,
                                             mode = 'walking', google_api = api))
        },
      error = function(na) {
        message("Erro: No results for this request (e.g. due to lack of support for this mode between the from and to locations)")
        message(na)
        output <- rbind(output, uma_linha)
      }
    )
  }
  return(output)
}



