#' @title Obter informacoes de distancia e tempo de viagem por modal
#' @name asha_dists
#'
#' @description Permite o levantamento de dados de rotas de viagem para
#'              modais de transporte entre dois conjuntos de pontos espaciais
#'
#' @param fluxo Uma matriz ou data frame de duas colunas representando a
#'              latitude e a longitude das origens.
#' @param zonas Uma matriz ou data frame de duas colunas representando a
#'              latitude e a longitude dos destinos.
#' @param modal String especificando o modo de transporte, que pode ser bicycling (padrao),
#'              walking, driving ou transit
#' @param api String com API do Google Distance Matrix API
#'
#' @details Converte o CRS do objeto passado em \code{zonas} em lat long (4326) para
#'          consultar a Google Matrix Distance API com funcao \code{\link[stplanr]{dist_google}}.
#'          A consulta pode ser feita para os modais caminhada, transporte publico,
#'          bicicleta e carro. Prepara automaticamente os objeto OD com
#'          \code{\link[stplanr]{od2odf}} e gera resultados para pares linha a linha,
#'          ao inves de todos para todos como o pacote stplanr, reduzindo o consumo da API.
#'
#' @return Retorna um data frame com colunas \code{de} (enderecos de origem),
#'         \code{para} (enderecos de destino), \code{distancias} (em metros),
#'         \code{tempo} (em segundos), \code{moeda} e \code{tarifa}
#'
#' @author Bruno Pinheiro
#'
#' @seealso \code{\link[stplanr]{dist_google}},
#'          \code{\link[stplanr]{od2odf}}
#'
#' @examples
#' \dontrun{
#' cent <- sf::st_centroid(setores)
#' x <- asha_nn(ubs_pontos, setores, "cnes", "cd_geocodi", 1)
#' zonas <- asha_zones(cent, ubs_pontos, "cd_geocodi", "cnes")
#' asha_dists(x[500:501, ], zonas, "transit")
#' }
#'
#' @export
asha_dists <- function(fluxo, zonas, modal = "walking", api) {
  from_addresses = NULL
  to_addresses = NULL
  distances = NULL
  duration = NULL
  currency = NULL
  fare = NULL
  zonas <- methods::as(sf::st_transform(zonas, 4326), "Spatial")
  od <- stplanr::od2odf(flow = fluxo, zones = zonas)
  uma_linha <- data.frame(from_addresses = NA, to_addresses = NA,
                          distances = NA, duration = NA,
                          currency = NA, fare = NA)
  output <- data.frame()
  if(missing(api)) {
    for (linha in 1:nrow(od)) {
      o <- od[linha, 3:4]
      d  <- od[linha, 5:6]
      output <- tryCatch(
        {
          rbind(output, stplanr::dist_google(from = o, to = d,
                                             mode = modal))
        },
        error = function(na) {
          message(na)
          output <- rbind(output, uma_linha)
        }
      )
    }
    return(output <- output %>%
             dplyr::rename(de = from_addresses, para = to_addresses,
                           distancias = distances, tempo = duration,
                           moeda = currency, tarifa = fare))
  } else if(modal == "walking" & missing(api)) {
    for (linha in 1:nrow(od)) {
      o <- od[linha, 3:4]
      d  <- od[linha, 5:6]
      output <- tryCatch(
        {
          rbind(output, stplanr::dist_google(from = o, to = d,
                                             mode = modal))
        },
        error = function(na) {
          message(na)
          output <- rbind(output, uma_linha)
        }
      )
    }
    return(output <-output %>%
             dplyr::rename(de = from_addresses, para = to_addresses,
                           distancias = distances, tempo = duration,
                           moeda = currency, tarifa = fare))
  } else if(modal == "walking") {
    for (linha in 1:nrow(od)) {
      o <- od[linha, 3:4]
      d  <- od[linha, 5:6]
      output <- tryCatch(
        {
          rbind(output, stplanr::dist_google(from = o, to = d,
                                             mode = modal, google_api = api))
        },
        error = function(na) {
          message(na)
          output <- rbind(output, uma_linha)
        }
      )
    }
    return(output <- output %>%
             dplyr::rename(de = from_addresses, para = to_addresses,
                           distancias = distances, tempo = duration,
                           moeda = currency, tarifa = fare))
  } else if(missing(modal) & missing(api)) {
    for (linha in 1:nrow(od)) {
      o <- od[linha, 3:4]
      d  <- od[linha, 5:6]
      output <- tryCatch(
        {
          rbind(output, stplanr::dist_google(from = o, to = d))
        },
        error = function(na) {
          message(na)
          output <- rbind(output, uma_linha)
        }
      )
    }
    return(output <-output %>%
             dplyr::rename(de = from_addresses, para = to_addresses,
                           distancias = distances, tempo = duration,
                           moeda = currency, tarifa = fare))
  } else {
    for (linha in 1:nrow(od)) {
      o <- od[linha, 3:4]
      d  <- od[linha, 5:6]
      output <- tryCatch(
        {
          rbind(output, stplanr::dist_google(from = o, to = d,
                                             mode = modal, google_api = api))
        },
        error = function(na) {
          message(na)
          output <- rbind(output, uma_linha)
        }
      )
    }
  }
  return(output <-
           output %>%
           dplyr::rename(de = from_addresses, para = to_addresses,
                         distancias = distances, tempo = duration,
                         moeda = currency, tarifa = fare))
}
