#' @title Grafico de barras para uma variavel categorica
#' @name asha_bar
#'
#' @description Uma funcao que pega uma variavel categorica, contabiliza suas classes
#'     e plota um grafico de barras formatado. O grafico sera plotado com coord_flip()
#'
#' @param df Um data.frame
#' @param x Uma variavel categorica
#' @param fill Uma variavel categorica
#'
#' @details A funcao usa o dplyr para calcular e o ggplot para plotar.
#'     O parametro \code{x} deve ser uma string e portanto precisar ser
#'     incluido entre aspas, conforme o exemplo.
#'
#' @return Um grafico de barras para a variavel \code{x} da base \code{df}.
#'
#' @author Bruno Pinheiro
#'
#' @seealso \code{\link[ggplot2]{geom_bar}},
#'          \code{\link[gridExtra]{grid.arrange}},
#'          \code{\link[scales]{percent}}
#'
#' @examples
#' data("setores_sp")
#' asha_bar(as.data.frame(setores_sp), "tipo")
#' # Com mais de uma variavel
#' library(dplyr)
#' setores_sp$pm <- as.factor(setores_sp$pessoas_sp > mean(setores_sp$pessoas_sp, na.rm = TRUE))
#' vars <- c("tipo", "pm")
#' plots <- lapply(vars, function(i) asha_bar(as.data.frame(setores_sp), i))
#'
#' @import dplyr ggplot2 sf
#'
#' @export
asha_bar <- function(df, x, fill = NULL) {
  x <- rlang::sym(x)
  prop <- NULL

  if(is.null(fill)) {
  maxProp <-
    df %>%
    filter(!is.na(!!x)) %>%
    count(!!x) %>%
    mutate(prop = prop.table(n)) %>%
    summarise(maxProp = max(prop) + .08) %>%
    as.numeric()

  df %>%
    as.data.frame() %>%
    filter(!is.na(!!x)) %>%
    count(!!x) %>%
    mutate(prop = prop.table(n)) %>%
    ggplot(aes(y = prop, x = !!x)) +
    geom_bar(stat = "identity") +
    scale_y_continuous(labels = scales::percent, limits = c(0, maxProp)) +
    geom_text(aes(label = scales::percent(round(prop, 3))), hjust=-.1) +
    labs(title=paste0("Distribuicao de proporcoes para ", colnames(select(df, !!x))),
         x = NULL, y = NULL) +
    theme_bw() +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank()) +
    coord_flip()
  } else {
    fill <- rlang::sym(fill)
    maxProp <-
      df %>%
      filter(!is.na(!!x)) %>%
      # group_by(!!fill) %>%
      count(!!x) %>%
      mutate(prop = prop.table(n)) %>%
      summarise(maxProp = max(prop) + .08) %>%
      as.numeric()

    df %>%
      as.data.frame() %>%
      filter(!is.na(!!x)) %>%
      group_by(!!fill) %>%
      count(!!x) %>%
      mutate(prop = prop.table(n)) %>%
      ggplot(aes(y = prop, x = !!x, fill = !!fill)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      geom_text(aes(label = scales::percent(round(prop, 3))),
                position = position_dodge(width = .9), hjust=-.1) +
      scale_y_continuous(labels = scales::percent, limits = c(0, maxProp)) +
      labs(title=paste0("Distribuicao de proporcoes para ", colnames(select(df, !!x))),
           x = NULL, y = NULL) +
      theme_bw() +
      theme(panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            legend.position = "top") +
      coord_flip()
  }
}

#' @title Histograma para uma variavel numerica
#' @name asha_hist
#'
#' @description Uma funcao que pega uma variavel numerica e plota
#'     um histograma. O grafico sera plotado ja formatado, com titulo
#'     chamando o nome da variavel.
#'
#' @param df Um data.frame
#' @param x Uma variavel numerica
#' @param fill Um variavel categorica
#'
#' @details A função usa o ggplot para plotar.
#'     O parametro \code{x} deve ser uma string e portanto precisar ser
#'     incluido entre aspas, conforme o exemplo.
#'
#' @return Um bonito histograma para a variavel \code{x} da base \code{df}.
#'
#' @author Bruno Pinheiro
#'
#' @seealso \code{\link[ggplot2]{geom_bar}}, \code{\link[gridExtra]{grid.arrange}}
#'
#' @examples
#' data("setores_sp")
#' asha_hist(as.data.frame(setores_sp), "pessoas_sp")
#'
#' @import ggplot2
#'
#' @export
asha_hist <- function(df, x, fill = NULL) {
  x <- rlang::sym(x)

  if(is.null(fill)) {
    ggplot2::ggplot(as.data.frame(df), aes(x = !!x)) +
      ggplot2::geom_histogram(col = "black",fill = "lightblue") +
      ggplot2::labs(title = paste0("Histograma de ", colnames(select(df, !!x))),
                    x = NULL) +
      ggplot2::theme_bw()
  } else {
    fill = rlang::sym(fill)
    ggplot2::ggplot(as.data.frame(df), aes(x = !!x)) +
      ggplot2::geom_histogram(aes(fill = !!fill),
                              col = "black", alpha = .5, lwd = .2,
                              position = position_identity()) +
      ggplot2::labs(title = paste0("Histograma de ", colnames(select(df, !!x))),
                    x = NULL) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "top")
    }
}

#' @title Mapa cloropletico
#' @name asha_map
#'
#' @description Um tema simples para plotar mapas.
#'
#' @param sf Um objeto espacial de classe \code{sf}
#' @param x Variavel categorica a ser usada em \code{group_by} e \code{fill}.
#'          Parametro opcional, por padrao assume NULL.
#' @param pal Nome de uma paleta Color Brewer. Parametro opcional, por padrao
#'            assume NULL: se \code{x} e dado, por padrao usa a paleta "Set2" e
#'            se \code{x} nao e dado, nao usa nenhuma paleta.
#'
#' @details O tema cria um template limpo para o mapa, sem texto de eixo
#'          grid, bordas etc.
#'
#' @return Um bonito histograma para a variavel \code{x} da base \code{df}.
#'
#' @author Bruno Pinheiro
#'
#' @seealso \code{\link[ggplot2]{theme}}
#'
#' @examples
#' data("ubs_sp")
#' asha_map(ubs_sp)
#'
#' @export
asha_map <- function(sf, x = NULL, pal = NULL) {
  if(is.null(x)) {
    sf %>%
      ggplot2::ggplot() +
      ggplot2::geom_sf() +
      theme_map()
  } else if(is.null(pal)) {
  x <- rlang::sym(x)
  sf %>%
    dplyr::group_by(!!x) %>%
    ggplot2::ggplot() +
    ggplot2::geom_sf(aes(fill = !!x), lwd = 0, col = NA) +
    ggplot2::scale_fill_brewer(palette = "Set2") +
    ggplot2::ggtitle(paste("Distribuicao de", colnames(select(sf, !!x)))) +
    theme_map()
  } else {
    x <- rlang::sym(x)
    sf %>%
      dplyr::group_by(!!x) %>%
      ggplot2::ggplot() +
      ggplot2::geom_sf(aes(fill = !!x), lwd = 0, col = NA) +
      ggplot2::scale_fill_brewer(palette = pal) +
      ggplot2::ggtitle(paste("Distribuicao de", colnames(select(sf, !!x)))) +
      theme_map()
  }
}

#' Tema para mapas
#' @description Um tema para mapas do ggplot2
#' @name theme_map
#'
#' @examples
#' library(ggplot2)
#' data("ubs_sp")
#'
#' # Map in geographic coordinates
#' ggplot(ubs_sp) +
#'     geom_sf() +
#'     theme_map()
#'
#' @export
theme_map <- function() {
  return(
    theme(panel.grid.major = element_line(colour = 'transparent'),
          panel.grid.minor = element_line(colour = 'transparent'),
          panel.background = element_rect(fill = 'transparent', colour = NA),
          plot.background = element_rect(fill = "transparent", colour = NA),
          plot.margin = unit(rep(0, 4), "lines"),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          strip.text = element_text(size = 8),
          legend.title = element_text(size = 8),
          legend.text = element_text(size = 6),
          legend.margin = margin(t=0,r=0,b=0,l=0, unit="mm"))
  )
}
