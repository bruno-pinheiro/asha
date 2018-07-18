# Barplot uma variavel categorica ###########################

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

# Histograma univariado ###########################

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
    ggplot(as.data.frame(df), aes(x=!!x)) +
      geom_histogram(col="black",fill="lightblue") +
      labs(title = paste0("Histograma de ", colnames(select(df, !!x))),
           x = NULL) +
    theme_bw()
  } else {
    fill = rlang::sym(fill)
    ggplot(as.data.frame(df), aes(x=!!x)) +
      geom_histogram(aes(fill = !!fill),
                     col="black", alpha = .5, lwd = .2,
                     position = position_identity()) +
      labs(title = paste0("Histograma de ", colnames(select(df, !!x))),
           x = NULL) +
      theme_bw() +
      theme(legend.position = "top")
    }
}
