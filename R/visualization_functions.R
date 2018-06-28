# Barplot uma variavel categorica ###########################

#' @title Grafico de barras para uma variavel categorica
#' @name barUniCat
#'
#' @description Uma funcao que pega uma variavel categorica, contabiliza suas classes
#'     e plota um grafico de barras formatado. O grafico sera plotado com coord_flip()
#'
#' @param df Um data.frame
#' @param x Uma variavel categorica
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
#' barUniCat(as.data.frame(setores_sp), "tipo")
#' # Com mais de uma variavel
#' library(dplyr)
#' setores_sp$pm <- as.factor(setores_sp$pessoas_setor > mean(setores_sp$pessoas_setor, na.rm = TRUE))
#' vars <- c("tipo", "pm")
#' plots <- lapply(vars, function(i) barUniCat(as.data.frame(setores_sp), i))
#'
#' @import dplyr ggplot2 sf
#'
#' @importFrom rlang sym
#'
#' @importFrom scales percent
#'
#' @export
barUniCat <- function(df, x) {
  x <- rlang::sym(x)
  prop <- NULL

  maxProp <-
    df %>%
    filter(!is.na(!!x)) %>%
    count(!!x) %>%
    mutate(prop = prop.table(n)) %>%
    summarise(maxProp = max(prop) + .08) %>%
    as.numeric()

  df %>%
    filter(!is.na(!!x)) %>%
    count(!!x) %>%
    mutate(prop = prop.table(n)) %>%
    ggplot(aes(y=prop, x=!!x)) +
    geom_bar(stat = "identity") +
    scale_y_continuous(labels = scales::percent, limits = c(0, maxProp)) +
    geom_text(aes(label = percent(round(prop, 3))), hjust=-.1) +
    labs(title=paste0("Distribuicao de proporcoes para ", colnames(select(df, !!x))),
         x = NULL, y = NULL) +
    theme_bw() +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank()) +
    coord_flip()
}

# Histograma univariado ###########################

#' @title Histograma para uma variavel numerica
#' @name histUni
#'
#' @description Uma funcao que pega uma variavel numerica e plota
#'     um histograma. O grafico sera plotado ja formatado, com titulo
#'     chamando o nome da variavel.
#'
#' @param df Um data.frame
#' @param x Uma variavel numerica
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
#' histUni(as.data.frame(setores_sp), "pessoas_setor")
#'
#' @import dplyr ggplot2 sf
#'
#' @importFrom rlang sym
#'
#' @export
histUni <- function(df, x) {
  x <- rlang::sym(x)

  ggplot(df, aes(x=!!x)) +
    geom_histogram(col="black",fill="lightblue") +
    labs(title = paste0("Histograma de ", colnames(select(df, !!x))),
         x = NULL) +
    theme_bw()
}
