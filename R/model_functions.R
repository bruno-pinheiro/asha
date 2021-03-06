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
#' \dontrun{
#' data("ubs_malha")
#' data("setores")
#' asha_intersect(ubs_malha,
#'                sf::st_centroid(setores),
#'                "cnes", "cd_geocodi")
#' }
#' @importFrom magrittr "%>%"
#'
#' @export
asha_intersect <- function(sf1, sf2, id1, id2) {
  sf1 %>%
    sf::st_join(dplyr::select(sf2, id2), join = sf::st_intersects) %>%
    as.data.frame() %>%
    dplyr::select(id2, id1)
}


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
#' \dontrun{
#' data("ubs_pontos")
#' data("setores")
#' cent <- sf::st_centroid(setores)
#' asha_nn(ubs_pontos, cent, "cnes", "cd_geocodi", 3)
#' }
#'
#' @importFrom rlang :=
#' @importFrom magrittr "%>%"
#'
#' @export
asha_nn <- function(sf1, sf2, id1, id2, n) {
  id1 <- rlang::sym(id1); id2 <- rlang::sym(id2)
  nn.dists.Var1 = NULL; nn.dists.Var2 = NULL
  nn.idx.value = NULL; nn.dists.value = NULL
  de = NULL; para = NULL; proximidade = NULL; distancia = NULL

  df <- list(coords1 = sf::st_coordinates(sf1), coords2 = sf::st_coordinates(sf2))
  df <- nabor::knn(data = df[[1]], query = df[[2]], k = n) %>% reshape2::melt()
  df <- as.data.frame(split(df, df$L1)) %>%
    dplyr::mutate(!!id2 := dplyr::pull(as.data.frame(sf2), !!id2)[nn.dists.Var1],
                  !!id1 := dplyr::pull(as.data.frame(sf1), !!id1)[nn.idx.value]) %>%
    dplyr::rename(proximidade = nn.dists.Var2, distancia = nn.dists.value) %>%
    dplyr::select(!!id2, !!id1, proximidade, distancia) %>%
    tibble::as_tibble()
  return(df)
}


#' @title Estrutura a base de zonas necessaria para usar \code{asha_dists}
#' @name asha_zones
#'
#' @description Mescla facilmente dois objetos sf em um
#'
#' @param sf1 Um objeto sf com coluna de id (por exemplo pontos de origem)
#' @param sf2 Um objeto sf com coluna de id (por exemplo pontos de destino)
#' @param id1 String especificando a variavel de id do objeto sf1
#' @param id2 String especificando a variavel de id do objeto sf2
#'
#' @details A funcao pega dois objetos espaciais de classe sf, mescla e
#'          organiza seus codigos id em uma unica coluna. E importante
#'          que os dois objetos tenham o mesmo crs definido previamente.
#'
#' @return Retorna um objeto sf com a coluna \code{id}, contendo os codigos de
#'         identificacao de sf1 e sf2, e a coluna \code{tipo}, referenciando os
#'         ids a partir dos nomes das variaves \code{id1} e \code{id2} passadas
#'         na funcao.
#'
#' @author Bruno Pinheiro
#'
#' @examples
#' \dontrun{
#' zonas <- asha_zones(sf::st_centroid(setores), ubs_pontos, "cd_geocodi", "cnes")
#' str(zonas)
#' }
#'
#' @export
asha_zones <- function(sf1, sf2, id1, id2) {
  id = NULL; tipo = NULL
  rbind(
    sf1 %>%
      dplyr::select(!!id1) %>%
      dplyr::rename(id = !!id1) %>%
      dplyr::mutate(tipo = !!id1),
    sf2 %>%
      dplyr::select(!!id2) %>%
      dplyr::rename(id = !!id2) %>%
      dplyr::mutate(tipo = !!id2)
  )
}


#' @title Indicador AC
#' @name asha_ac
#'
#' @description Cria o indicador de acessibilidade competitiva
#'
#' @param df1 Um dataframe contendo as variaveis necessarias para o calculo. Veja os detalhes.
#' @param pop Variavel com o total de habitantes da menor unidade territorial
#' @param area Variavel de id da area de cobertura, para a qual o indicador sera calculado
#' @param model Variavel opcional para a construcao do indicador para modelos diferentes
#' @param n Grandeza da taxa (100, 1000, 10000 habitantes...)
#'
#' @details Alem das variaveis com numero de habitantes e id de area, que sao passados
#'          na funcao, o dataframe deve conter a variavel \code{oportunidades}, que
#'          sera automaticamente usada no calculo do indicador AC. Se o calculo for para
#'          modelos diferentes, o data.frame devera ter uma variavel chamada modelo, com
#'          os respectivos valores indicativos. Neste caso a funcao calcula o indicador da
#'          area em cada modelo.
#' @return Retorna o dataframe indicado no argumento \code{df1} acrescidas as colunas demanda
#'         (que contabiliza a demanda total da \code{area}) e \code{ac}, com o indicador de
#'         de acessibilidade competitiva, por area.
#'
#' @author Bruno Pinheiro
#'
#' @examples
#' \dontrun{
#' asha_ac(df1, pop, area, model)
#' }
#'
#' @export
asha_ac <- function(df1, pop, area, model, n = 1) {
  oportunidades = NULL; demanda = NULL
  pop <- dplyr::enquo(pop)
  area <- dplyr::enquo(area)
  if(missing(model)) {
    df1 %>%
      dplyr::group_by(!! area) %>%
      dplyr::mutate(demanda = sum(!! pop, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(ac = (oportunidades / demanda) * n)
  } else {
    model <- dplyr::enquo(model)
    df1 %>%
      dplyr::group_by(!! model, !! area) %>%
      dplyr::mutate(demanda = sum(!! pop, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(ac = (oportunidades / demanda) * n)
  }
}


#' @title Indicador AV
#' @name asha_av
#'
#' @description Cria o indicador de acessibilidade viavel (AV)
#'
#' @param df Um dataframe contendo as variaveis necessarias para o calculo. Veja os detalhes.
#' @param id Variavel com o id das areas de saude
#' @param segundos Variavel com o tempo de deslocamento em segundos
#' @param pop Variavel com o total de habitantes da
#'            menor unidade territorial
#' @param raio Variavel numerica indicando o tamanho do raio (em minutos)
#' @param model Modelo, ou malha territoral
#'
#' @details Utiliza as variaveis passadas na funcao para calcular o indicador de acessibilidade
#'          viavel, incluindo a criacao das variaveis de acessibilidade.
#' @return A funcao adiciona 3 colunas: \code{minutos} (com os minutos de deslocamento),
#'         \code{av} (variavel binaria indicando se o setor esta no raio de acessibilidade viavel
#'         ou nao) e \code{av_prop} (que indica a proporcao da populacao no raio de acesso viavel
#'         em cada UBS.
#'
#' @author Bruno Pinheiro
#'
#' @examples
#' \dontrun{
#' asha_av(df, id, tempo, pop, raio)
#' }
#'
#' @export
asha_av <- function(df, id, segundos, pop, model, raio) {
  av = NULL; av_prop = NULL; minutos = NULL
  id <- dplyr::enquo(id)
  pop <- dplyr::enquo(pop)
  segundos <- dplyr::enquo(segundos)

  df <- df %>%
    dplyr::mutate(minutos = !! segundos / 60,
                  av = dplyr::if_else(minutos <= raio, "Sim", "Nao"))

  if (missing(model)){
    av_x <- df %>%
      dplyr::filter(!is.na(av)) %>%
      dplyr::group_by(!! id, av) %>%
      dplyr::summarise(av_prop = sum(!! pop, na.rm = TRUE)) %>%
      dplyr::mutate(av_prop = prop.table(av_prop)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(av == "Sim") %>%
      dplyr::select(-av)
  } else {
    av_x <- df %>%
      dplyr::filter(!is.na(av)) %>%
      dplyr::group_by(!! id, av, !! dplyr::enquo(model)) %>%
      dplyr::summarise(av_prop = sum(!! pop, na.rm = TRUE)) %>%
      dplyr::mutate(av_prop = prop.table(av_prop)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(av == "Sim") %>%
      dplyr::select(-av)
  }

  if(nrow(av_x) < nrow(df %>% dplyr::distinct(!! id))) {
    av_x <- av_x %>%
      rbind(tibble::tibble(
        cnes = df[!(df[[rlang::quo_name(id)]] %in% av_x[[rlang::quo_name(id)]]), ] %>%
          dplyr::pull(!! id) %>% unique(),
        av_prop = 0))
  }

  if (missing(model)){
    df <- df %>% dplyr::left_join(av_x, by = c(rlang::quo_name(id)))
    return(df)
  } else {
    df <- df %>% dplyr::left_join(av_x, by = c(rlang::quo_name(id), rlang::quo_name(dplyr::enquo(model))))
    return(df)
  }
}


#' @title Calcular indicadores agregados
#' @name asha_ind_group
#'
#' @description Calcula população agregada por categorias
#'
#' @param df Um dataframe contendo as variaveis necessarias para o calculo. Veja os detalhes.
#' @param ind Variavel categórica com o indicador
#' @param pop Variavel populacional
#' @param group Variável para agregar, além do indicador (opcional)
#' @param na.rm Útil caso a variável passada em \code{ind} contenha NA (default is FALSE)
#'
#' @details Descrever
#'
#' @return Descrever
#'
#' @author Bruno Pinheiro
#'
#' @examples
#' \dontrun{
#' asha_ind_group(od_indicadores, ac_cat, habitantes, malha, na.rm = TRUE)
#' }
#'
#' @export
asha_ind_group <- function(df, ind, pop, group, na.rm = FALSE){
  ind <- rlang::enquo(ind)
  pop <- rlang::enquo(pop)

  if (na.rm == TRUE){
    df <- df %>% dplyr::filter(!is.na(!! ind))
  }

  if (missing(group)) {

    df <- df %>%
      dplyr::group_by(!! ind) %>%
      dplyr::summarise(pop = sum(!! pop, na.rm = TRUE)) %>%
      dplyr::mutate(prop = prop.table(pop)) %>%
      dplyr::ungroup()

  } else {
    group <- rlang::enquo(group)
    df <- df %>%
      dplyr::group_by(!! group, !! ind) %>%
      dplyr::summarise(pop = sum(!! pop, na.rm = TRUE)) %>%
      dplyr::mutate(prop = prop.table(pop)) %>%
      dplyr::ungroup()
  }
  return(df)
}
