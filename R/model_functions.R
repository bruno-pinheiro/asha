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
    st_join(select(sf2, id2), join = st_intersects) %>%
    as.data.frame() %>%
    select(id2, id1)
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
#' data("ubs_sp")
#' data("centroides_sp")
#' asha_nn(ubs_sp, centroides_sp, "cnes", "cd_geocodi", 3)
#'
#' @import sf dplyr
#' @importFrom rlang :=
#'
#' @export
asha_nn <- function(sf1, sf2, id1, id2, n) {
  id1 <- rlang::sym(id1); id2 <- rlang::sym(id2)
  nn.dists.Var1 = NULL; nn.dists.Var2 = NULL
  nn.idx.value = NULL; nn.dists.value = NULL
  de = NULL; para = NULL; proximidade = NULL; distancia = NULL

  df <- list(coords1 = st_coordinates(sf1), coords2 = st_coordinates(sf2))
  df <- nabor::knn(data = df[[1]], query = df[[2]], k = n) %>%
    reshape2::melt()
  df <- as.data.frame(split(df, df$L1)) %>%
    mutate(!!id2 := pull(as.data.frame(sf2), !!id2)[nn.dists.Var1],
           !!id1 := pull(as.data.frame(sf1), !!id1)[nn.idx.value]) %>%
    rename(proximidade = nn.dists.Var2,
           distancia = nn.dists.value) %>%
    select(!!id2, !!id1, proximidade, distancia)
  return(df)
}

#' @title Obter informacões de distancia e tempo de viagem por modal
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
#' modelo_proximidade <- asha_nn(ubs_sp, centroides_sp, "cnes", "cd_geocodi", 1)
#' asha_dists(modelo_proximidade[251:255, ], zonas, "transit", api = api02)
#'
#' @export
asha_dists <- function(fluxo, zonas, modal = "walking", api) {
  from_addresses = NULL
  to_addresses = NULL
  distances = NULL
  duration = NULL
  currency = NULL
  fare = NULL
  zonas <- methods::as(st_transform(zonas, 4326), "Spatial")
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
    return(output <-
             output %>%
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
      return(output <-
               output %>%
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
        return(output <-
                 output %>%
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
            return(output <-
                     output %>%
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
#' zonas <- asha_zones(centroides_sp, ubs_sp, "cd_geocodi", "cnes")
#' str(zonas)
#'
#' @export
asha_zones <- function(sf1, sf2, id1, id2) {
  id = NULL
  tipo = NULL
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


# Funcao asha_ac ###########################

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
#' # asha_ac(df1, pop, area, model)
#'
#' @export
asha_ac <- function(df1, pop, area, model, n) {
  oportunidades <- NULL
  demanda <- NULL
  pop <- dplyr::enquo(pop)
  area <- dplyr::enquo(area)
  if(missing(model)) {
    df1 <-
      df1 %>%
      dplyr::group_by(!! area) %>%
      dplyr::mutate(demanda = sum(!! pop, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(ac = (oportunidades / demanda) * n)
    return(df1)
  } else if(missing(n)) {
    model <- dplyr::enquo(model)
    df1 <-
      df1 %>%
      dplyr::group_by(!! model, !! area) %>%
      dplyr::mutate(demanda = sum(!! pop, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(ac = (oportunidades / demanda))
    return(df1)
  } else {
    model <- dplyr::enquo(model)
    df1 <-
      df1 %>%
      dplyr::group_by(!! model, !! area) %>%
      dplyr::mutate(demanda = sum(!! pop, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(ac = (oportunidades / demanda) * n)
    return(df1)
  }
}

#' @title Indicador AV
#' @name asha_av
#'
#' @description Cria o indicador de acessibilidade viavel (AV)
#'
#' @param df Um dataframe contendo as variaveis necessarias para o calculo. Veja os detalhes.
#' @param id Variavel com o id das areas de saúde
#' @param segundos Variavel com o tempo de deslocamento em segundos
#' @param pop Variavel com o total de habitantes da
#'            menor unidade territorial
#' @param raio Variavel numerica indicando o tamanho do raio (em minutos)
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
#' # asha_av(df, id, tempo, pop, raio)
#'
#' @export
asha_av <- function(df, id, segundos, pop, raio) {
  av = NULL; av_prop = NULL; minutos = NULL
  id <- dplyr::enquo(id)
  pop <- dplyr::enquo(pop)
  segundos <- dplyr::enquo(segundos)

  df <- df %>%
    dplyr::mutate(minutos = !! segundos / 60, av = as.character(minutos <= raio),
                  av = dplyr::recode(av, "TRUE" = "Sim", "FALSE" = "Nao"))

  av_x <- df %>%
    dplyr::filter(!is.na(av)) %>%
    dplyr::group_by(!! id, av) %>%
    dplyr::summarise(av_prop = sum(!! pop, na.rm = TRUE)) %>%
    dplyr::mutate(av_prop = prop.table(av_prop)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(av == "Sim") %>%
    dplyr::select(-av)

  if(nrow(av_x) < nrow(df %>% dplyr::distinct(!! id))) {
    av_x <- av_x %>%
      rbind(data.frame(cnes = df[!(df[[quo_name(id)]] %in% av_x[[quo_name(id)]]), ] %>%
                     dplyr::pull(!! id) %>% unique(),
                   av_prop = 0))
    df <- df %>% dplyr::left_join(av_x, by = quo_name(id))
    return(df)
  } else {
    df <- df %>% dplyr::left_join(av_x, by = quo_name(id))
    return(df)
  }
}
