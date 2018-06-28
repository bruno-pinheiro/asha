#' @importFrom tibble tibble
NULL

#' Poligonos de setores censitarios da cidade de Sao Paulo
#'
#' O dataset foi estruturado a partir de dados do Censo 2010 disponibilidos
#' pelo IBGE (malha de setores censitarios do estado de Sao Paulo) e resultados
#' agregados por setor censitario para a cidade de Sao Paulo. Alem disso inclui
#' variaveis calcularas: o numero de habitantes, a area e densidade demografica.
#'
#' @format Um dataset classe sf com os poligonos dos setores censitarios,
#'     contendo 18953 linhas e 6 variaveis:
#' \describe{
#'   \item{cd_geocodi}{Codigo de identificacao do setor censitario}
#'   \item{cd_geocodd}{Codigo de identificacao do distrito}
#'   \item{nm_distrit}{Nome do distrito}
#'   \item{tipo}{Indica se o setor e urbano ou rural}
#'   \item{pessoas_setor}{Numero de pessoas residentes nos setores censitarios}
#'   \item{area}{Area do setor censitario em km^2^}
#'   \item{dens_demografica}{Densidade demografica do setor censitario}
#'   \item{geometry}{Geometria dos polígonos}
#' }
"setores_sp"

#' Poligonos das areas de cobertura das UBS da cidade de Sao Paulo
#'
#' O dataset foi obtido da Secretaria Municipal de Saude por meio
#' de pedido de acesso a informacao. Traz os polígnos dos territórios
#' das Unidades Basicas de Saude, identificadas pelo número CNES de cada
#' equipamento. Foram adicionadas variaveis com a quantidade de medicos
#' e enfermeiros que trabalham em cada unidade.
#'
#' @format Um dataset classe sf com os poligonos dos setores censitarios,
#'     contendo 456 linhas e 8 variaveis:
#' \describe{
#'   \item{cnes}{Codigo de identificacao da UBS}
#'   \item{nomeubs}{Nome da UBS}
#'   \item{sts}{Superintendencia de saude responsavel pela unidade}
#'   \item{crs}{Coordenadoria regional de saude responsavel pela unidade}
#'   \item{subpref}{Prefeitura regional onde a UBS esta localizada}
#'   \item{total_med}{Quantidade de medicos da UBS}
#'   \item{total_enf}{Quantidade de enfermeiros da UBS}
#'   \item{geometry}{Geometria dos polígonos}
#' }
"areas_ubs"
