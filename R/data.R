#' Setores censitarios da cidade de Sao Paulo
#'
#' O dataset foi estruturado a partir de dados do Censo 2010 disponibilidos
#' pelo IBGE (malha de setores censitarios do estado de Sao Paulo) e resultados
#' agregados por setor censitario para a cidade de Sao Paulo. Alem disso inclui
#' variaveis calcularas: o numero de habitantes, a area e densidade demografica.
#' As variaveis com sufixo _vig e _prox referem-se aos modelos vigente e de
#' proximidade, respectivamente.
#'
#' @format Um dataset classe sf com os poligonos dos setores censitarios,
#'     contendo 18953 linhas e 5 variaveis:
#' \describe{
#'   \item{cd_geocodi}{Codigo de identificacao do setor censitario}
#'   \item{tipo}{Indica se o setor e urbano ou rural}
#'   \item{cd_geocodd}{Codigo de identificacao do distrito}
#'   \item{nm_distrit}{Nome do distrito}
#'   \item{geometry}{Geometria dos polígonos}
#' }
"setores"

#' Número de habitantes por setor censitario
#'
#' Sao os dados da variavel V001 do arquivo Domicilio02_SP1.csv dos
#' resultados do universo do Censo 2010 (IBGE), agregados por setor
#' censitario.
#'
#' @format Um dataset classe \code{tibble()} contendo 18363 linhas
#' 2 variaveis:
#'
#' \describe{
#'   \item{cd_geocodi}{Codigo de identificacao do setor censitario}
#'   \item{habitantes}{Numero de habitantes em domicílios particulares
#'   e coletivos}
#' }
"populacao"

#' Localizacao das UBS da cidade de Sao Paulo
#'
#' Geolocalização das UBS, representadas por pontos.
#'
#' @format Um dataset classe sf com geometria de pontos,
#'     contendo 456 linhas e 2 variaveis:
#'
#' \describe{
#'   \item{cnes}{Codigo de identificacao da UBS}
#'   \item{geometry}{Geometria dos pontos}
#' }
"ubs_pontos"

#' Areas de cobertura das UBS da cidade de Sao Paulo
#'
#' O dataset foi obtido da Secretaria Municipal de Saude por meio
#' de pedido de acesso a informacao. Traz os polígnos dos territórios
#' das Unidades Basicas de Saude, identificadas pelo número CNES de cada
#' equipamento. Foram adicionadas variaveis com a quantidade de medicos
#' e enfermeiros que trabalham em cada unidade.
#'
#' @format Um data frame com 94765 linhas e 10 colunas
#' \describe{
#'   \item{cnes}{Codigo de identificacao da UBS}
#'   \item{nomeubs}{Nome da UBS}
#'   \item{sts}{Superintendencia de saude responsavel pela unidade}
#'   \item{crs}{Coordenadoria regional de saude responsavel pela unidade}
#'   \item{subpref}{Prefeitura regional onde a UBS esta localizada}
#'   \item{geometry}{Geometria dos polígonos}
#' }
"ubs_malha"

#' Profissionais de saude em Sao Paulo
#'
#' Dados obtidos no DATASUS, com o total de medicos e enfermeiros nas
#' unidades de saude do municipio de Sao Paulo.
#'
#' @format Um dataset classe \code{tibble} contendo 18363 linhas
#' 2 variaveis:
#' \describe{
#'   \item{cnes}{Codigo CNES da UBS}
#'   \item{enfermeiros}{Quantidade de enfermeiros}
#'   \item{medicos}{Quantidade de medicos}
#' }
"prof_saude"

#' Distacias e tempos de viagem a pe entre setores censitarios e UBS
#'
#' O dataset foi criado com a funcao \code{asha_dist} e contem dados obtidos
#' da à Google Distance Matrix API. Inclui variaveis de distancias e tempos de
#' viagem entre os centroides dos setores censitarios da cidade de Sao Paulo e
#' as 5 UBS mais proximas de cada um.
#'
#' @format Um data frame contendo 94765 linhas e 10 variaveis:
#' \describe{
#'   \item{cd_geocodi}{Codigo de identificacao do setor censitario}
#'   \item{cnes}{Codigo de identificacao da UBS}
#'   \item{ox}{Latitude da origem}
#'   \item{oy}{Longitude da origem}
#'   \item{dx}{Latitude do destino}
#'   \item{dy}{Longitude do destino}
#'   \item{de}{Endereco de origem}
#'   \item{para}{Endereco de destino}
#'   \item{distancias}{Distancia de caminhada em metros}
#'   \item{tempo}{Tempo de caminhada em segundos}
#' }
"od_viagens"

#' Setores censitarios da cidade de Sao Paulo
#'
#' O dataset foi estruturado a partir de dados do Censo 2010 disponibilidos
#' pelo IBGE (malha de setores censitarios do estado de Sao Paulo) e resultados
#' agregados por setor censitario para a cidade de Sao Paulo.
#'
#' @format Um dataset classe sf com os poligonos dos setores censitarios
#'         contendo 37906 linhas e 15 variaveis:
#' \describe{
#'   \item{cd_geocodi}{Codigo de identificacao do setor censitario}
#'   \item{cnes}{Codigo CNES da UBS}
#'   \item{malha}{Indica se a observacao se refere a malha vigente ou ao experimento}
#'   \item{habitantes}{Numero de pessoas residentes nos setores censitarios}
#'   \item{enfermeiros}{Numero de enfermeiros nas UBS}
#'   \item{medicos}{Numero de medicos nas UBS}
#'   \item{oportunidades}{Indicador de Oportunidades -- medicos + enfermeiros}
#'   \item{demanda}{Indicador de Demanda das UBS}
#'   \item{distancia}{Distancia em metros do percurso a pe entre o setor
#'   e a UBS}
#'   \item{tempo}{Tempo de viagem do percurso a pe entre o setor censitario
#'   e a UBS em segundos}
#'   \item{minutos}{Tempo de viagem do percurso a pe entre o setor censitario
#'   e a UBS em minutos}
#'   \item{av}{Indica se o setor esta ou nao no raio de acessibilidade viavel da UBS}
#'   \item{av_prop_}{Proporcao de habitantes residentes no raio de
#'   acessibilidade viavel por UBS}
#'   \item{ac}{Indicador de Acessibilidade Competitiva}
#'   \item{geometry}{Geometria dos polígonos}
#' }
"od_indicadores"
