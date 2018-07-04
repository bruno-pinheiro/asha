#' Setores censitarios da cidade de Sao Paulo
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

#' Centroides dos setores censitarios da cidade de Sao Paulo
#'
#' Criado com a funcao st_centroid a partir do dataset \link{setores_sp}.
#'
#' @format Um dataset classe sf com os centroides dos setores censitarios,
#'     contendo 18953 linhas e 1 variavel
#' \describe{
#'   \item{cd_geocodi}{Codigo de identificacao do setor censitario}
#' }
"centroides_sp"

#' Localizacao das UBS da cidade de Sao Paulo
#'
#' Traz as mesmas variaveis da base ubs_sp_areas, porem para os pontos de
#' localizacao das UBS e nao os poligonos das respectivas areas. As UBS sao
#' identificadas pelo seu numero CNES. Foram adicionadas variaveis com a
#' quantidade de medicos e enfermeiros que trabalham em cada unidade.
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
"ubs_sp"

#' Areas de cobertura das UBS da cidade de Sao Paulo
#'
#' O dataset foi obtido da Secretaria Municipal de Saude por meio
#' de pedido de acesso a informacao. Traz os polígnos dos territórios
#' das Unidades Basicas de Saude, identificadas pelo número CNES de cada
#' equipamento. Foram adicionadas variaveis com a quantidade de medicos
#' e enfermeiros que trabalham em cada unidade.
#'
#' @format Um data frame com 94765 linhas e 10 colunas
#'
#' \describe{
#'   \item{cnes}{Codigo de identificacao da UBS}
#'   \item{nomeubs}{Nome da UBS}
#'   \item{sts}{Superintendencia de saude responsavel pela unidade}
#'   \item{crs}{Coordenadoria regional de saude responsavel pela unidade}
#'   \item{subpref}{Prefeitura regional onde a UBS esta localizada}
#'   \item{geometry}{Geometria dos polígonos}
#' }
"ubs_sp_areas"

#' Distacias e tempos de viagem a pe entre setores censitarios e UBS
#'
#' O dataset foi criado com a funcao \code{asha_dist} e contem dados obtidos
#' da à Google Distance Matrix API. Inclui variaveis de distancias e tempos de
#' viagem entre os centroides dos setores censitarios da cidade de Sao Paulo e
#' as 5 UBS mais proximas de cada um.
#'
#' @format Um data frame contendo 94765 linhas e 10 variaveis:
#'
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
"ubs_sp_mobilidade"

#' Pontos de UBS e centroides de setores censitarios unidos
#'
#' E a versao de ubs_sp_mobilidade com os casos que nao apresentaram
#' dados de rotas para modelo vigente entre as 5 UBS com menores
#' distancias euclidianas ate os setores censitarios.
#'
#' @format Um data frame contendo 94765 linhas e 10 variaveis:
#'
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
"rotas_faltas"

#' Total de medicos e enfermeiros nas UBS de Sao Paulo
#'
#' O dataset conta com informacoes a respeito do total de profissionais
#' de atencao basica de saude (medicos e enfermeiros) trabalhando em
#' cada uma das UBS da cidade de Sao Paulo.
#'
#' @format Um data frame com 456 linhas e 3 variaveis:
#'
#' \describe{
#'   \item{cnes}{Codigo de identificacao da UBS}
#'   \item{total_med}{Quantidade de medicos da UBS}
#'   \item{total_enf}{Quantidade de enfermeiros da UBS}
#'}
"ubs_sp_profissionais"



#' Pontos de UBS e centroides de setores censitarios unidos
#'
#' O dataset reune os pontos de UBS e dos centroides dos setores
#' censitarios. É o formato padrão do parametro \code{zones} da
#' funcao \code{asha_dists}
#'
#' @format Um dataset classe sf com pontos, contendo
#'         19409 linhas e 1 variavel:
#'
#' \describe{
#'   \item{id}{Codigo de identificacao das origens e destinos}
#' }
"zonas"

#' Pontos de UBS e centroides de setores censitarios unidos
#'
#' O dataset reune os pontos de UBS e dos centroides dos setores
#' censitarios. É o formato padrão do parametro \code{zones} da
#' funcao \code{asha_dists}
#'
#' @format Um dataset classe sf com pontos, contendo
#'         19409 linhas e 1 variavel:
#'
#' \describe{
#'   \item{id}{Codigo de identificacao das origens e destinos}
#' }
"rotas_faltas"
