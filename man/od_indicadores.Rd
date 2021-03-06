% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{od_indicadores}
\alias{od_indicadores}
\title{Setores censitarios da cidade de Sao Paulo}
\format{Um dataset classe sf com os poligonos dos setores censitarios
        contendo 37906 linhas e 15 variaveis:
\describe{
  \item{cd_geocodi}{Codigo de identificacao do setor censitario}
  \item{cnes}{Codigo CNES da UBS}
  \item{malha}{Indica se a observacao se refere a malha vigente ou ao experimento}
  \item{habitantes}{Numero de pessoas residentes nos setores censitarios}
  \item{enfermeiros}{Numero de enfermeiros nas UBS}
  \item{medicos}{Numero de medicos nas UBS}
  \item{oportunidades}{Indicador de Oportunidades -- medicos + enfermeiros}
  \item{demanda}{Indicador de Demanda das UBS}
  \item{distancia}{Distancia em metros do percurso a pe entre o setor
  e a UBS}
  \item{tempo}{Tempo de viagem do percurso a pe entre o setor censitario
  e a UBS em segundos}
  \item{minutos}{Tempo de viagem do percurso a pe entre o setor censitario
  e a UBS em minutos}
  \item{av}{Indica se o setor esta ou nao no raio de acessibilidade viavel da UBS}
  \item{av_prop_}{Proporcao de habitantes residentes no raio de
  acessibilidade viavel por UBS}
  \item{ac}{Indicador de Acessibilidade Competitiva}
  \item{geometry}{Geometria dos polígonos}
}}
\usage{
od_indicadores
}
\description{
O dataset foi estruturado a partir de dados do Censo 2010 disponibilidos
pelo IBGE (malha de setores censitarios do estado de Sao Paulo) e resultados
agregados por setor censitario para a cidade de Sao Paulo.
}
\keyword{datasets}
