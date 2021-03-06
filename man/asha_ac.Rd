% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_functions.R
\name{asha_ac}
\alias{asha_ac}
\title{Indicador AC}
\usage{
asha_ac(df1, pop, area, model, n = 1)
}
\arguments{
\item{df1}{Um dataframe contendo as variaveis necessarias para o calculo. Veja os detalhes.}

\item{pop}{Variavel com o total de habitantes da menor unidade territorial}

\item{area}{Variavel de id da area de cobertura, para a qual o indicador sera calculado}

\item{model}{Variavel opcional para a construcao do indicador para modelos diferentes}

\item{n}{Grandeza da taxa (100, 1000, 10000 habitantes...)}
}
\value{
Retorna o dataframe indicado no argumento \code{df1} acrescidas as colunas demanda
        (que contabiliza a demanda total da \code{area}) e \code{ac}, com o indicador de
        de acessibilidade competitiva, por area.
}
\description{
Cria o indicador de acessibilidade competitiva
}
\details{
Alem das variaveis com numero de habitantes e id de area, que sao passados
         na funcao, o dataframe deve conter a variavel \code{oportunidades}, que
         sera automaticamente usada no calculo do indicador AC. Se o calculo for para
         modelos diferentes, o data.frame devera ter uma variavel chamada modelo, com
         os respectivos valores indicativos. Neste caso a funcao calcula o indicador da
         area em cada modelo.
}
\examples{
\dontrun{
asha_ac(df1, pop, area, model)
}

}
\author{
Bruno Pinheiro
}
