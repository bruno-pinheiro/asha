[![Travis build status](https://travis-ci.org/bruno-pinheiro/asha.svg?branch=master)](https://travis-ci.org/bruno-pinheiro/asha)

# asha

O pacote `asha` é uma ferramenta em desenvolvimento para a análise da acessibilidade de sistemas de atenção em saúde. Suas funções viabilizam a replicação de metodologia desenvolvida em projeto de pesquisa de iniciação cientítica desenvolvido no escopo do [OIPP (Observatório Interdisciplinar de Políticas Públicas das EACH/USP)](http://www5.each.usp.br/web/prof/oipp/) e orientado pelo professor do curso de Gestão de Políticas Públicas da USP, Dr. Alexandre Ribeiro Leichsenring.

Seguindo a metodologia do projeto e usando as funções do pacote é possível replicar os indicadores para as unidades territoriais intramunicipais do Censo 2010 (IBGE).

Mais informações sobre a metodologia, veja [este artigo](https://bruno-pinheiro.github.io/asha/articles/construcao-de-indicadores-de-acessibilidade-espacial-a-saude-com-o-pacote-asha.html).

# Instalação

O pacote não está disponível no CRAN, apenas no GitHub. Para instalar:

```r
install.packages("devtools")
devetools::install_github("bruno-pinheiro/asha")
```
