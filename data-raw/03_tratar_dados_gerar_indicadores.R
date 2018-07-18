# CARREGAR PACOTES --------------------------------------------------------
library(dplyr)
library(sf)
library(asha)

load("data-raw/setores_sp.rda")
load("data-raw/ubs_sp.rda")
load("data-raw/ubs_sp_areas.rda")
load("data-raw/ubs_sp_profissionais.rda")

# OBTER DISTANCIAS E TEMPOS DE VIAGEM -------------------------------------

## Determinar pontos de origem ---------------

centroides_sp <-
  st_centroid(setores_sp) %>%
  select(cd_geocodi)

## Determinar OD por proximidade euclidiana ----------------

#' Isto sigfica identificar as "n" UBS mais proximas de cada setor censitario.
#' No caso estamos trabalhando com as 5 UBS mais proximas.

od_euclidiano <-
  asha_nn(ubs_sp, centroides_sp, "cnes", "cd_geocodi", 5)

## Preparar base de zonas ---------------

zonas <- asha_zones(centroides_sp, ubs_sp, "cd_geocodi", "cnes")

## Levantar dados de rotas ---------------
#'
# ubs_sp_mobilidade <- asha_dists(od_euclidiano, zonas)
#
# ubs_sp_mobilidade <-
#   ubs_sp_mobilidade %>%
#   rename(cd_geocodi = code_o,
#          cnes = code_d,
#          de = from_addresses,
#          para = to_addresses,
#          distancias = distances,
#          tempo = duration,
#          ox = fx, oy = fy,
#          dx = tx, dy = ty) %>%
#   select(-currency, -fare)
#
# devtools::use_data(ubs_sp_mobilidade)

#' O levantamento de dados de rotas consiste na consulta à Google Distance
#' Matrix API para obtencao de dados de rotas (distancia e tempo de viagem)
#' entre os pontos de origem e destino criados em `od_euclidiano`. Este processo
#' envolve multiplas rodadas de consulta, dado o limite de consultas gratuitas
#' imposto pelo Google. O procedimento foi realizado fora deste script (de
#' acordo com o exemplo acima, utilizando a funcao `asha_dist()`) e o conjunto
#' de dados resultante (`ubs_sp_mobilidade`) foi salvo em `data/` para estar
#' disponível facilmente --> `data("ubs_sp_mobilidade")`` chama o dataset.

# MODELAR A ACESSIBILIDADE ESPACIAL ---------------------------------------

## Modelo vigente ---------------

od_vigente <-
  asha_intersect(ubs_sp_areas, centroides_sp, "cnes", "cd_geocodi")

#' O resultado tem 18948 setores, de 18953. Os cinco nao incluidos
#' nao estao dentro de nenhuma area de ubs, por diferencas na geometria.
#' Abaixo incluo diretamente as linhas com os setores ausentes, mas busco
#' no mapa as UBS correspondentes para entãi=oo incluir no dataset.

od_vigente <-
  od_vigente %>%
  rbind(
    data.frame(cd_geocodi = setores_sp$cd_geocodi[!(setores_sp$cd_geocodi %in% od_vigente$cd_geocodi)],
               cnes = c("3121135", "4049934", "2788039", "2788217", "2788500"))
  )

## Incluir dados de rotas no OD vigente ---------------

od_vigente <-
  od_vigente %>%
  merge(ubs_sp_mobilidade, by = c("cd_geocodi", "cnes"), all.x = TRUE) %>%
  select(cd_geocodi, cnes, distancias, tempo) %>%
  mutate(modelo = "vigente")

#' As colunas distancias e tempo apresenta 701 valores NA nas colunas distancias
#' e tempo. Entre estes valores provavelmente há tanto casos sem resposta da
#' API, como casos em que a UBS do modelo vigente não é uma das 5 mais próximas
#' em termos euclidianos. É preciso então separar estes casos e realizar nova
#' consulta apenas para eles.

# Extrair fluxos OD faltantes ---------------
od_vigente_falta <-
  od_vigente %>%
  filter(is.na(tempo)) %>%
  select(cd_geocodi, cnes)

# Obter dados de rota para OD faltantes ---------------

# rotas_faltas <- asha_dists(od_vigente_falta, zonas)
# devtools::use_data(rotas_faltas)


# Excluir OD faltantes e recolocá-los com dados de rota ---------------
od_vigente <-
  od_vigente %>%
  filter(!is.na(tempo)) %>%
  rbind(
    data.frame(select(rotas_faltas, cd_geocodi, cnes, distancias, tempo),
               modelo = "vigente")
  )


## Modelo Proximidade ---------------

od_prox <-
  ubs_sp_mobilidade %>%
  group_by(cd_geocodi) %>%
  filter(tempo == min(tempo)) %>%
  filter(distancias == min(distancias)) %>% # ha menor tempo com distancia igual
  ungroup() %>%
  select(cd_geocodi, cnes, distancias, tempo) %>%
  mutate(modelo = "proximidade")

#' O objeto tem 18905 setores e não 18953. Vamos identificar abaixo quais sao.

# Extrair setores ausentes
setores <- setores_sp$cd_geocodi[!(setores_sp$cd_geocodi %in% od_prox$cd_geocodi)]

erros <-
  ubs_sp_mobilidade %>%
  filter(cd_geocodi %in% setores) %>%
  select(cd_geocodi, cnes, distancias, tempo)

# Consultar rotas para os erros
# erros_dists <- asha_dists(erros, zonas)

#' A consulta resulta em erro da API em todos os casos.

od_prox <-
  od_prox %>%
  rbind(od_euclidiano %>%
          filter(proximidade == 1, cd_geocodi %in% setores) %>%
          select(cd_geocodi, cnes) %>%
          mutate(distancias = NA, tempo = NA, modelo = "proximidade")
  )


## Finalizar base OD --------------
setores_sp <- setores_sp %>%
  merge(pessoas_sp, by = "cd_geocodi", all.x=T) %>% # mesclar dados de habitantes
  mutate(tipo = as.factor(tipo), # converter para fator
         area = as.numeric(st_area(.) / 1000000), # calcular area dos setores
         dens_demografica = pessoas_sp / area, # calcular densidade demografica
         nm_distrit = iconv(nm_distrit, from = "UTF-8", to = "ASCII//TRANSLIT")) # limpar strings



od <-
  rbind(od_vigente, od_prox) %>%
  merge(ubs_sp_profissionais, by = "cnes", all.x = TRUE) %>%
  merge(setores_sp, by = "cd_geocodi", all.x = TRUE) %>%
  mutate(oportunidades = total_enf + total_med) %>%
  select(cd_geocodi, cnes, dens_demografica, pessoas_sp,
         distancias, tempo, total_enf, total_med, oportunidades, modelo)




# CALCULAR INDICADORES ----------------------------------------------------

## Acessibilidade competitiva -------------

od <- asha_ac(od, pessoas_sp, cnes, modelo, 1000)

## Acessibilidade viável -------------

od <- asha_av(od, cnes, tempo, pessoas_sp)

# MESCLAR OD EM setores_sp ------------------------------------------------

od <-
  od %>%
  mutate(minutos_classes = cut(minutos,
                               breaks = c(0, 5, 10, 15, 30, 60, Inf),
                               labels = c("< 5", "5 a 10", "10 a 15",
                                          "15 a 30", "30 a 60", "> 60")),
         av_prop_decimais = cut(av_prop,
                                breaks = c(0, .1, .2, .3, .4, .5, .6, .7, .8, .9, 1),
                                labels = c("Até 10%", "10-20%", "20-30%", "30-40%", "40-50%",
                                           "50-60%", "60-70%", "70-80%", "80-90%", "90-100"))) %>%
  group_by(modelo) %>%
  mutate(ac_classes = recode(ntile(ac, 5),
                             "1" = "Q1", "2" = "Q2",
                             "3" = "Q3", "4" = "Q4",
                             "5" = "Q5")) %>%
  ungroup()

vigente <-
  od %>%
  filter(modelo == "vigente") %>%
  select(-pessoas_sp, -dens_demografica, -modelo, -total_enf, -total_med) %>%
  rename(cnes_vig = cnes,
         o_vig = oportunidades, d_vig = demanda,
         ac_vig = ac, minutos_vig = minutos,
         av_vig = av, av_prop_vig = av_prop,
         distancias_vig = distancias, tempo_vig = tempo,
         ac_c_vig = ac_classes, av_prop_decimais_vig = av_prop_decimais,
         minutos_classes_vig = minutos_classes)

proximidade <-
  od %>%
  filter(modelo == "proximidade") %>%
  select(-pessoas_sp, -dens_demografica, -modelo) %>%
  rename(cnes_prox = cnes,
         o_prox = oportunidades, d_prox = demanda,
         ac_prox = ac, minutos_prox = minutos,
         av_prox = av, av_prop_prox = av_prop,
         distancias_prox = distancias, tempo_prox = tempo,
         ac_c_prox = ac_classes, av_prop_decimais_prox = av_prop_decimais,
         minutos_classes_prox = minutos_classes)

setores_sp <-
  setores_sp %>%
  merge(vigente, by = "cd_geocodi", all.x = TRUE) %>%
  merge(proximidade, by = "cd_geocodi", all.x = TRUE)

# SALVAR OS DADOS EM DATA/ --------------------------------------------

devtools::use_data(od)
devtools::use_data(setores_sp)
devtools::use_data(pessoas_sp)
devtools::use_data(ubs_sp)
devtools::use_data(ubs_sp_areas)
devtools::use_data(ubs_sp_profissionais)
devtools::use_data(zonas)
devtools::use_data(centroides_sp)

rm(list=ls())

#' Neste ponto as bases de dados ja estao prontas para analise. Os dados foram
#' importados, preparados para manipulacao, os indicadores foram gerados e
#' estao presenntes tanto nas base tidy `od` como na malha de setores
#' censitarios `setores_sp`. Os dados podem ser mesclados tambem em
#' `ubs_sp_areas` a partir da coluna `cnes` de `od`.
#'
#' Os dados brutos utilizados nao acompanham o pacote pelo peso, mas apos o
#' procedimento de obtencao ficam armazenados em `inst/extdata`. Os dados
#' intermediarios, que correspondem aos dados brutos preparados para a geracao
#' de indicadores, ficam armazenados em `data-raw/`. Os dados finais, que
#' incluem os indicadores e estao prontos para analise, estao disponiveis no
#' diretorio `data/` e portanto estao embutidos no pacote, de modo que podem
#' ser acessados com o comando `data("nome_do_pacote")`.
