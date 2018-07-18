# CARREGAR PACOTES --------------------------------------------------------
library(dplyr)
library(data.table)
library(sf)
library(lwgeom)
library(asha)

# IMPORTAR DADOS NO R -----------------------------------------------------

## Importar malha de setores censitários ---------------
setores_sp <-
  read_sf(dsn="inst/extdata/sp_setores_censitarios", layer="35SEE250GC_SIR",
          stringsAsFactors = F, options = "ENCODING=windows-1252") %>%
  rename_all(tolower) %>% # renomear tudo para minuscula
  filter(cd_geocodm == "3550308")# filtrar seteores da capital

## Importar Domicilio02_SP1.csv ---------------
# V001 Moradores em domicilios particulares e coletivos
pessoas_sp <-
  fread(system.file("extdata", "Domicilio02_SP1.csv", package = "asha"),
        sep = ";", dec = ",", encoding = "Latin-1", fill = T,
        stringsAsFactors = FALSE) %>%
  mutate(cd_geocodi=as.character(Cod_setor)) %>%
  select(cd_geocodi, V001) %>%
  rename(pessoas_sp = V001)

## Importar malha de áreas de UBS ---------------
ubs_sp_areas <-
  read_sf(dsn="inst/extdata/ubs_sp_areas", layer="AA_UBS_MSP_2015_2016_UTM_SIRGAS2000_fuso23S",
          stringsAsFactors = FALSE)

## Importar pontos de UBS ---------------
# load("data-raw/base_ubs.rda")
ubs_sp <-
  read_sf(dsn="inst/extdata/ubs_sp", layer="SIRGAS_SHP_TEMA_-_SAUDE_UBS-POSTO_DE_SAUDE-CENTRO_DE_SAUDE",
          stringsAsFactors = FALSE)

## Importar dados de profissionais em UBS ---------------
ubs_sp_profissionais <-
  merge(read.csv(system.file("extdata", "enfermeiros.csv", package = "asha"),
                 sep=";", stringsAsFactors = FALSE),
        read.csv(system.file("extdata", "medicos.csv", package = "asha"),
                 sep=";", stringsAsFactors = FALSE),
        by = "ESTAB_SA")

# PREPARAR BASES PARA MANIPULACAO  -------------------------------------------

## Malha de setores censitários ---------------
setores_sp <- setores_sp %>%
  st_make_valid() %>% # certificar-se da validade da geometria
  st_transform(31983) %>% # reprojetar para SIRGAS 2000
  select(cd_geocodi, cd_geocodd, nm_distrit, tipo) %>% # limpar variaveis desencessarias
  mutate(nm_distrit = iconv(nm_distrit, from = "UTF-8", to = "ASCII//TRANSLIT")) # limpar strings

## Malha de areas de UBS ---------------
ubs_sp_areas <- ubs_sp_areas %>%
  st_transform(31983) %>%
  st_make_valid() %>%
  # select(CNES, NOMEUBS, STS, CRS, SUBPREF) %>%
  rename_all(tolower) %>%
  mutate(sts = as.factor(sts),
         crs = as.factor(crs))

## Dados de profissionais nas UBS ---------------
ubs_sp_profissionais <- ubs_sp_profissionais %>%
  mutate(cnes = substring(ESTAB_SA, first=1, last=7)) %>%
  rename_all(tolower) %>%
  select(sort(names(.)), -estab_sa) %>%
  filter(cnes %in% ubs_sp_areas$cnes)

## Pontos de localizacao de UBS ---------------

dim(ubs_sp)
dim(ubs_sp_areas)

#' Os dados de UBS e areas de UBS sao inconsistentes uns com os outros.
#' Ha 456 areas, porem 455 UBS na base de pontos, sendo que deveria existir
#' uma UBS por area de UBS. A base de pontos de UBS e mais antiga que a malha
#' de areas, de modo que vamos considerar os pontos desatualizados. Abaixo
#' verificamos qual UBS nao esta presente nos pontos.

## Identificar UBSs incongruentes ---------------

(a <- ubs_sp_areas$cnes[!(ubs_sp_areas$cnes %in% ubs_sp$eq_cnes)])

#' Esperava que a chamada acima retornasse apenas uma UBS, dado que ubs_sp_areas
#' tem 456 areas, contra 455 UBS em ubs_sp. Mas o resultado indica que ha 7 UBS
#' na malha de areas que nao estao na base de pontos de UBS, que sao:
#' "2751887" "2787539" "2751879" "4050312" "2027291" "2752328" "2751844".
#' Isto pode ser devido a fechamentos e aberturas de novas unidades, com novos
#' codigos de cadastro. Vou verificar entao o inverso, se ha codigos cnes nos pontos
#' que nao estao entre os codigos na malha de areas

(b <- ubs_sp$eq_cnes[!(ubs_sp$eq_cnes %in% ubs_sp_areas$cnes)])

#' Por sua vez ha seis UBS que nao estao na malha de areas de UBS. Se nenhuma
#' delas for igual, pontos concluir que foram fechadas 6 UBS e 7 novas foram
#' criadas no intervalo entre a producao de uma base e outra, resultando em uma
#' UBS a mais na base mais recente de areas de UBS

table(a %in% b)

#' Nenhuma delas sao coincidentes, entao podemos apenas excluir as seis UBS
#' identificas em a e incluir as 7 ubs identificadas em b.

ubs_sp <- ubs_sp %>%
  rename(cnes = eq_cnes) %>% # renomear variavel de identificacao
  select(cnes) %>% # limpar demais variaveis
  filter(!(cnes %in% b)) %>% # filtrar para retirar as que foram fechadas
  st_as_sf() %>% # converter para sf
  st_set_crs(31983) %>% # definir projeção SIRGAS 2000
  st_make_valid() # validar geometria

ubs_sp_criadas <- ubs_sp_areas %>%
  filter(cnes %in% a) %>%
  as.data.frame() %>%
  mutate(endereco = paste0(logradouro, ", ", numero, ", ", bairro, " - Sao Paulo, SP", ", ", cep)) %>%
  select(cnes, endereco)

#' A UBS de codigo cnes 2027291 (UBS Uniao de Vila Nova I) tem indicacao
#' de que foi fechada e que o atendimento esta sendo realizado na UBS com
#' codigo cnes 2819856 (UBS Uniao de Vila Nova II), tambem localizada no
#' bairro Sao Miguel Paulista. Vamos precisar considerar isto adiante.

## Geocodificar UBSs criadas ---------------

# Rodada 1
ubs_sp_criadas <- ubs_sp_criadas %>%
  filter(cnes != 2027291) %>%
  cbind(ggmap::geocode(ubs_sp_criadas$endereco[ubs_sp_criadas$cnes != 2027291]))

#' No caso houve uma falha durante a geocodificacao. Foi preciso entao
#' repetir o processo para obter as coordenadas do ponto que falhou na
#' primeira rodada (o erro e aleatorio, pode funcionar bem da primeira
#' vez ou necessidade de uma ou mais rodadas complementares para obter
#' as coordenadas dos seis pontos.

# Rodada 2
ubs_sp_criadas2 <- ubs_sp_criadas %>%
  filter(is.na(lon)) %>%
  select(1:2) %>%
  cbind(ggmap::geocode(ubs_sp_criadas$endereco[is.na(ubs_sp_criadas$lon)]))

#' Na segunda rodada o ponto retornou corretamente. Entao agora eu
#' posso simplesmente unir os dois objetos que resultaram das duas
#' rodadas de geocodificacao.

## Unir resultados de localização "corretos" em ubs_sp

# Juntar UBSs resultados das rodadas 1 e 2
ubs_sp_criadas <- ubs_sp_criadas %>%
  rbind(ubs_sp_criadas2) %>%
  filter(!is.na(lon))

rm(ubs_sp_criadas2)

# Transformar em objeto espacial sf e unir na base de UBS
ubs_sp_criadas <-
  st_as_sf(ubs_sp_criadas, coords = c(3, 4), crs = 4326, agr = "constant") %>%
  st_transform(31983) %>%
  st_make_valid() %>%
  select(cnes)

ubs_sp <- rbind(ubs_sp_criadas, ubs_sp)

#' Como eliminamos codigo cnes 2027291 (UBS Uniao de Vila Nova I) base de
#' pontos de UBS por estar desativada, precisamos atribuir a sua area
#' na malha de areas de UBS ao codigo cnes 2819856, que corresponde a UBS
#' que esta atendendo a demanda da que foi desativada.

## Corrigir codigo cnes da area da UBS excluída da base de pontos ----------------

ubs_sp_areas <- ubs_sp_areas %>%
  mutate(cnes = gsub("2027291", "2819856", cnes))

#' Antes de seguir: há 16 UBS com erros de geolocalizacao devido a motivos
#' diversos, como erro na criacao da base, atualizacao de endereco etc.
#' Este erro so e percebido depois de tracar as linhas de desejo, pois
#' algumas conexoes no modelo proximidade ficam esquisistas. E preciso efetuar
#' mais um procedimento manual, de checagem no cadastro do CNES conjuntamente
#' com a localizacao no Google Maps. Apos isto foram corrigidas as localizacoes
#' destas 16 UBS. Este passo segue abaixo.

# CORRECAO DE LOCALIZACAO DE PONTOS DE UBS --------------------------------

# Importar base de estabelecimentos de setembro de 2017 obtida no CNES
# em http://cnes.datasus.gov.br/pages/estabelecimentos/extracao.jsp

if(!(file.exists("inst/extdata/estabelecimentos-355030-201709.csv"))) {
  tmp <- tempfile(fileext = ".zip")
  baseurl = "http://cnesdownload.datasus.gov.br/download/EstabelecimentosServlet?path="
  path = "dWFXTnBjR2x2UFRNMU5UQXpNQ1puWlhOMFlXODlWQ1pqYjIxd1BUSXdNVGN3T1E9PWJYVg=="
  download.file(paste0(baseurl, path), tmp)
  unzip(tmp, exdir = "inst/extdata")
  unlink(tmp)
  rm(baseurl, path, tmp)
}

estab <-
  read.csv("inst/extdata/estabelecimentos-355030-201709.csv", sep = ";") %>%
  filter(grepl("UBS", NOME.FANTASIA))

ubs <-
  ubs_sp %>%
  rename(id = cnes)

checa_ubs <-
  asha_intersect(ubs_sp_areas, ubs, "cnes", "id") %>%
  filter(is.na(cnes) | is.na(id) | id != cnes)

enderecos <-
  ubs_sp_mobilidade %>%
  filter(cnes %in% checa_ubs$id) %>%
  distinct(cnes, .keep_all = TRUE) %>%
  select(2, 8, 10) %>%
  arrange(cnes)

#' Endereco consultado em http://cnes2.datasus.gov.br/cabecalho_reduzido.asp?VCod_Unidade=355030[cd_cnes]
#' e verificado no Google Maps. O "endereco" abaixo foi definido com base
#' no que resultava melhor localizacao no servico, o que nem sempre e o endereco,
#' como no exemplo de "UBS Vera Poty, Sao Paulo".

endereco_correto <-
  data.frame(cnes = as.character(sort(enderecos$cnes)),
             para_ok = as.character(
               c(NA,
                 "Estrada da Ligação, 01, Ponte Seca - São Paulo, SP",
                 "R. Lajedo, 123 - Cidade Tiradentes, São Paulo - SP",
                 "Av. Edu Chaves, 1197 - Parque Edu Chaves, São Paulo - SP",
                 "R. Frei Fidelis Mota, 1001 – Parque Cruzeiro do Sul, São Paulo - SP",
                 "R. Humberto de Almeida, 279, Chacara Santana, São Paulo - SP",
                 "Tv. Somos Todos Iguais, 915 - Jardim da Conquista, São Paulo - SP",
                 "UBS Vera Poty, São Paulo",
                 "R. Rolando Curti, 701 - Vila Clara, São Paulo - SP, 04413-070",
                 "UBS Vila Calu",
                 "UBS Parque do Largo - Estrada da Baronesa. 1000, Parque do Lago, São Paulo - SP",
                 "Rua Catarina Gabrielli, 236 - Americanópolis, São Paulo - SP",
                 "R. Benedito Schunck, 8 - Emburá, São Paulo - SP, 04893-050",
                 "AMA CHÁCARA CRUZEIRO DO SUL - Rua Mercedes Lopes - Vila Santana, São Paulo - SP",
                 "R. Francisco de Sáles, 10 - Jardim das Palmas, São Paulo - SP, 05749-280",
                 "UBS Barragem, São Paulo - SP",
                 "UBS MARSILAC - Estrada Engenheiro Marsilac - Emburá, São Paulo - SP"))
  )

#' É preciso repetir a sequência acima até ter as coordenadas para todas
#' as 16 UBS listadas em enderecos. No caso funcionou em duas rodadas,
#' mas durante replicacao pode ser que de certo tudo de primeira ou
#' mesmo demandar mais de duas rodadas.

# Rodada 1
geo_ubs_correto <-
  endereco_correto[2:17, ] %>%
  cbind(ggmap::geocode(as.character(endereco_correto$para_ok[2:17])))

result_na <- geo_ubs_correto %>%
  filter(is.na(lon))

#' Obteve dois erros. Refazendo com os dois erros.

# Rodada 2
geo_ubs_correto2 <-
  result_na[, 1:2] %>%
  cbind(ggmap::geocode(as.character(result_na$para_ok)))

result_na <- geo_ubs_correto2 %>%
  filter(is.na(lon))

#' Obteve os dois erros. Seguir adiante.

geo_corretos <- rbind(geo_ubs_correto,
                      geo_ubs_correto2) %>%
  filter(!is.na(lon))


######################### REVISADOS

geo_corretos <- geo_corretos[, c(1, 3, 4)] %>%
  sf::st_as_sf(coords = c(2, 3), crs = 4326, agr = "constant") %>%
  sf::st_transform(31983)

ubs_sp <- ubs_sp %>%
  filter(!(cnes %in% geo_corretos$cnes)) %>%
  rbind(geo_corretos)

# SALVAR OS DADOS BRUTOS EM data-raw/ --------------------------------------------

save(setores_sp, file = "data-raw/setores_sp.rda")
save(pessoas_sp, file = "data-raw/pessoas_sp.rda")
save(ubs_sp, file = "data-raw/ubs_sp.rda")
save(ubs_sp_areas, file = "data-raw/ubs_sp_areas.rda")
save(ubs_sp_profissionais, file = "data-raw/ubs_sp_profissionais.rda")

rm(list = ls())

#' Os dados ja estao prontos para manipulacao e geracao dos indicadores. Agora
#' e preciso rodar o script "03_trata_gerar_indicadores.R" que vai gerar as
#' bases de dados prontas para analise e salvar em data/
