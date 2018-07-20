## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  message = FALSE,
  error = FALSE
)

## ------------------------------------------------------------------------
# install.packages("devtools") # para o caso do devtools não estar instalado
# devtools::install_github("bruno-pinheiro/asha") # para instalar o pacote asha

## ------------------------------------------------------------------------
library(dplyr)
library(sf)
library(ggplot2)
library(stplanr)
library(asha)

## ------------------------------------------------------------------------
setores_sp <- setores_sp %>%
  select(cd_geocodi:dens_demografica)
head(as.data.frame(setores_sp))

## ------------------------------------------------------------------------
head(as.data.frame(ubs_sp))

## ---- fig.width = 7, fig.height = 5, eval = FALSE------------------------
#  p1 <- ggplot() +
#    geom_sf(data = setores_sp, fill = "white", lwd = .1) +
#    labs(caption = "Setores censitários")
#    tema_mapa
#  
#  p2 <- ggplot() +
#    geom_sf(data = ubs_sp_areas, , fill = "white", lwd = .3) +
#    geom_sf(data = ubs_sp, col = "red", lwd = .2) +
#    labs(caption = "UBS e áreas de UBS")
#  
#  do.call(gridExtra::grid.arrange, c(list(p1, p2), ncol = 2))

## ------------------------------------------------------------------------
head(pessoas_sp)

## ------------------------------------------------------------------------
sum(setores_sp$pessoas_sp, na.rm = TRUE)

## ------------------------------------------------------------------------
head(ubs_sp_profissionais)

## ------------------------------------------------------------------------
centroides_sp <-
  st_centroid(setores_sp) %>%
  select(cd_geocodi)
glimpse(centroides_sp)

## ------------------------------------------------------------------------
od_euclidiano <- asha_nn(ubs_sp, centroides_sp, "cnes", "cd_geocodi", 5)
glimpse(od_euclidiano)

## ------------------------------------------------------------------------
zonas <- asha_zones(centroides_sp, ubs_sp, "cd_geocodi", "cnes")
glimpse(zonas)

## ------------------------------------------------------------------------
cd_setor <- "355030871000027"
cd_ubs <- od_euclidiano %>% filter(cd_geocodi == cd_setor) %>% pull(cnes)

ubs <- ubs_sp %>% filter(cnes %in% cd_ubs) 

df <- data.frame(cd_geocodi = cd_setor,
                 cnes = cd_ubs)

linhas <- stplanr::od2line(df, zonas)
linhas2 <- as(st_transform(linhas, 4326), "Spatial")
rotas <- stplanr::line2route(linhas2, route_fun = route_osrm)
rotas2 <- st_transform(st_as_sf(rotas), st_crs(linhas))

p1 <-
  ggplot() +
  geom_sf(data = ubs_sp_areas %>% filter(cnes %in% cd_ubs), fill = "white", col = "lightgrey") +
  geom_sf(data = setores_sp %>% filter(cd_geocodi %in% cd_setor), fill = "yellow", lwd = .2) +
  # geom_sf(data = setor) +
  geom_sf(data = ubs, col = "black", cex = 1.2) +
  geom_sf(data = linhas, col = "red", lwd = .2) +
  geom_sf(data = rotas2, col = "blue")


cd_setor <- "355030804000023"
cd_ubs <- od_euclidiano %>% filter(cd_geocodi == cd_setor) %>% pull(cnes)

ubs <- ubs_sp %>% filter(cnes %in% cd_ubs) 

df <- data.frame(cd_geocodi = cd_setor,
                 cnes = cd_ubs)

linhas <- stplanr::od2line(df, zonas)
linhas2 <- as(st_transform(linhas, 4326), "Spatial")
rotas <- stplanr::line2route(linhas2, route_fun = route_osrm)
rotas2 <- st_transform(st_as_sf(rotas), st_crs(linhas))

p2 <-
  ggplot() +
  geom_sf(data = ubs_sp_areas %>% filter(cnes %in% cd_ubs), fill = "white", col = "lightgrey") +
  geom_sf(data = setores_sp %>% filter(cd_geocodi %in% cd_setor), fill = "yellow", lwd = .2) +
  # geom_sf(data = setor) +
  geom_sf(data = ubs, col = "black", cex = 1.2) +
  geom_sf(data = linhas, col = "red", lwd = .2) +
  geom_sf(data = rotas2, col = "blue")

## ---- fig.width=7, fig.height=4, fig.align='center'----------------------
do.call(gridExtra::grid.arrange, c(list(p1, p2), ncol =2))

## ---- eval = FALSE-------------------------------------------------------
#  # não executar
#  ubs_sp_mobilidade <- asha_dists(od_euclidiano, zonas)

## ------------------------------------------------------------------------
glimpse(ubs_sp_mobilidade)

## ------------------------------------------------------------------------
od_vigente <- asha_intersect(ubs_sp_areas, centroides_sp, "cnes", "cd_geocodi")
glimpse(od_vigente)

## ------------------------------------------------------------------------
od_vigente <-
  od_vigente %>%
  rbind(
    data.frame(cd_geocodi = setores_sp$cd_geocodi[!(setores_sp$cd_geocodi %in% od_vigente$cd_geocodi)],
               cnes = c("3121135", "4049934", "2788039", "2788217", "2788500"))
    )

## ------------------------------------------------------------------------
od_vigente <- 
  od_vigente %>%
  merge(ubs_sp_mobilidade, by = c("cd_geocodi", "cnes"), all.x = TRUE) %>%
  select(cd_geocodi, cnes, distancias, tempo) %>%
  mutate(modelo = "vigente")
summary(od_vigente)

## ------------------------------------------------------------------------
# separar
od_vigente_falta <-
  od_vigente %>%
  filter(is.na(tempo)) %>%
  select(cd_geocodi, cnes)

## ---- eval = FALSE-------------------------------------------------------
#  # não executar
#  rotas_faltas <- asha_dists(od_vigente_falta, zonas)

## ------------------------------------------------------------------------
od_vigente <-
  od_vigente %>%
  filter(!is.na(tempo)) %>%
  rbind(
    data.frame(select(rotas_faltas, cd_geocodi, cnes, distancias, tempo),
               modelo = "vigente")
    )

## ---- fig.width=5, fig.height=6------------------------------------------
# linhas <- stplanr::od2line(od_vigente, zonas)
# 
# st_write(linhas, "linhas.shp")
# 
# st_write(ubs_sp_areas, "ubs_sp_areas.shp")
# st_write(ubs_sp, "ubs_sp.shp")
# 
# ggplot() +
#   geom_sf(data = ubs_sp_areas) +
#   # geom_sf(data = setores_sp %>% filter(cd_geocodi %in% cd_setor), fill = "yellow", lwd = .2) +
#   # geom_sf(data = setor) +
#   # geom_sf(data = ubs, col = "black", cex = 1.2) +
#   geom_sf(data = linhas, lwd = .2, aes(colour = tempo / 60)) +
#   # geom_sf(data = rotas2, col = "blue") +
#   tema_mapa

## ------------------------------------------------------------------------
od_prox <-
  ubs_sp_mobilidade %>%
  group_by(cd_geocodi) %>%
  filter(tempo == min(tempo)) %>%
  filter(distancias == min(distancias)) %>% # há casos de menores tempos com distancias iguais
  ungroup() %>%
  select(cd_geocodi, cnes, distancias, tempo) %>%
  mutate(modelo = "proximidade")
  
glimpse(od_prox)

## ------------------------------------------------------------------------
# identificar os setores que não estão em od_prox
setores <- setores_sp$cd_geocodi[!(setores_sp$cd_geocodi %in% od_prox$cd_geocodi)]
length(setores)

## ------------------------------------------------------------------------
# verificar estes setores em ubs_sp_mobilidade
erros <-
  ubs_sp_mobilidade %>%
  filter(cd_geocodi %in% setores) %>%
  select(cd_geocodi, cnes, distancias, tempo)
summary(erros)

## ------------------------------------------------------------------------
table(erros$cd_geocodi)

## ---- eval = FALSE-------------------------------------------------------
#  # não executar
#  erros_dists <- asha_dists(erros, zonas)
#  summary(erros_dists)

## ------------------------------------------------------------------------
od_prox <-
  od_prox %>%
  rbind(od_euclidiano %>%
          filter(proximidade == 1, cd_geocodi %in% setores) %>%
          select(cd_geocodi, cnes) %>%
          mutate(distancias = NA, tempo = NA, modelo = "proximidade")
        )
glimpse(od_prox)

## ------------------------------------------------------------------------
rm(erros)

## ---- eval = FALSE-------------------------------------------------------
#  # não executar
#  teste <- asha_dists(od_euclidiano[60001:60005, ], zonas, modal = "transit", api = "chave_da_api")

## ------------------------------------------------------------------------
od <-
  rbind(od_vigente, od_prox) %>%
  merge(ubs_sp_profissionais, by = "cnes", all.x = TRUE) %>%
  merge(setores_sp, by = "cd_geocodi", all.x = TRUE) %>%
  mutate(oportunidades = total_enf + total_med) %>%
  select(cd_geocodi, cnes, dens_demografica, pessoas_sp,
         distancias, tempo, total_enf, total_med, oportunidades, modelo)
  
summary(od)

## ---- fig.width=7, fig.height=8------------------------------------------
vars <- od %>% select_if(is.numeric) %>% select(-oportunidades) %>% names()
plots <- lapply(vars, function(i) asha_hist(od, i, "modelo"))
do.call(gridExtra::grid.arrange, c(plots, ncol = 2))

## ------------------------------------------------------------------------
od <- asha_ac(od, pessoas_sp, cnes, modelo, 1000)
glimpse(od)

## ---- fig.width=7, fig.height=6, warning = FALSE, message = FALSE--------
vars <- c("oportunidades", "demanda", "ac")
plots <- lapply(vars, function(i) asha_hist(od, i, fill = "modelo"))
do.call(gridExtra::grid.arrange, c(plots, ncol = 2))

## ------------------------------------------------------------------------
od <- asha_av(od, cnes, tempo, pessoas_sp)
glimpse(od)

## ------------------------------------------------------------------------
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

## ---- fig.width=7, fig.height=4, eval = FALSE----------------------------
#  p1 <-
#    setores_sp %>%
#    group_by(av_vig) %>%
#    summarise(pop = sum(pessoas_sp, na.rm = T)) %>%
#    ggplot() +
#    geom_sf(aes(fill = av_vig), lwd = .2) +
#    labs(caption = "Modelo vigente")
#  
#  p2 <-
#    setores_sp %>%
#    group_by(av_prox) %>%
#    summarise(pop = sum(pessoas_sp, na.rm = T)) %>%
#    ggplot() +
#    geom_sf(aes(fill = av_prox), lwd = .2) +
#    labs(caption = "Modelo de proximidade")
#  
#  do.call(gridExtra::grid.arrange, c(list(p1, p2), ncol = 2))

## ---- fig.height=2, fig.width=7------------------------------------------
od %>%
  group_by(modelo, av) %>%
  filter(!is.na(av_prop), !is.na(av)) %>%
  summarise(pop = sum(pessoas_sp, na.rm = T)) %>%
  mutate(av_prop = prop.table(pop)) %>%  ggplot(aes(y = av_prop, x = modelo)) +
  geom_bar(aes(fill = av), stat = "identity") +
  geom_text(aes(label = scales::percent(round(av_prop, 3)), group = av),
            position = position_stack(vjust=.5),
            size = 3, color = "white", fontface = "bold") +
  xlab(NULL) + ylab("% AV") +
  theme(legend.position = "top") +
  coord_flip()

## ------------------------------------------------------------------------
od$modelo <- factor(od$modelo, levels=rev(levels(factor(od$modelo))))

df <-
  od %>%
  filter(!is.na(ac_classes)) %>%
  group_by(modelo, ac_classes) %>%
  summarise(AV = median(av_prop, na.rm = TRUE),
            AC = round(median(ac, na.rm = TRUE), 2)) %>%
  rename(Intervalos = ac_classes)

df_tab <-
  df[1:5, 2:4] %>%
  bind_cols(df[6:10, 3:4])

df_tab <- df_tab %>%
  mutate(AV_d = scales::percent(AV1 - AV),
         AV_d = kableExtra::cell_spec(AV_d, color = ifelse(AV_d < 0, "red", "")),
         AC_d = scales::percent((AC1 / AC) - 1),
         AC_d = kableExtra::cell_spec(AC_d, color = ifelse(AC_d < 0, "red", "")),
         AV = scales::percent(AV),
         AV1 = scales::percent(AV1)) %>%
  rename(AC = AC1,
         AV = AV1,
         AC = AC_d,
         AV = AV_d)

knitr::kable(df_tab, escape = F, format = "html", booktabs = T,
      caption = "Medianas dos indicadores: comparação por modelo e quintil de AC",
      format.args = list(decimal.mark = ',', big.mark = ".")) %>%
  kableExtra::kable_styling(font_size = 12) %>%
  kableExtra::add_header_above(c("",
                   "Modelo Vigente" = 2,
                   "Modelo Proximidade" = 2,
                   "Diferenças (Proximidade - Vigente)" = 2))

