## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
# Antes de seguir vamos carregar os pacotes que utilizaremos
library(dplyr)
library(sf)
library(ggplot2)
library(stplanr)
library(asha)
library(ggsn)

## ------------------------------------------------------------------------
tema_mapa <-
  theme(panel.grid.major = element_line(colour = 'transparent'),
              panel.grid.minor = element_line(colour = 'transparent'),
              panel.background = element_rect(fill = 'transparent', colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(),
              axis.title = element_blank(),
              plot.margin = unit(rep(0, 4), "lines"))

## ---- fig.width = 5, fig.height = 6--------------------------------------
plot(setores_sp[ ,1], lwd = .1, col = "white")

## ------------------------------------------------------------------------
glimpse(setores_sp)

## ------------------------------------------------------------------------
sum(setores_sp$pessoas_setor, na.rm = TRUE)

## ---- fig.width = 5, fig.height = 6--------------------------------------
ggplot() +
  geom_sf(data = ubs_sp, col = "red", lwd = .3) +
  tema_mapa

## ------------------------------------------------------------------------
glimpse(ubs_sp)

## ---- fig.width = 5, fig.height = 6--------------------------------------
ggplot() +
  geom_sf(data = ubs_sp_areas, lwd = .3) +
  geom_sf(data = ubs_sp, col = "red", lwd = .2) +
  tema_mapa

## ------------------------------------------------------------------------
glimpse(ubs_sp_profissionais)

## ------------------------------------------------------------------------
centroides_sp <-
  st_centroid(setores_sp) %>%
  select(cd_geocodi)
glimpse(centroides_sp)

## ---- fig.width = 4, fig.height = 5--------------------------------------
plot(centroides_sp, col = "black", pch = 16, cex = .2)

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
  geom_sf(data = rotas2, col = "blue") +
  tema_mapa


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
  geom_sf(data = rotas2, col = "blue") +
  tema_mapa

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

