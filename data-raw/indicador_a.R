library(dplyr)
library(sf)
library(ggplot2)



data("od")
data("setores_sp")


od <- od %>%
  mutate(a = ac * av_prop,
         a_quintis = as.character(ntile(a, 5)),
         av_quintis = as.character(ntile(av_prop, 5)))



setores_sp <- setores_sp %>%
  merge(select(filter(od, modelo == "vigente"), cd_geocodi, a, a_quintis, av_quintis), by = "cd_geocodi", all.x = T) %>%
  rename(a_vig = a, a_quintis_vig = a_quintis, av_quintis_vig = av_quintis) %>%
  merge(select(filter(od, modelo == "proximidade"), cd_geocodi, a, a_quintis, av_quintis), by = "cd_geocodi", all.x = T) %>%
  rename(a_prox = a, a_quintis_prox = a_quintis, av_quintis_prox = av_quintis)

plots <- lapply(c("ac", "av_prop", "a"), function(i) asha_hist(od, i, "modelo"))
do.call(gridExtra::grid.arrange, c(plots, ncol = 3))

# setores_sp %>%
#   group_by(ac_c_vig) %>%
#   ggplot() +
#   geom_sf(aes(fill = ac_c_vig), lwd = 0, col = NA) +
#   scale_fill_brewer(palette = "RdPu") +
#   ggtitle(paste("Distribuição de", !!x))

plots <- lapply(c("ac_c_vig", "av_quintis_vig", "a_quintis_vig"), function(i) asha_map(setores_sp, i))
do.call(gridExtra::grid.arrange, c(plots, ncol = 3))

system.time(
  asha_map(setores_sp, "a_quintis_vig", "YlGn")
  )

system.time(
  ubs_sp_areas %>%
    as.data.frame() %>%
    ggplot() +
    geom_sf()
  )

system.time(
  ubs_sp_areas %>%
    ggplot() +
    geom_sf()
)


library(microbenchmark)

microbenchmark("noDF" = {
  setores_sp %>%
    ggplot() +
    geom_sf()
  },
  "DF" = {
    setores_sp %>%
      as.data.frame() %>%
      ggplot() +
      geom_sf()
  }
  )



system.time(setores_sp %>%
  as.data.frame() %>%
  group_by(a_quintis_vig) %>%
  mutate(ag = mean(a_vig, na.rm = TRUE)) %>%
  ggplot() +
  geom_sf(aes(fill = a_quintis_vig), lwd = 0, col = NA) +
  scale_fill_brewer(palette = "RdPu") +
  ggtitle(paste("Distribuição de", colnames(select(setores_sp, a_quintis_vig)))) +
  theme_map())

library(ggplot2)
