load("data-raw/rotas.rda")

rotas <-
  rotas %>%
  rename(cd_geocodi = code_o,
         cnes = code_d) %>%
  mutate(minutos_pe = duration / 60) %>%
  rename(origem = from_addresses,
         destino = to_addresses,
         metros_pe = distances,
         segundos_pe = duration,
         ox = fx, oy = fy,
         dx = tx, dy = ty) %>%
  select(-currency, -fare)

devtools::use_data(rotas)
