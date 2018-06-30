# load("data-raw/rotas.rda")
#
# rotas <-
#   rotas %>%
#   rename(cd_geocodi = code_o,
#          cnes = code_d) %>%
#   mutate(minutos_pe = duration / 60) %>%
#   rename(origem = from_addresses,
#          destino = to_addresses,
#          metros_pe = distances,
#          segundos_pe = duration,
#          ox = fx, oy = fy,
#          dx = tx, dy = ty) %>%
#   select(-currency, -fare)
api01 <- "AIzaSyBRPrAjSE_pRMWSq_XlO4BFwGD63j_gB4U"
api02 <- "AIzaSyCMQ5yJQ6UmiHCOT8M-S5mwENpAa2BZcMs"


apis <- c("AIzaSyBRPrAjSE_pRMWSq_XlO4BFwGD63j_gB4U",
          "AIzaSyCMQ5yJQ6UmiHCOT8M-S5mwENpAa2BZcMs",
          "AIzaSyBBKLBM_tckdQQxURAaGI5YRFKD0vVkX9U")


data(cents, package = "stplanr")
data(flow, package = "stplanr")

zonas <- as(st_transform(base_saude_setores, 4326), "Spatial")
od <- stplanr::od2odf(flow=modelo_proximidade[c(1:5,256,274,276), ], zones=zonas)

data(cents, package = "stplanr")
data(flow, package = "stplanr")

od <- stplanr::od2odf(flow=flow, zones=cents)

uma_linha <- data.frame(from_addresses=NA, to_addresses=NA, distances=NA,
                        duration=NA, currency=NA, fare=NA)
output <- data.frame()

for (linha in 1:nrow(od)) {
  o <- od[linha, 3:4]
  d  <- od[linha, 5:6]
  output <- tryCatch(
    {
      rbind(output, stplanr::dist_google(from = o, to = d,
                                         mode = 'walking', google_api = api02))
    },
    error = function(na) {
      message("Erro: No results for this request (e.g. due to lack of support for this mode between the from and to locations)")
      message(na)
      output <- rbind(output, uma_linha)
    }
  )
}

dput(modelo_proximidade[c(1:5,256,274,276), ])

setores <- modelo_proximidade[c(1:5,256,274,276), 1]
cnes <- modelo_proximidade[c(1:5,256,274,276), 2]

base_saude_setores <-


n <- 1

for (linha in 1:nrow(od)) {
  o <- od[linha, 3:4]
  d  <- od[linha, 5:6]
  output <- tryCatch(
    {
      rbind(output, stplanr::dist_google(from = o, to = d,
                                         mode = 'walking',
                                         google_api = apis[n]))
    },
    error = function(na) {
      message("Erro: No results for this request (e.g. due to lack of support for this mode between the from and to locations)")
      message(na)
      output <- rbind(output, uma_linha)
      },
    error = function(quota) {
      message("Erro: You have exceeded your daily request quota for this API.")
      message(quota)
      n <- n + 1
      }
  )
}



myTryCatch <- function(expr) {
  warn <- err <- NULL
  value <- withCallingHandlers(
    tryCatch(expr,
             error=function(e) {
               err <<- e
               NULL
               }),
    warning=function(w) {
      warn <<- w
      invokeRestart("muffleWarning")
    })
  list(value=value, warning=warn, error=err)
}
devtools::use_data(rotas)
