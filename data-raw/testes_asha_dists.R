#### FUNCAO BASICA PARA TESTES --------

zonas <- as(st_transform(base_saude_setores, 4326), "Spatial")
od <- stplanr::od2odf(flow=modelo_proximidade[c(1:5,256,274,276), ], zones=zonas)
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

for (linha in 1:nrow(od)) {
  o <- od[linha, 3:4]
  d  <- od[linha, 5:6]
  n <- 1
  output <- tryCatch(
    {
      rbind(output, stplanr::dist_google(from = o, to = d,
                                         mode = 'walking', google_api = apis[n]))
    },
    custom_error = function(e) {
      err <- conditionMessage(e)
      message("found custom_error: ", err)
      output <- rbind(output, uma_linha)
    },
    error = function(e) {
      err <- conditionMessage(e)
      message("found an error: ", err)
      n <- n + 1
    }
  )
  return(output)
}


asha_dists <- function(fluxo, zonas, api) {
  zonas <- as(st_transform(zonas, 4326), "Spatial")
  od <- stplanr::od2odf(flow = fluxo, zones = zonas)
  uma_linha <- data.frame(from_addresses=NA, to_addresses=NA, distances=NA,
                          duration=NA, currency=NA, fare=NA)
  n <- 1
  output <- data.frame()
  for (linha in 1:nrow(od)) {
    o <- od[linha, 3:4]
    d  <- od[linha, 5:6]
    output <- tryCatch({
      rbind(
        output,
        stplanr::dist_google(
          from = o, to = d, mode = 'walking', google_api = api[n]))
    },
    error = function(e) {
      err <- conditionMessage(e)
      if (startsWith("No results for this request", err)) {
        warning(err)  # warn instead of error
        output <- rbind(output, uma_linha)  # empty output as result
      } else if (startsWith("You have exceeded your daily", err)) {
        warning(err)
        n <<- n + 1   # '<<-' to update n _outside_ this function
      }
    }
    )}
  return(output)
}




