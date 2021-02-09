#' @title Cria um data frame com os dados de restos a pagar.
#' @name build_rp
#'
#' @description Converte os dados gerados para importação pelo SIAPC/PAD do TCE/RS em um Data Frame do R com os dados de restos a pagar.
#'
#' @param empenho Data frame de EMPENHO.TXT obtido por parse_empenho()
#' @param liquidac Data frame de LIQUIDAC.TXT obtido por parse_liquidac()
#' @param pagament Data frame de PAGAMENT.TXT obtido por parse_pagament()
#' @return Um Data Frame com os dados.
#'
#' @author Everton da Rosa
#'
#' @export
#' @import sqldf
#'
build_rp <- function(empenho, liquidac, pagament){

  data_base <- sqldf("select data_base from empenho")
  # data limite para considerar os restos a pagar
  data_limite <- as.POSIXlt(as.Date(data_base$data_base[1]))
  data_limite$year <- data_limite$year - 1
  ano_limite <- format(data_limite, "%Y")
  data_limite <- as.Date(paste(ano_limite, 12, 31, sep = "-"), "%Y-%m-%d")

  # datas de início e fim da movimentação dos restos
  # inicio_movimentacao_restos <- as.POSIXlt(as.Date(data_base$data_base[1]))
  # ano_inicio <- format(inicio_movimentacao_restos, "%Y")
  # inicio_movimentacao_restos <- as.Date(paste(ano_inicio, 1, 1, sep = "-"), "%Y-%m-%d")
  # fim_movimentacao_restos <- as.Date(data_base$data_base[1])


  # lista de empenhos

  s <- sprintf("select
                      empenho.orgao,
                      empenho.uniorcam,
                      empenho.funcao,
                      empenho.subfuncao,
                      empenho.programa,
                      empenho.projativ,
                      empenho.rubrica,
                      empenho.recurso_vinculado,
                      empenho.contrapartida_recurso_vinculado,
                      empenho.numero_empenho,
                      empenho.credor,
                      empenho.caracteristica_peculiar,
                      empenho.complemento_recurso_vinculado,
                      empenho.ano_empenho,
                      empenho.cnpj,
                      empenho.data_base,
                      empenho.data_geracao,
                      empenho.entidade,
                      sum(empenho.valor_empenho) as empenhado,
                      (select sum(valor_liquidacao) from liquidac where liquidac.numero_empenho = empenho.numero_empenho and data_liquidacao <= %d) as liquidado_anterior,
                      (select sum(valor_pagamento) from pagament where pagament.numero_empenho = empenho.numero_empenho and data_pagamento <= %d) as pago_anterior,
                      (select sum(valor_liquidacao) from liquidac where liquidac.numero_empenho = empenho.numero_empenho and data_liquidacao > %d) as nao_processado_liquidado,
                      (select sum(valor_pagamento) from pagament where pagament.numero_empenho = empenho.numero_empenho and data_pagamento > %d) as restos_pago,
                      (select sum(valor_liquidacao) from liquidac where liquidac.numero_empenho = empenho.numero_empenho and data_liquidacao > %d and valor_liquidacao < 0) as processados_cancelado
                    from empenho
                    where data_empenho <= %d
                    group by
                      empenho.orgao,
                      empenho.uniorcam,
                      empenho.funcao,
                      empenho.subfuncao,
                      empenho.programa,
                      empenho.projativ,
                      empenho.rubrica,
                      empenho.recurso_vinculado,
                      empenho.contrapartida_recurso_vinculado,
                      empenho.numero_empenho,
                      empenho.credor,
                      empenho.caracteristica_peculiar,
                      empenho.complemento_recurso_vinculado,
                      empenho.ano_empenho,
                      empenho.cnpj,
                      empenho.data_base,
                      empenho.data_geracao,
                      empenho.entidade
                    ", data_limite, data_limite, data_limite, data_limite, data_limite, data_limite)
  # print(s)
  empenhos <- sqldf(s)

  # substituindo valores NA por 0
  empenhos$empenhado[is.na(empenhos$empenhado)] <- 0
  empenhos$liquidado_anterior[is.na(empenhos$liquidado_anterior)] <- 0
  empenhos$pago_anterior[is.na(empenhos$pago_anterior)] <- 0
  empenhos$nao_processado_liquidado[is.na(empenhos$nao_processado_liquidado)] <- 0
  empenhos$restos_pago[is.na(empenhos$restos_pago)] <- 0
  empenhos$processados_cancelado[is.na(empenhos$processados_cancelado)] <- 0


  # saldo inicial de restos

  empenhos$saldo_nao_processado <- round(as.numeric(empenhos$empenhado) - as.numeric(empenhos$liquidado_anterior), 2)
  empenhos$saldo_processado <- round(as.numeric(empenhos$liquidado_anterior) - as.numeric(empenhos$pago_anterior), 2)

  empenho_cancelado <- data.frame()
  total_liquidado <- data.frame()
  total_pago <- data.frame()
  total_empenhado <- data.frame()
  for (i in 1:nrow(empenhos)) {
    numero_empenho <- empenhos[i, "numero_empenho"]

    s <- sprintf("select sum(valor_empenho) as nao_processado_cancelado from empenho where data_empenho > %d and numero_empenho like '%s' and valor_empenho < 0", data_limite, numero_empenho)
    empenho_cancelado <- rbind(empenho_cancelado, sqldf(s))

    s <- sprintf("select sum(valor_empenho) as total_empenho from empenho where numero_empenho like '%s'", numero_empenho)
    total_empenhado <- rbind(total_empenhado, sqldf(s))

    s <- sprintf("select sum(valor_liquidacao) as total_liquidacao from liquidac where  numero_empenho like '%s'", numero_empenho)
    total_liquidado <- rbind(total_liquidado, sqldf(s))

    s <- sprintf("select sum(valor_pagamento) as total_pagamento from pagament where  numero_empenho like '%s'", numero_empenho)
    total_pago <- rbind(total_pago, sqldf(s))
  }
  empenho_cancelado[is.na(empenho_cancelado)] <- 0
  total_empenhado[is.na(total_empenhado)] <- 0
  total_liquidado[is.na(total_liquidado)] <- 0
  total_pago[is.na(total_pago)] <- 0

  empenhos <- cbind(empenhos, empenho_cancelado, total_empenhado, total_liquidado, total_pago)

  # saldos finais
  empenhos$saldo_final_nao_processado <- round(empenhos$total_empenho - empenhos$total_liquidacao, 2)
  empenhos$saldo_final_processado <- round(empenhos$total_liquidacao - empenhos$total_pagamento, 2)

  return(empenhos)
}
