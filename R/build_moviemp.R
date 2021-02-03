#' @title Cria um data frame com a movimentação dos empenhos.
#' @name build_moviemp
#'
#' @description Converte os dados gerados para importação pelo SIAPC/PAD do TCE/RS em um Data Frame do R com a movimentação dos empenhos.
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
build_moviemp <- function(empenho, liquidac, pagament){

  empenhos <- sqldf("select
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
                      (select sum(valor_liquidacao) from liquidac where liquidac.numero_empenho = empenho.numero_empenho) as liquidado,
                      (select sum(valor_pagamento) from pagament where pagament.numero_empenho = empenho.numero_empenho) as pago
                    from empenho
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
                    ")

  return(empenhos)

}
