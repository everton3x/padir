#' @title Cria um data frame com os dados dos pagamentos
#' @name build_pagamento
#'
#' @description Converte os dados gerados para importação pelo SIAPC/PAD do TCE/RS em um Data Frame do R os dados dos pagamentos.
#'
#' @param empenho Data frame de EMPENHO.TXT obtido por parse_empenho()
#' @param pagament Data frame de PAGAMENT.TXT obtido por parse_pagament()
#'
#' @return Um Data Frame com os dados.
#'
#' @author Everton da Rosa
#'
#' @export
#' @import sqldf
#'
build_pagamento <- function(empenho, pagament){

  pagamento <- sqldf("
                    select
                      pagament.*,
                      (select distinct orgao from empenho where empenho.numero_empenho = pagament.numero_empenho) as orgao,
                      (select distinct uniorcam from empenho where empenho.numero_empenho = pagament.numero_empenho) as uniorcam,
                      (select distinct funcao from empenho where empenho.numero_empenho = pagament.numero_empenho) as funcao,
                      (select distinct subfuncao from empenho where empenho.numero_empenho = pagament.numero_empenho) as subfuncao,
                      (select distinct programa from empenho where empenho.numero_empenho = pagament.numero_empenho) as programa,
                      (select distinct projativ from empenho where empenho.numero_empenho = pagament.numero_empenho) as projativ,
                      (select distinct rubrica from empenho where empenho.numero_empenho = pagament.numero_empenho) as rubrica,
                      (select distinct recurso_vinculado from empenho where empenho.numero_empenho = pagament.numero_empenho) as recurso_vinculado,
                      (select distinct contrapartida_recurso_vinculado from empenho where empenho.numero_empenho = pagament.numero_empenho) as contrapartida_recurso_vinculado,
                      (select distinct credor from empenho where empenho.numero_empenho = pagament.numero_empenho) as credor,
                      (select distinct caracteristica_peculiar from empenho where empenho.numero_empenho = pagament.numero_empenho) as caracteristica_peculiar,
                      (select distinct complemento_recurso_vinculado from empenho where empenho.numero_empenho = pagament.numero_empenho) as complemento_recurso_vinculado,
                      (select distinct ano_empenho from empenho where empenho.numero_empenho = pagament.numero_empenho) as ano_empenho
                    from pagament
                    ")

  return(pagamento)

}
