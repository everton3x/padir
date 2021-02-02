#' @title Converte PAGAMENT.TXT para um Data Frame.
#' @name parse_pagament
#'
#' @description Converte os dados gerados para importação pelo SIAPC/PAD do TCE/RS em um Data Frame do R.
#'
#' @param arquivo_txt O caminho para o arquivo PAGAMENT.TXT
#' @return Um Data Frame com os dados.
#'
#' @author Everton da Rosa
#'
#' @export
#' @import readr
#' @import stringr
parse_pagament <- function(arquivo_txt){

  # Importa o TXT
  df <- read_fwf(
    arquivo_txt,
    fwf_cols(
      numero_empenho = 13,
      numero_pagamento = 20,
      data_pagamento = 8,
      valor_pagamento = 13,
      sinal_valor = 1,
      obsoleto1 = 120,
      codigo_operacao = 30,
      conta_contabil_debito = 20,
      orgao_debito = 2,
      uniorcam_debito = 2,
      conta_contabil_credito = 20,
      orgao_credito = 2,
      uniorcam_credito = 2,
      historico_pagamento = 400,
      numero_liquidacao = 20
    ),
    col_types = cols(
      numero_empenho = col_character(),
      numero_pagamento = col_character(),
      data_pagamento = col_character(),
      valor_pagamento = col_number(),
      sinal_valor = col_character(),
      obsoleto1 = col_character(),
      codigo_operacao = col_character(),
      conta_contabil_debito = col_character(),
      orgao_debito = col_character(),
      uniorcam_debito = col_character(),
      conta_contabil_credito = col_character(),
      orgao_credito = col_character(),
      uniorcam_credito = col_character(),
      historico_pagamento = col_character(),
      numero_liquidacao = col_character()
    ),
    skip = 1,
    trim_ws = T,
    progress = T,
    skip_empty_rows = T
  )

  # Remove a linha do FINALIZADOR
  df <- subset(df, df$numero_empenho != 'FINALIZADOR00')

  # Converte as colunas de moeda
  df$valor_pagamento <- round(as.numeric(paste(df$sinal_valor, df$valor_pagamento, sep = '')) / 100, digits = 2)

  # Acrescenta colunas extras
  df$ano_empenho <- as.character(substr(df$numero_empenho, 1, 5))

  # Formata campos
  df$data_pagamento <- as.Date(df$data_pagamento, tryFormats = c("%d%m%Y"))

  df$conta_contabil_debito <- gsub('^0{0,}', '', df$conta_contabil_debito)
  df$conta_contabil_debito <- str_pad(df$conta_contabil_debito, 20, c('right'), pad = '0')
  df$conta_contabil_debito <- sprintf(
    "%s.%s.%s.%s.%s.%s.%s.%s.%s.%s",
    str_sub(df$conta_contabil_debito, start = 1, end = 1),
    str_sub(df$conta_contabil_debito, start = 2, end = 2),
    str_sub(df$conta_contabil_debito, start = 3, end = 3),
    str_sub(df$conta_contabil_debito, start = 4, end = 4),
    str_sub(df$conta_contabil_debito, start = 5, end = 5),
    str_sub(df$conta_contabil_debito, start = 6, end = 7),
    str_sub(df$conta_contabil_debito, start = 8, end = 9),
    str_sub(df$conta_contabil_debito, start = 10, end = 11),
    str_sub(df$conta_contabil_debito, start = 12, end = 13),
    str_sub(df$conta_contabil_debito, start = 14, end = 15)
  )


  df$conta_contabil_credito <- gsub('^0{0,}', '', df$conta_contabil_credito)
  df$conta_contabil_credito <- str_pad(df$conta_contabil_credito, 20, c('right'), pad = '0')
  df$conta_contabil_credito <- sprintf(
    "%s.%s.%s.%s.%s.%s.%s.%s.%s.%s",
    str_sub(df$conta_contabil_credito, start = 1, end = 1),
    str_sub(df$conta_contabil_credito, start = 2, end = 2),
    str_sub(df$conta_contabil_credito, start = 3, end = 3),
    str_sub(df$conta_contabil_credito, start = 4, end = 4),
    str_sub(df$conta_contabil_credito, start = 5, end = 5),
    str_sub(df$conta_contabil_credito, start = 6, end = 7),
    str_sub(df$conta_contabil_credito, start = 8, end = 9),
    str_sub(df$conta_contabil_credito, start = 10, end = 11),
    str_sub(df$conta_contabil_credito, start = 12, end = 13),
    str_sub(df$conta_contabil_credito, start = 14, end = 15)
  )

  # Acrescenta os dados do cabeçalho
  cabecalho <- scan(arquivo_txt, nlines = 1, what = 'character', quiet = T)
  cabecalho <- paste(cabecalho, collapse = " ")
  cnpj <- str_sub(cabecalho, start = 1, end = 14)
  data_base <- str_sub(cabecalho, start = 23, end = 30)
  data_base <- as.Date(data_base, format = '%d%m%Y')
  data_geracao <- str_sub(cabecalho, start = 31, end = 38)
  data_geracao <- as.Date(data_geracao, format = '%d%m%Y')
  entidade <- str_trim(str_sub(cabecalho, start = 39, end = 118), side = c("both"))
  df$cnpj <- cnpj
  df$data_base <- data_base
  df$data_geracao <- data_geracao
  df$entidade <- entidade

  # Retornando o resultado
  return(df)
}
