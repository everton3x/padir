#' @title Converte BRUB_ANT.TXT para um Data Frame.
#' @name parse_brub_ant
#'
#' @description Converte os dados gerados para importação pelo SIAPC/PAD do TCE/RS em um Data Frame do R.
#'
#' @param arquivo_txt O caminho para o arquivo BRUB_ANT.TXT
#' @return Um Data Frame com os dados.
#'
#' @author Everton da Rosa
#'
#' @export
#' @import readr
#' @import stringr
parse_brub_ant <- function(arquivo_txt){

  # Importa o TXT
  df <- read_fwf(
    arquivo_txt,
    fwf_cols(
      orgao = 2,
      uniorcam = 2,
      funcao = 2,
      subfuncao = 3,
      programa = 4,
      obsoleto1 = 3,
      projativ = 5,
      rubrica = 15,
      recurso_vinculado = 4,
      empenhado_1bim = 11,
      empenhado_2bim = 11,
      empenhado_3bim = 11,
      empenhado_4bim = 11,
      empenhado_5bim = 11,
      empenhado_6bim = 11,
      liquidado_1bim = 11,
      liquidado_2bim = 11,
      liquidado_3bim = 11,
      liquidado_4bim = 11,
      liquidado_5bim = 11,
      liquidado_6bim = 11,
      pago_1bim = 11,
      pago_2bim = 11,
      pago_3bim = 11,
      pago_4bim = 11,
      pago_5bim = 11,
      pago_6bim = 11,
      complemento_recurso_vinculado = 4
    ),
    col_types = cols(
      orgao = col_character(),
      uniorcam = col_character(),
      funcao = col_character(),
      subfuncao = col_character(),
      programa = col_character(),
      obsoleto1 = col_character(),
      projativ = col_character(),
      rubrica = col_character(),
      recurso_vinculado = col_character(),
      empenhado_1bim = col_number(),
      empenhado_2bim = col_number(),
      empenhado_3bim = col_number(),
      empenhado_4bim = col_number(),
      empenhado_5bim = col_number(),
      empenhado_6bim = col_number(),
      liquidado_1bim = col_number(),
      liquidado_2bim = col_number(),
      liquidado_3bim = col_number(),
      liquidado_4bim = col_number(),
      liquidado_5bim = col_number(),
      liquidado_6bim = col_number(),
      pago_1bim = col_number(),
      pago_2bim = col_number(),
      pago_3bim = col_number(),
      pago_4bim = col_number(),
      pago_5bim = col_number(),
      pago_6bim = col_number(),
      complemento_recurso_vinculado = col_character()
    ),
    skip = 1,
    trim_ws = T,
    progress = T,
    skip_empty_rows = T
  )

  # Remove a linha do FINALIZADOR
  df <- subset(df, df$orgao != 'FI')

  # Converte as colunas de moeda
  df$empenhado_1bim <- round(df$empenhado_1bim / 100, digits = 2)
  df$empenhado_2bim <- round(df$empenhado_2bim / 100, digits = 2)
  df$empenhado_3bim <- round(df$empenhado_3bim / 100, digits = 2)
  df$empenhado_4bim <- round(df$empenhado_4bim / 100, digits = 2)
  df$empenhado_5bim <- round(df$empenhado_5bim / 100, digits = 2)
  df$empenhado_6bim <- round(df$empenhado_6bim / 100, digits = 2)
  df$liquidado_1bim <- round(df$liquidado_1bim / 100, digits = 2)
  df$liquidado_2bim <- round(df$liquidado_2bim / 100, digits = 2)
  df$liquidado_3bim <- round(df$liquidado_3bim / 100, digits = 2)
  df$liquidado_4bim <- round(df$liquidado_4bim / 100, digits = 2)
  df$liquidado_5bim <- round(df$liquidado_5bim / 100, digits = 2)
  df$liquidado_6bim <- round(df$liquidado_6bim / 100, digits = 2)
  df$pago_1bim <- round(df$pago_1bim / 100, digits = 2)
  df$pago_2bim <- round(df$pago_2bim / 100, digits = 2)
  df$pago_3bim <- round(df$pago_3bim / 100, digits = 2)
  df$pago_4bim <- round(df$pago_4bim / 100, digits = 2)
  df$pago_5bim <- round(df$pago_5bim / 100, digits = 2)
  df$pago_6bim <- round(df$pago_6bim / 100, digits = 2)

  # Acrescenta colunas extras

  # Formata campos
  df$rubrica <- gsub('^0{0,}', '', df$rubrica)
  df$rubrica <- str_pad(df$rubrica, 15, c('right'), pad = '0')
  df$rubrica <- sprintf(
    "%s.%s.%s.%s.%s.%s.%s.%s",
    str_sub(df$rubrica, start = 1, end = 1),
    str_sub(df$rubrica, start = 2, end = 2),
    str_sub(df$rubrica, start = 3, end = 4),
    str_sub(df$rubrica, start = 5, end = 6),
    str_sub(df$rubrica, start = 7, end = 8),
    str_sub(df$rubrica, start = 9, end = 10),
    str_sub(df$rubrica, start = 11, end = 12),
    str_sub(df$rubrica, start = 13, end = 15)
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
