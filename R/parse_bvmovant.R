#' @title Converte BVMOVANT.TXT para um Data Frame.
#' @name parse_bvmovant
#'
#' @description Converte os dados gerados para importação pelo SIAPC/PAD do TCE/RS em um Data Frame do R.
#'
#' @param arquivo_txt O caminho para o arquivo BVMOVANT.TXT
#' @return Um Data Frame com os dados.
#'
#' @author Everton da Rosa
#'
#' @export
#' @import readr
#' @import stringr
parse_bvmovant <- function(arquivo_txt){

  # Importa o TXT
  df <- read_fwf(
    arquivo_txt,
    fwf_cols(
      conta_contabil = 20,
      orgao = 2,
      uniorcam = 2,
      debito_1bim = 13,
      credito_1bim = 13,
      debito_2bim = 13,
      credito_2bim = 13,
      debito_3bim = 13,
      credito_3bim = 13,
      debito_4bim = 13,
      credito_4bim = 13,
      debito_5bim = 13,
      credito_5bim = 13,
      debito_6bim = 13,
      credito_6bim = 13
    ),
    col_types = cols(
      conta_contabil = col_character(),
      orgao = col_character(),
      uniorcam = col_character(),
      debito_1bim = col_number(),
      credito_1bim = col_number(),
      debito_2bim = col_number(),
      credito_2bim = col_number(),
      debito_3bim = col_number(),
      credito_3bim = col_number(),
      debito_4bim = col_number(),
      credito_4bim = col_number(),
      debito_5bim = col_number(),
      credito_5bim = col_number(),
      debito_6bim = col_number(),
      credito_6bim = col_number()
    ),
    skip = 1,
    trim_ws = T,
    progress = T,
    skip_empty_rows = T
  )

  # Remove a linha do FINALIZADOR
  df <- subset(df, grepl("^[^FINALIZADOR]", df$conta_contabil))

  # Converte as colunas de moeda
  df$debito_1bim <- round(as.numeric(df$debito_1bim) / 100, digits = 2)
  df$credito_1bim <- round(as.numeric(df$credito_1bim) / 100, digits = 2)
  df$debito_2bim <- round(as.numeric(df$debito_2bim) / 100, digits = 2)
  df$credito_2bim <- round(as.numeric(df$credito_2bim) / 100, digits = 2)
  df$debito_3bim <- round(as.numeric(df$debito_3bim) / 100, digits = 2)
  df$credito_3bim <- round(as.numeric(df$credito_3bim) / 100, digits = 2)
  df$debito_4bim <- round(as.numeric(df$debito_4bim) / 100, digits = 2)
  df$credito_4bim <- round(as.numeric(df$credito_4bim) / 100, digits = 2)
  df$debito_5bim <- round(as.numeric(df$debito_5bim) / 100, digits = 2)
  df$credito_5bim <- round(as.numeric(df$credito_5bim) / 100, digits = 2)
  df$debito_6bim <- round(as.numeric(df$debito_6bim) / 100, digits = 2)
  df$credito_6bim <- round(as.numeric(df$credito_6bim) / 100, digits = 2)

  # Acrescenta colunas extras

  # Formata campos
  df$conta_contabil <- gsub('^0{0,}', '', df$conta_contabil)
  df$conta_contabil <- str_pad(df$conta_contabil, 20, c('right'), pad = '0')
  df$conta_contabil <- sprintf(
    "%s.%s.%s.%s.%s.%s.%s.%s.%s.%s",
    str_sub(df$conta_contabil, start = 1, end = 1),
    str_sub(df$conta_contabil, start = 2, end = 2),
    str_sub(df$conta_contabil, start = 3, end = 3),
    str_sub(df$conta_contabil, start = 4, end = 4),
    str_sub(df$conta_contabil, start = 5, end = 5),
    str_sub(df$conta_contabil, start = 6, end = 7),
    str_sub(df$conta_contabil, start = 8, end = 9),
    str_sub(df$conta_contabil, start = 10, end = 11),
    str_sub(df$conta_contabil, start = 12, end = 13),
    str_sub(df$conta_contabil, start = 14, end = 15)
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
