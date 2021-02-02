#' @title Converte CTA_DISP.TXT para um Data Frame.
#' @name parse_cta_disp
#'
#' @description Converte os dados gerados para importação pelo SIAPC/PAD do TCE/RS em um Data Frame do R.
#'
#' @param arquivo_txt O caminho para o arquivo CTA_DISP.TXT
#' @return Um Data Frame com os dados.
#'
#' @author Everton da Rosa
#'
#' @export
#' @import readr
#' @import stringr
parse_cta_disp <- function(arquivo_txt){

  # Importa o TXT
  df <- read_fwf(
    arquivo_txt,
    fwf_cols(
      conta_contabil = 20,
      orgao = 2,
      uniorcam = 2,
      recurso_vinculado = 4,
      banco = 5,
      agencia = 5,
      conta_corrente = 20,
      tipo_conta = 1,
      classificacao_disponivel = 1,
      complemento_recurso_vinculado = 4
    ),
    col_types = cols(
      conta_contabil = col_character(),
      orgao = col_character(),
      uniorcam = col_character(),
      recurso_vinculado = col_character(),
      banco = col_character(),
      agencia = col_character(),
      conta_corrente = col_character(),
      tipo_conta = col_character(),
      classificacao_disponivel = col_character(),
      complemento_recurso_vinculado = col_character()
    ),
    skip = 1,
    trim_ws = T,
    progress = T,
    skip_empty_rows = T
  )

  # Remove a linha do FINALIZADOR
  df <- subset(df, grepl("^[^FINALIZADOR]", df$conta_contabil))

  # Converte as colunas de moeda

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
