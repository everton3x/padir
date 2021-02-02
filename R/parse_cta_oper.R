#' @title Converte CTA_OPER.TXT para um Data Frame.
#' @name parse_cta_oper
#'
#' @description Converte os dados gerados para importação pelo SIAPC/PAD do TCE/RS em um Data Frame do R.
#'
#' @param arquivo_txt O caminho para o arquivo CTA_OPER.TXT
#' @return Um Data Frame com os dados.
#'
#' @author Everton da Rosa
#'
#' @export
#' @import readr
#' @import stringr
parse_cta_oper <- function(arquivo_txt){

  # Importa o TXT
  df <- read_fwf(
    arquivo_txt,
    fwf_cols(
      codigo_operacao = 30,
      data_operacao = 8,
      valor_operacao = 13,
      sinal_valor = 1,
      recurso_vinculado = 4,
      codigo_receita = 20,
      orgao_receita = 2,
      uniorcam_receita = 2,
      conta_contabil = 20,
      orgao_contabil = 2,
      uniorcam_contabil = 2,
      complemento_recurso_vinculado = 4
    ),
    col_types = cols(
      codigo_operacao = col_character(),
      data_operacao = col_character(),
      valor_operacao = col_number(),
      sinal_valor = col_character(),
      recurso_vinculado = col_character(),
      codigo_receita = col_character(),
      orgao_receita = col_character(),
      uniorcam_receita = col_character(),
      conta_contabil = col_character(),
      orgao_contabil = col_character(),
      uniorcam_contabil = col_character(),
      complemento_recurso_vinculado = col_character()
    ),
    skip = 1,
    trim_ws = T,
    progress = T,
    skip_empty_rows = T
  )

  # Remove a linha do FINALIZADOR
  df <- subset(df, grepl("^[^FINALIZADOR]", df$codigo_operacao))

  # Converte as colunas de moeda
  df$valor_operacao <- round(as.numeric(paste(df$sinal_valor, df$valor_operacao, sep = '')) / 100, digits = 2)

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

  df$codigo_receita <- gsub('^0{0,}', '', df$codigo_receita)
  df$codigo_receita <- gsub('^9{0,1}', '', df$codigo_receita)
  df$codigo_receita <- str_pad(df$codigo_receita, 20, c('right'), pad = '0')
  df$codigo_receita <- sprintf(
    "%s.%s.%s.%s.%s.%s.%s.%s.%s.%s",
    str_sub(df$codigo_receita, start = 1, end = 1),
    str_sub(df$codigo_receita, start = 2, end = 2),
    str_sub(df$codigo_receita, start = 3, end = 3),
    str_sub(df$codigo_receita, start = 4, end = 4),
    str_sub(df$codigo_receita, start = 5, end = 6),
    str_sub(df$codigo_receita, start = 7, end = 7),
    str_sub(df$codigo_receita, start = 8, end = 8),
    str_sub(df$codigo_receita, start = 9, end = 10),
    str_sub(df$codigo_receita, start = 11, end = 12),
    str_sub(df$codigo_receita, start = 13, end = 14)
  )

  df$data_operacao <- as.Date(df$data_operacao, tryFormats = c("%d%m%Y"))

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
