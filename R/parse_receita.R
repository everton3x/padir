#' @title Converte RECEITA.TXT para um Data Frame.
#' @name parse_receita
#'
#' @description Converte os dados gerados para importação pelo SIAPC/PAD do TCE/RS em um Data Frame do R.
#'
#' @param arquivo_txt O caminho para o arquivo RECEITA.TXT
#' @return Um Data Frame com os dados.
#'
#' @author Everton da Rosa
#'
#' @export
#' @import readr
#' @import stringr
parse_receita <- function(arquivo_txt){

  # Importa o TXT
  df <- read_fwf(
    arquivo_txt,
    fwf_cols(
      codigo_receita = 20,
      orgao = 2,
      uniorcam = 2,
      realizada_janeiro = 13,
      realizada_fevereiro = 13,
      realizada_marco = 13,
      realizada_abril = 13,
      realizada_maio = 13,
      realizada_junho = 13,
      realizada_julho = 13,
      realizada_agosto = 13,
      realizada_setembro = 13,
      realizada_outubro = 13,
      realizada_novembro = 13,
      realizada_dezembro = 13,
      meta_1bim = 12,
      meta_2bim = 12,
      meta_3bim = 12,
      meta_4bim = 12,
      meta_5bim = 12,
      meta_6bim = 12,
      caracteristica_peculiar_receita = 3,
      recurso_vinculado = 4,
      complemento_recurso_vinculado = 4
    ),
    col_types = cols(
      codigo_receita = col_character(),
      orgao = col_character(),
      uniorcam = col_character(),
      realizada_janeiro = col_number(),
      realizada_fevereiro = col_number(),
      realizada_marco = col_number(),
      realizada_abril = col_number(),
      realizada_maio = col_number(),
      realizada_junho = col_number(),
      realizada_julho = col_number(),
      realizada_agosto = col_number(),
      realizada_setembro = col_number(),
      realizada_outubro = col_number(),
      realizada_novembro = col_number(),
      realizada_dezembro = col_number(),
      meta_1bim = col_number(),
      meta_2bim = col_number(),
      meta_3bim = col_number(),
      meta_4bim = col_number(),
      meta_5bim = col_number(),
      meta_6bim = col_number(),
      caracteristica_peculiar_receita = col_character(),
      recurso_vinculado = col_character(),
      complemento_recurso_vinculado = col_character()
    ),
    skip = 1,
    trim_ws = T,
    progress = T,
    skip_empty_rows = T
  )

  # Remove a linha do FINALIZADOR
  df <- subset(df, grepl("^[^FINALIZADOR]", df$codigo_receita))

  # Converte as colunas de moeda
  df$realizada_janeiro <- round(as.numeric(df$realizada_janeiro) / 100, digits = 2)
  df$realizada_fevereiro <- round(as.numeric(df$realizada_fevereiro) / 100, digits = 2)
  df$realizada_marco <- round(as.numeric(df$realizada_marco) / 100, digits = 2)
  df$realizada_abril <- round(as.numeric(df$realizada_abril) / 100, digits = 2)
  df$realizada_maio <- round(as.numeric(df$realizada_maio) / 100, digits = 2)
  df$realizada_junho <- round(as.numeric(df$realizada_junho) / 100, digits = 2)
  df$realizada_julho <- round(as.numeric(df$realizada_julho) / 100, digits = 2)
  df$realizada_agosto <- round(as.numeric(df$realizada_agosto) / 100, digits = 2)
  df$realizada_setembro <- round(as.numeric(df$realizada_setembro) / 100, digits = 2)
  df$realizada_outubro <- round(as.numeric(df$realizada_outubro) / 100, digits = 2)
  df$realizada_novembro <- round(as.numeric(df$realizada_novembro) / 100, digits = 2)
  df$realizada_dezembro <- round(as.numeric(df$realizada_dezembro) / 100, digits = 2)

  df$meta_1bim <- round(as.numeric(df$meta_1bim) / 100, digits = 2)
  df$meta_2bim <- round(as.numeric(df$meta_2bim) / 100, digits = 2)
  df$meta_3bim <- round(as.numeric(df$meta_3bim) / 100, digits = 2)
  df$meta_4bim <- round(as.numeric(df$meta_4bim) / 100, digits = 2)
  df$meta_5bim <- round(as.numeric(df$meta_5bim) / 100, digits = 2)
  df$meta_6bim <- round(as.numeric(df$meta_6bim) / 100, digits = 2)

  # Acrescenta colunas extras

  # Formata campos

  df$codigo_receita <- gsub('^0{0,}', '', df$codigo_receita)
  df$codigo_receita <- str_pad(df$codigo_receita, 20, c('right'), pad = '0')
  eh_deducao <- startsWith(df$codigo_receita, "9")
  df <- cbind(df, eh_deducao)
  df$codigo_receita <- ifelse(df$eh_deducao == TRUE, sprintf(
    "%s.%s.%s.%s.%s.%s.%s.%s.%s.%s.%s",
    str_sub(df$codigo_receita, start = 1, end = 1),
    str_sub(df$codigo_receita, start = 2, end = 2),
    str_sub(df$codigo_receita, start = 3, end = 3),
    str_sub(df$codigo_receita, start = 4, end = 4),
    str_sub(df$codigo_receita, start = 5, end = 5),
    str_sub(df$codigo_receita, start = 6, end = 7),
    str_sub(df$codigo_receita, start = 8, end = 8),
    str_sub(df$codigo_receita, start = 9, end = 9),
    str_sub(df$codigo_receita, start = 10, end = 11),
    str_sub(df$codigo_receita, start = 12, end = 13),
    str_sub(df$codigo_receita, start = 14, end = 15)
  ), sprintf(
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
  ))
  df$eh_deducao <- NULL

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
