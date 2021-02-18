#' @title Converte DECRETO.TXT para um Data Frame.
#' @name parse_decreto
#'
#' @description Converte os dados gerados para importação pelo SIAPC/PAD do TCE/RS em um Data Frame do R.
#'
#' @param arquivo_txt O caminho para o arquivo DECRETO.TXT
#' @return Um Data Frame com os dados.
#'
#' @author Everton da Rosa
#'
#' @export
#' @import readr
#' @import stringr
parse_decreto <- function(arquivo_txt){

  # Importa o TXT
  df <- read_fwf(
    arquivo_txt,
    fwf_cols(
      numero_lei = 20,
      data_lei = 8,
      numero_decreto = 20,
      data_decreto = 8,
      valor_adicional = 13,
      valor_reducao = 13,
      tipo_adicional = 1,
      origem_recurso = 1,
      alteracao_orcamentaria = 1,
      valor_alteracao_orcamentaria = 13,
      data_reabertura_credito_adicional = 8,
      valor_saldo_reaberto = 13
    ),
    col_types = cols(
      numero_lei = col_character(),
      data_lei = col_character(),
      numero_decreto = col_character(),
      data_decreto = col_character(),
      valor_adicional = col_number(),
      valor_reducao = col_number(),
      tipo_adicional = col_character(),
      origem_recurso = col_character(),
      alteracao_orcamentaria = col_character(),
      valor_alteracao_orcamentaria = col_number(),
      data_reabertura_credito_adicional = col_character(),
      valor_saldo_reaberto = col_number()
    ),
    skip = 1,
    trim_ws = T,
    progress = T,
    skip_empty_rows = T
  )

  # Remove a linha do FINALIZADOR
  df <- subset(df, grepl("^[^FINALIZADOR]", df$numero_lei))

  # Converte as colunas de moeda
  df$valor_adicional <- round(as.numeric(df$valor_adicional) / 100, digits = 2)
  df$valor_reducao <- round(as.numeric(df$valor_reducao) / 100, digits = 2)
  df$valor_alteracao_orcamentaria <- round(as.numeric(df$valor_alteracao_orcamentaria) / 100, digits = 2)
  df$valor_saldo_reaberto <- round(as.numeric(df$valor_saldo_reaberto) / 100, digits = 2)

  # Acrescenta colunas extras

  # Formata campos
  df$data_lei <- as.Date(df$data_lei, tryFormats = c("%d%m%Y"))
  df$data_decreto <- as.Date(df$data_decreto, tryFormats = c("%d%m%Y"))
  df$data_reabertura_credito_adicional <- as.Date(df$data_reabertura_credito_adicional, tryFormats = c("%d%m%Y"), optional = T)

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
