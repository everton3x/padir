#' @title Converte RECURSO.TXT para um Data Frame.
#' @name parse_recurso
#'
#' @description Converte os dados gerados para importação pelo SIAPC/PAD do TCE/RS em um Data Frame do R.
#'
#' @param arquivo_txt O caminho para o arquivo RECURSO.TXT
#' @return Um Data Frame com os dados.
#'
#' @author Everton da Rosa
#'
#' @export
#' @import readr
#' @import stringr
parse_recurso <- function(arquivo_txt){

  # Importa o TXT
  df <- read_fwf(
    arquivo_txt,
    fwf_cols(
      codigo_recurso_vinculado = 4,
      nome_recurso_vinculado = 80,
      finalidade_recurso_vinculado = 160
    ),
    col_types = cols(
      codigo_recurso_vinculado = col_character(),
      nome_recurso_vinculado = col_character(),
      finalidade_recurso_vinculado = col_character()
    ),
    skip = 1,
    trim_ws = T,
    progress = T,
    skip_empty_rows = T
  )

  # Remove a linha do FINALIZADOR
  df <- subset(df, df$codigo_recurso_vinculado != 'FINA')

  # Converte as colunas de moeda


  # Acrescenta colunas extras


  # Formata campos
  df$nome_recurso_vinculado <- str_trim(df$nome_recurso_vinculado)
  df$finalidade_recurso_vinculado <- str_trim(df$finalidade_recurso_vinculado)

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
