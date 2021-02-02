#' @title Converte UNIORCAM.TXT para um Data Frame.
#' @name parse_uniorcam
#'
#' @description Converte os dados gerados para importação pelo SIAPC/PAD do TCE/RS em um Data Frame do R.
#'
#' @param arquivo_txt O caminho para o arquivo UNIORCAM.TXT
#' @return Um Data Frame com os dados.
#'
#' @author Everton da Rosa
#'
#' @export
#' @import readr
#' @import stringr
parse_uniorcam <- function(arquivo_txt){

  # Importa o TXT
  df <- read_fwf(
    arquivo_txt,
    fwf_cols(
      exercicio = 4,
      codigo_orgao = 2,
      codigo_uniorcam = 2,
      nome_uniorcam = 80,
      identificador_uniorcam = 2,
      cnpj_uniorcam = 14
    ),
    col_types = cols(
      exercicio = col_character(),
      codigo_orgao = col_character(),
      codigo_uniorcam = col_character(),
      nome_uniorcam = col_character(),
      identificador_uniorcam = col_character(),
      cnpj_uniorcam = col_character()
    ),
    skip = 1,
    trim_ws = T,
    progress = T,
    skip_empty_rows = T
  )

  # Remove a linha do FINALIZADOR
  df <- subset(df, df$exercicio != 'FINA')

  # Converte as colunas de moeda


  # Acrescenta colunas extras


  # Formata campos
  df$nome_uniorcam <- str_trim(df$nome_orgao)

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
