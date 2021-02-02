#' @title Converte RUBRICA.TXT para um Data Frame.
#' @name parse_rubrica
#'
#' @description Converte os dados gerados para importação pelo SIAPC/PAD do TCE/RS em um Data Frame do R.
#'
#' @param arquivo_txt O caminho para o arquivo RUBRICA.TXT
#' @return Um Data Frame com os dados.
#'
#' @author Everton da Rosa
#'
#' @export
#' @import readr
#' @import stringr
parse_rubrica <- function(arquivo_txt){

  # Importa o TXT
  df <- read_fwf(
    arquivo_txt,
    fwf_cols(
      exercicio = 4,
      rubrica = 15,
      especificacao_rubrica = 110,
      tipo_nivel = 1,
      nivel = 2
    ),
    col_types = cols(
      exercicio = col_character(),
      rubrica = col_character(),
      especificacao_rubrica = col_character(),
      tipo_nivel = col_character(),
      nivel = col_character()
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
  df$especificacao_rubrica <- str_trim(especificacao_rubrica)

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
