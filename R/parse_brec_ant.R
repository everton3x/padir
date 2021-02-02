#' @title Converte BREC_ANT.TXT para um Data Frame.
#' @name parse_brec_ant
#'
#' @description Converte os dados gerados para importação pelo SIAPC/PAD do TCE/RS em um Data Frame do R.
#'
#' @param arquivo_txt O caminho para o arquivo BREC_ANT.TXT
#' @return Um Data Frame com os dados.
#'
#' @author Everton da Rosa
#'
#' @export
#' @import readr
#' @import stringr
parse_brec_ant <- function(arquivo_txt){

  # Importa o TXT
  df <- read_fwf(
    arquivo_txt,
    fwf_cols(
      codigo_receita = 20,
      orgao = 2,
      uniorcam = 2,
      receita_orcada = 13,
      receita_realizada = 13,
      recurso_vinculado = 4,
      especificacao_receita = 170,
      tipo_nivel = 1,
      nivel = 2,
      caracteristica_peculiar_receita = 3,
      complemento_recurso_vinculado = 4
    ),
    col_types = cols(
      codigo_receita = col_character(),
      orgao = col_character(),
      uniorcam = col_character(),
      receita_orcada = col_number(),
      receita_realizada = col_number(),
      recurso_vinculado = col_character(),
      especificacao_receita = col_character(),
      tipo_nivel = col_character(),
      nivel = col_character(),
      caracteristica_peculiar_receita = col_character(),
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
  df$receita_orcada <- round(as.numeric(df$receita_orcada) / 100, digits = 2)
  df$receita_realizada <- round(as.numeric(df$receita_realizada) / 100, digits = 2)

  # Acrescenta colunas extras

  # Formata campos
  df$especificacao_receita <- str_trim(df$especificacao_receita)

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
