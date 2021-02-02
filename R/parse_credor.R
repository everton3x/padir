#' @title Converte CREDOR.TXT para um Data Frame.
#' @name parse_credor
#'
#' @description Converte os dados gerados para importação pelo SIAPC/PAD do TCE/RS em um Data Frame do R.
#'
#' @param arquivo_txt O caminho para o arquivo CREDOR.TXT
#' @return Um Data Frame com os dados.
#'
#' @author Everton da Rosa
#'
#' @export
#' @import readr
#' @import stringr
parse_credor <- function(arquivo_txt){

  # Importa o TXT
  df <- read_fwf(
    arquivo_txt,
    fwf_cols(
      codigo_credor = 10,
      nome_credor = 60,
      cnpj_cpf = 14,
      inscricao_estadual = 15,
      inscricao_municipal = 15,
      endereco = 50,
      cidade = 30,
      uf = 2,
      cep = 8,
      fone = 15,
      fax = 15,
      tipo_credor = 2,
      tipo_pessoa = 2
    ),
    col_types = cols(
      codigo_credor = col_character(),
      nome_credor = col_character(),
      cnpj_cpf = col_character(),
      inscricao_estadual = col_character(),
      inscricao_municipal = col_character(),
      endereco = col_character(),
      cidade = col_character(),
      uf = col_character(),
      cep = col_character(),
      fone = col_character(),
      fax = col_character(),
      tipo_credor = col_character(),
      tipo_pessoa = col_character()
    ),
    skip = 1,
    trim_ws = T,
    progress = T,
    skip_empty_rows = T
  )

  # Remove a linha do FINALIZADOR
  df <- subset(df, grepl("^[^FINALIZADOR]", df$codigo_credor))

  # Converte as colunas de moeda


  # Acrescenta colunas extras


  # Formata campos
  df$nome_credor <- str_trim(df$nome_credor)
  df$endereco <- str_trim(df$endereco)
  df$cidade <- str_trim(df$cidade)

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
