#' @title Converte EMPENHO.TXT para um Data Frame.
#' @name parse_empenho
#'
#' @description Converte os dados gerados para importação pelo SIAPC/PAD do TCE/RS em um Data Frame do R.
#'
#' @param arquivo_txt O caminho para o arquivo EMPENHO.TXT
#' @return Um Data Frame com os dados.
#'
#' @author Everton da Rosa
#'
#' @export
#' @import readr
#' @import stringr
parse_empenho <- function(arquivo_txt){

  # Importa o TXT
  df <- read_fwf(
    arquivo_txt,
    fwf_cols(
      orgao = 2,
      uniorcam = 2,
      funcao = 2,
      subfuncao = 3,
      programa = 4,
      obsoleto1 = 3,
      projativ = 5,
      rubrica = 15,
      recurso_vinculado = 4,
      contrapartida_recurso_vinculado = 4,
      numero_empenho = 13,
      data_empenho = 8,
      valor_empenho = 13,
      sinal_valor = 1,
      credor = 10,
      obsoleto2 = 165,
      caracteristica_peculiar = 3,
      obsoleto3 = 2,
      registro_precos = 1,
      obsoleto4 = 20,
      numero_licitacao = 20,
      ano_licitacao = 4,
      historico_empenho = 400,
      forma_contratacao = 3,
      base_legal = 2,
      despesa_funcionario = 1,
      licitacao_compartilhada = 1,
      cnpj_orgao_gerenciador = 14,
      complemento_recurso_vinculado = 4
    ),
    col_types = cols(
      orgao = col_character(),
      uniorcam = col_character(),
      funcao = col_character(),
      subfuncao = col_character(),
      programa = col_character(),
      obsoleto1 = col_number(),
      projativ = col_character(),
      rubrica = col_character(),
      recurso_vinculado = col_character(),
      contrapartida_recurso_vinculado = col_character(),
      numero_empenho = col_character(),
      data_empenho = col_character(),
      valor_empenho = col_number(),
      sinal_valor = col_character(),
      credor = col_number(),
      obsoleto2 = col_character(),
      caracteristica_peculiar = col_character(),
      obsoleto3 = col_number(),
      registro_precos = col_character(),
      obsoleto4 = col_character(),
      numero_licitacao = col_character(),
      ano_licitacao = col_character(),
      historico_empenho = col_character(),
      forma_contratacao = col_character(),
      base_legal = col_character(),
      despesa_funcionario = col_character(),
      licitacao_compartilhada = col_character(),
      cnpj_orgao_gerenciador = col_character(),
      complemento_recurso_vinculado = col_character()
    ),
    skip = 1,
    trim_ws = T,
    progress = T,
    skip_empty_rows = T
  )

  # Remove a linha do FINALIZADOR
  df <- subset(df, df$orgao != 'FI')

  # Converte as colunas de moeda
  df$valor_empenho <- round(as.numeric(paste(df$sinal_valor, df$valor_empenho, sep = '')) / 100, digits = 2)

  # Acrescenta colunas extras
  df$ano_empenho <- as.character(substr(df$numero_empenho, 1, 5))

  # Formata campos
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

  df$data_empenho <- as.Date(df$data_empenho, tryFormats = c("%d%m%Y"))

  df$historico_empenho <- str_trim(df$historico_empenho)

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
