#' @title Converte LIQUIDAC.TXT para um Data Frame.
#' @name parse_liquidac
#'
#' @description Converte os dados gerados para importação pelo SIAPC/PAD do TCE/RS em um Data Frame do R.
#'
#' @param arquivo_txt O caminho para o arquivo LIQUIDAC.TXT
#' @return Um Data Frame com os dados.
#'
#' @author Everton da Rosa
#'
#' @export
#' @import readr
#' @import stringr
parse_liquidac <- function(arquivo_txt){

  # Importa o TXT
  df <- read_fwf(
    arquivo_txt,
    fwf_cols(
      numero_empenho = 13,
      numero_liquidacao = 20,
      data_liquidacao = 8,
      valor_liquidacao = 13,
      sinal_valor = 1,
      obsoleto1 = 165,
      codigo_operacao = 30,
      historico_liquidacao = 400,
      existe_contrato = 1,
      numero_contrato_tce = 20,
      numero_contrato = 20,
      ano_contrato = 4,
      existe_nota_fiscal = 1,
      numero_nota_fiscal = 9,
      serie_nota_fiscal = 3,
      tipo_contrato = 1
    ),
    col_types = cols(
      numero_empenho = col_character(),
      numero_liquidacao = col_character(),
      data_liquidacao = col_character(),
      valor_liquidacao = col_number(),
      sinal_valor = col_character(),
      obsoleto1 = col_character(),
      codigo_operacao = col_character(),
      historico_liquidacao = col_character(),
      existe_contrato = col_character(),
      numero_contrato_tce = col_character(),
      numero_contrato = col_character(),
      ano_contrato = col_character(),
      existe_nota_fiscal = col_character(),
      numero_nota_fiscal = col_character(),
      serie_nota_fiscal = col_character(),
      tipo_contrato = col_character()
    ),
    skip = 1,
    trim_ws = T,
    progress = T,
    skip_empty_rows = T
  )

  # Remove a linha do FINALIZADOR
  df <- subset(df, df$numero_empenho != 'FINALIZADOR00')

  # Converte as colunas de moeda
  df$valor_liquidacao <- round(as.numeric(paste(df$sinal_valor, df$valor_liquidacao, sep = '')) / 100, digits = 2)

  # Acrescenta colunas extras
  df$ano_empenho <- as.character(substr(df$numero_empenho, 1, 5))

  # Formata campos
  df$data_liquidacao <- as.Date(df$data_liquidacao, tryFormats = c("%d%m%Y"))

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
