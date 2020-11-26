#' @title Converte BAL_DESP.TXT para um Data Frame.
#' @name parse_bal_desp
#'
#' @description Converte os dados gerados para importação pelo SIAPC/PAD do TCE/RS em um Data Frame do R.
#'
#' @param arquivo_txt O caminho para o arquivo BAL_DESP.TXT
#' @return Um Data Frame com os dados.
#'
#' @author Everton da Rosa
#'
#' @export
#' @import readr
#' @import stringr
parse_bal_desp <- function(arquivo_txt){

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
      elemento = 6,
      recurso_vinculado = 4,
      dotacao_inicial = 13,
      atualizacao_monetaria = 13,
      creditos_suplementares = 13,
      creditos_especiais = 13,
      creditos_extraordinarios = 13,
      reducao_dotacao = 13,
      suplementacao_recurso_vinculado = 13,
      reducao_recurso_vinculado = 13,
      valor_empenhado = 13,
      valor_liquidado = 13,
      valor_pago = 13,
      valor_limitado = 13,
      valor_recomposto = 13,
      previsao_termino = 13,
      complemento_recurso_vinculado = 4
    ),
    col_types = cols(
      orgao = col_character(),
      uniorcam = col_character(),
      funcao = col_character(),
      subfuncao = col_character(),
      programa = col_character(),
      obsoleto1 = col_character(),
      projativ = col_character(),
      elemento = col_character(),
      recurso_vinculado = col_character(),
      dotacao_inicial = col_number(),
      atualizacao_monetaria = col_number(),
      creditos_suplementares = col_number(),
      creditos_especiais = col_number(),
      creditos_extraordinarios = col_number(),
      reducao_dotacao = col_number(),
      suplementacao_recurso_vinculado = col_number(),
      reducao_recurso_vinculado = col_number(),
      valor_empenhado = col_number(),
      valor_liquidado = col_number(),
      valor_pago = col_number(),
      valor_limitado = col_number(),
      valor_recomposto = col_number(),
      previsao_termino = col_number(),
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
  df$dotacao_inicial <- round(df$dotacao_inicial / 100, digits = 2)
  df$atualizacao_monetaria <- round(df$atualizacao_monetaria / 100, digits = 2)
  df$creditos_suplementares <- round(df$creditos_suplementares / 100, digits = 2)
  df$creditos_especiais <- round(df$creditos_especiais / 100, digits = 2)
  df$creditos_extraordinarios <- round(df$creditos_extraordinarios / 100, digits = 2)
  df$reducao_dotacao <- round(df$reducao_dotacao / 100, digits = 2)
  df$suplementacao_recurso_vinculado <- round(df$suplementacao_recurso_vinculado / 100, digits = 2)
  df$reducao_recurso_vinculado <- round(df$reducao_recurso_vinculado / 100, digits = 2)
  df$valor_empenhado <- round(df$valor_empenhado / 100, digits = 2)
  df$valor_liquidado <- round(df$valor_liquidado / 100, digits = 2)
  df$valor_pago <- round(df$valor_pago / 100, digits = 2)
  df$valor_limitado <- round(df$valor_limitado / 100, digits = 2)
  df$valor_recomposto <- round(df$valor_recomposto / 100, digits = 2)
  df$previsao_termino <- round(df$previsao_termino / 100, digits = 2)

  # Acrescenta colunas extras
  df$dotacao_atualizada <- df$dotacao_inicial + df$atualizacao_monetaria + df$creditos_suplementares + df$creditos_especiais + df$creditos_extraordinarios - df$reducao_dotacao + df$suplementacao_recurso_vinculado - df$reducao_recurso_vinculado

  # Formata campos
  df$elemento <- sprintf(
    "%s.%s.%s.%s",
    str_sub(df$elemento, start = 1, end = 1),
    str_sub(df$elemento, start = 2, end = 2),
    str_sub(df$elemento, start = 3, end = 4),
    str_sub(df$elemento, start = 5, end = 6)
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
