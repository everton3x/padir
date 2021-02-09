# Arquivo para testes
devtools::load_all()

dados <- build_rp(
    parse_empenho("C:/Users/Everton/OneDrive/Prefeitura/2021/PAD/2021-01/pm/MES01/EMPENHO.TXT"),
    parse_liquidac("C:/Users/Everton/OneDrive/Prefeitura/2021/PAD/2021-01/pm/MES01/LIQUIDAC.TXT"),
    parse_pagament("C:/Users/Everton/OneDrive/Prefeitura/2021/PAD/2021-01/pm/MES01/PAGAMENT.TXT")
  )
write.csv2(dados, "dados.csv")
