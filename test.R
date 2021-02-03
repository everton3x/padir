# Arquivo para testes
devtools::load_all()

dados <- build_pagamento(parse_empenho("C:/Users/Everton/OneDrive/Prefeitura/2020/PAD/2020-12/pm/MES12/EMPENHO.TXT"), parse_pagament("C:/Users/Everton/OneDrive/Prefeitura/2020/PAD/2020-12/pm/MES12/PAGAMENT.TXT"))
# write.csv2(dados, "dados.csv")
