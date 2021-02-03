# Arquivo para testes
devtools::load_all()
# dados <- parse_tce_4111("C:/Users/Everton/OneDrive/Prefeitura/2020/PAD/2020-12/pm/MES12/tce_4111.TXT")
# dados
# write.csv2(dados, 'dados.csv')

dados <- build_moviemp(parse_empenho("C:/Users/Everton/OneDrive/Prefeitura/2020/PAD/2020-12/pm/MES12/EMPENHO.TXT"), parse_liquidac("C:/Users/Everton/OneDrive/Prefeitura/2020/PAD/2020-12/pm/MES12/LIQUIDAC.TXT"), parse_pagament("C:/Users/Everton/OneDrive/Prefeitura/2020/PAD/2020-12/pm/MES12/PAGAMENT.TXT"))
write.csv2(dados, "dados.csv")
