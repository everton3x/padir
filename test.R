# Arquivo para testes
devtools::load_all()
# bal_desp <- parse_bal_desp("C:/Users/Everton/OneDrive/Prefeitura/2020/PAD/2020-12/pm/MES12/BAL_DESP.TXT")
# bal_desp

# empenho <- parse_empenho("C:/Users/Everton/OneDrive/Prefeitura/2020/PAD/2020-12/pm/MES12/EMPENHO.TXT")
# empenho
# write.csv2(empenho, 'empenho.csv')

# liquidac <- parse_liquidac("C:/Users/Everton/OneDrive/Prefeitura/2020/PAD/2020-12/pm/MES12/LIQUIDAC.TXT")
# liquidac
# write.csv2(liquidac, 'liquidac.csv')

pagament <- parse_pagament("C:/Users/Everton/OneDrive/Prefeitura/2020/PAD/2020-12/pm/MES12/PAGAMENT.TXT")
pagament
write.csv2(pagament, 'pagament.csv')

